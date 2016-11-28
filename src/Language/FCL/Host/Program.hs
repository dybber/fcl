module Language.FCL.Host.Program where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)

import CGen
import CGen.OpenCL.HostCode
import Language.FCL.ILKernel
import Language.FCL.Host.Syntax

type ILHost a = CGen CompileHostState a

data Kernel =
  Kernel
    [VarName] -- implicit parameters, bound in host code (inputs)
    [CType]   -- explicit parameters, given at call site (outputs)
    TopLevel  -- kernel declaration
    ClKernel  -- kernel handle in hostcode

data Value =
    VInt CExp
  | VBool CExp
  | VString CExp
  | VBufPtr CExp ClDeviceBuffer

unInt :: Value -> CExp
unInt (VInt i) = i
unInt _ = error ("Unexpected value, expecting integer.")

type KernelMap = Map Name Kernel

data CompileHostState =
  CompileHostState
    { kernels :: KernelMap
    , allocations :: Map VarName (CExp, ClDeviceBuffer)
    }

initHostState :: CompileHostState
initHostState =
  CompileHostState
    { kernels = Map.empty
    , allocations = Map.empty
    }

addKernel :: Name -> Kernel -> ILHost ()
addKernel name k =
  modifyState (\s -> s { kernels = Map.insert name k (kernels s) })

allocate :: ClContext -> Name -> CType -> Value -> ILHost ()
allocate ctx name ty n =
  do buf <- allocDevice ctx ReadWrite (unInt n) ty
     modifyState (\s -> s { allocations = Map.insert (name, pointer_t [] ty) (unInt n, buf) (allocations s) })

-- Find all free variables:
---- free variables occuring in host-code are named inputs
---- free variables not in host-code are outputs, which
-----  can first be resolved at call sites
mkKernel :: ClProgram -> Set VarName -> Name -> ([VarName], ILKernel ()) -> ILHost ()
mkKernel p env name (explicit_params, body) =
  do (stmts, _, _) <- embed body initializeState
     let sharedMemVar = ("sdata", pointer_t [] int32_t)
         free = freeVars stmts `Set.difference` (Set.fromList [("CLK_LOCAL_MEM_FENCE", int32_t), sharedMemVar])
         implicit_params = Set.difference free (Set.fromList explicit_params)
         params = sharedMemVar : Set.toList implicit_params ++ explicit_params
         fndef = kernel name params stmts
         unknownFree = Set.difference implicit_params env
     kernelHandle <- createKernel p name
     if Prelude.not (Set.null unknownFree)
       then error ("Free variables in kernel that aren't bound in host code" ++ show unknownFree)
       else addKernel name (Kernel (Set.toList implicit_params)
                                   (map snd explicit_params)
                                   fndef
                                   kernelHandle)


createArgument :: VarEnv -> VarName -> ILHost KernelArg
createArgument env (x, ty) =
  do v <- lookupVar (x,ty) env
     case v of
       Just (VBufPtr size buf) -> return (ArgBuffer ty size buf)
       Just (VInt i) -> return (ArgScalar ty i)
       Just (VBool b) -> return (ArgScalar ty b)
       Just _ -> error "Strings can not be passed as arguments to kernels"
       Nothing -> error "variable not found"


callKernel :: ClContext -> VarEnv -> Kernel -> Value -> [VarName] -> ILHost ()
callKernel ctx env (Kernel implicit _ _ kernelHandle) n explicit =
  do -- set shared memoryp
     let smarg = ArgSharedMemory (constant (2048 :: Int))
     -- set input parameters: map over kernel list, lookup allocations in environment
     implicitArgs <- mapM (createArgument env) implicit
     explicitArgs <- mapM (createArgument env) explicit
     -- set output parameters: map over es, lookup allocations in environment
     invokeKernel ctx kernelHandle (smarg : implicitArgs ++ explicitArgs)
                                   (unInt n)
                                   (constant (256 :: Int))


lookupVar :: VarName -> VarEnv -> ILHost (Maybe Value)
lookupVar x@(_, ty) env =
  case ty of
    CPtr [] _ -> do allocs <- getsState allocations
                    case Map.lookup x allocs of
                      Just (size, bufptr) -> return (Just (VBufPtr size bufptr))
                      Nothing -> return Nothing
    _ -> return (Map.lookup x env)

type VarEnv = Map.Map VarName Value

compExp :: VarEnv -> HostExp -> ILHost Value
compExp _ (EInt i) = return (VInt (constant i))
compExp _ (EBool b) = return (VBool (constant b))
compExp _ (EString str) = return (VString (string str))
compExp env (EVar x) =
  do v' <- lookupVar x env
     case v' of
       Just v -> return v
       Nothing -> error ("Undefined variable " ++ show x)

comp :: ClContext -> ClProgram -> HostProgram -> ILHost ()
comp ctx p program =
  let
    go :: VarEnv -> HostProgram -> ILHost ()
    go _ [] = return ()
    go env (Alloc (x,_) ty e : ss) =
      do v <- compExp env e
         allocate ctx x ty v
         go env ss
    go env (DefKernel x k : ss) =
      do allocs <- getsState allocations
         mkKernel p (Set.fromList (Map.keys allocs ++ Map.keys env)) x k
         go env ss
    go env ((Declare (x,ty) e) : ss) =
      do v <- compExp env e
         case v of
           VInt i -> do v' <- let_ x ty i
                        go (Map.insert (x,ty) (VInt v') env) ss
           _ -> go (Map.insert (x,ty) v env) ss
    go env (Call name e es : ss) =
      do n <- compExp env e
         kernelMap <- getsState kernels
         case (Map.lookup name kernelMap) of
           Just k ->
             do callKernel ctx env k n es 
                go env ss
           Nothing -> error "Call to unknown kernel"
  in go Map.empty program


-- Generate Host-code
compileHost :: HostProgram -> ILHost ()
compileHost program =
  do ctx <- initialize 2
     p <- buildProgram ctx "kernels.cl"
     comp ctx p program
     kernelMap <- getsState kernels
     mapM_ releaseKernel (Map.map (\(Kernel _ _ _ handle) -> handle) kernelMap)
     releaseProgram p
     releaseContext ctx

prettyKernels :: KernelMap -> String
prettyKernels kernelMap = pretty (map (\(Kernel _ _ s _) -> s) (Map.elems kernelMap))

createMain :: Statements -> String
createMain body = pretty (includes ++ [function [] "main" body])

includes = [includeSys "stdio.h",
            includeSys "sys/time.h",
            includeSys "mcl.h",
            includeSys "fcl.h"]
                  
compile :: HostProgram -> (String, String)
compile program =
 let (mainBody, _, _, finalState) =
       runCGen initHostState (compileHost program)
 in (createMain mainBody,
     prettyKernels (kernels finalState))

compileAndOutput :: HostProgram -> FilePath -> IO ()
compileAndOutput program path =
  let (main, kernelsfile) = compile program
      mainPath = path ++ "/main.c"
      kernelPath = path ++ "/kernels.cl"
  in do writeFile mainPath main
        writeFile kernelPath kernelsfile
