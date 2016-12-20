module Language.FCL.Host.Program where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)

import CGen
import CGen.OpenCL.HostCode

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
  | VBufPtr VarName CType ClDeviceBuffer

unInt :: Value -> CExp
unInt (VInt i) = i
unInt _ = error ("Unexpected value, expecting integer.")

unString :: Value -> CExp
unString (VString str) = str
unString _ = error ("Unexpected value, expecting integer.")


type KernelMap = Map Name Kernel

data CompileHostState =
  CompileHostState
    { kernels :: KernelMap
    , deviceAllocations :: Map ClDeviceBuffer (CExp, CType)
    , hostAllocations :: Set VarName
    }

initHostState :: CompileHostState
initHostState =
  CompileHostState
    { kernels = Map.empty
    , deviceAllocations = Map.empty
    , hostAllocations = Set.empty
    }

addKernel :: Name -> Kernel -> ILHost ()
addKernel name k =
  modifyState (\s -> s { kernels = Map.insert name k (kernels s) })

allocate :: ClContext -> CType -> Value -> ILHost Value
allocate ctx elemty size =
  do buf <- allocDevice ctx ReadWrite (unInt size) elemty
     modifyState (\s -> s { deviceAllocations = Map.insert buf (unInt size, elemty) (deviceAllocations s) })
     sizeVar <- letVar "size" int32_t (unInt size)
     return (VBufPtr sizeVar elemty buf)

readCSVFile :: CType -> Value -> ILHost (VarName, VarName)
readCSVFile CInt32 filename =
 do  valuesRead <- letVar "valuesRead" int32_t (constant (0 :: Int))
     hostPtr <- eval "csvinput"
                      (pointer_t [] int32_t)
                      "readIntVecFile"
                      [addressOf (var valuesRead), unString filename]
     modifyState (\s -> s { hostAllocations = Set.insert hostPtr (hostAllocations s) })
     return (valuesRead, hostPtr)
readCSVFile CDouble filename =
 do  valuesRead <- letVar "valuesRead" int32_t (constant (0 :: Int))
     hostPtr <- eval "csvinput"
                     (pointer_t [] double_t)
                     "readDoubleVecFile"
                     [addressOf (var valuesRead), unString filename]
     modifyState (\s -> s { hostAllocations = Set.insert hostPtr (hostAllocations s) })
     return (valuesRead, hostPtr)
readCSVFile _ _ = error "Can only read CSV files of doubles or integers"
  
copyToDevice :: ClContext -> CType -> CExp -> CExp -> ILHost Value
copyToDevice ctx elemty n hostptr =
  do buf <- dataToDevice ctx ReadWrite n elemty hostptr
     modifyState (\s -> s { deviceAllocations = Map.insert buf (n, elemty) (deviceAllocations s) })
     size <- letVar "size" int32_t n
     return (VBufPtr size elemty buf)

mkKernel :: ClProgram -> Set VarName -> Name -> ([VarName], Statements) -> ILHost ()
mkKernel p env name (explicit_params, stmts) =
  do let sharedMemVar = ("sdata", pointer_t [attrLocal] int32_t)

         -- free variables occuring in host-code should be implicitly given as arguments
         free = freeVars stmts `Set.difference` (Set.fromList [("CLK_LOCAL_MEM_FENCE", int32_t), sharedMemVar])
         implicit_params = Set.difference free (Set.fromList explicit_params)
         
         -- free variables NOT occuring in host code are passed at call sites
         params = sharedMemVar : Set.toList implicit_params ++ explicit_params
         fndef = kernel name params stmts
         
         -- Err if there any free variables not bound in host code.
         unknownFree = Set.difference implicit_params env
     if Prelude.not (Set.null unknownFree)
       then error ("Free variables in kernel that aren't bound in host code" ++ show unknownFree)
       else do kernelHandle <- createKernel p name
               addKernel name (Kernel (Set.toList implicit_params)
                              (map snd explicit_params)
                              fndef
                              kernelHandle)

createArgument :: VarEnv -> VarName -> ILHost KernelArg
createArgument env (x, ty) =
  do case Map.lookup (x,ty) env of
       Just (VBufPtr _ _ buf) -> return (ArgBuffer buf)
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
     -- set output parameters: map over es, lookup allocations in environment
     explicitArgs <- mapM (createArgument env) explicit
     invokeKernel ctx kernelHandle (smarg : implicitArgs ++ explicitArgs)
                                   (unInt n)
                                   (constant (256 :: Int))

type VarEnv = Map.Map VarName Value

compExp :: ClContext -> VarEnv -> HostExp -> ILHost Value
compExp _ _ (EInt i) = return (VInt (constant i))
compExp _ _ (EBool b) = return (VBool (constant b))
compExp _ _ (EString str) = return (VString (string str))
compExp _ env (EVar x) =
  do case Map.lookup x env of
       Just v -> return v
       Nothing -> error ("Undefined variable " ++ show x)
compExp ctx env (EAlloc elemty e) =
  do size <- compExp ctx env e
     allocate ctx elemty size
compExp ctx env (EReadIntCSV e) =
  do filename <- compExp ctx env e
     (valuesRead, hostPtr) <- readCSVFile int32_t filename
     copyToDevice ctx int32_t (var valuesRead) (var hostPtr)
compExp ctx env (EReadDoubleCSV e) =
  do filename <- compExp ctx env e
     (valuesRead, hostPtr) <- readCSVFile double_t filename
     copyToDevice ctx double_t (var valuesRead) (var hostPtr)
compExp ctx env (ELength e) =
  do arr <- compExp ctx env e
     case arr of
       (VBufPtr size _ _) -> return (VInt (var size))
       _ -> error ""
compExp ctx env (EBinOp op e1 e2) =
  do v1 <- compExp ctx env e1
     v2 <- compExp ctx env e2
     return (binop op v1 v2)

binop :: BinOp -> Value -> Value -> Value
binop AddI (VInt i1) (VInt i2) = VInt (addi i1 i2)
binop SubI (VInt i1) (VInt i2) = VInt (subi i1 i2)
binop MulI (VInt i1) (VInt i2) = VInt (muli i1 i2)
binop DivI (VInt i1) (VInt i2) = VInt (divi i1 i2)
binop _ _ _ = error "binop error"

printArray :: ClContext -> Value -> Value -> ILHost ()
printArray ctx n (VBufPtr _ ty buf) =
  do hostptr <- mmapToHost ctx buf ty (unInt n)
     for (unInt n) (\i ->
       exec void_t "printf" [string "%i: %i\\n", i, index hostptr i])
     unmmap ctx buf hostptr
printArray _ _ _ = error "Print array expects array"

compileProgram :: ClContext -> ClProgram -> HostProgram -> ILHost ()
compileProgram ctx p program =
  let
    go :: VarEnv -> HostProgram -> ILHost ()
    go _ [] = return ()
    go env (DefKernel x k : ss) =
      do mkKernel p (Set.fromList (Map.keys env)) x k
         go env ss
    go env ((Declare (x,ty) e) : ss) =
      do v <- compExp ctx env e
         case v of
           VInt i -> do v' <- let_ x ty i
                        go (Map.insert (x,ty) (VInt v') env) ss
           _ -> go (Map.insert (x,ty) v env) ss
    go env (Call name e es : ss) =
      do n <- compExp ctx env e
         kernelMap <- getsState kernels
         case (Map.lookup name kernelMap) of
           Just k ->
             do callKernel ctx env k n es 
                go env ss
           Nothing -> error "Call to unknown kernel"
    go env (PrintArray size arr : ss) =
      do n <- compExp ctx env size
         arr' <- compExp ctx env arr
         finish ctx
         printArray ctx n arr'
         go env ss
         
  in go Map.empty program

releaseAllDeviceArrays :: ILHost ()
releaseAllDeviceArrays =
  do allocs <- getsState deviceAllocations
     mapM_ releaseDeviceData (Map.keys allocs)

releaseAllKernels :: ILHost ()
releaseAllKernels =
  do kernelMap <- getsState kernels
     mapM_ releaseKernel (Map.map (\(Kernel _ _ _ handle) -> handle) kernelMap)

-- Generate Host-code
compileHost :: HostProgram -> ILHost ()
compileHost program =
  do ctx <- initializeContext 2
     p <- buildProgram ctx "kernels.cl"
     compileProgram ctx p program
     releaseAllDeviceArrays
     releaseAllKernels
     releaseProgram p
     releaseContext ctx

prettyKernels :: KernelMap -> String
prettyKernels kernelMap = pretty (map (\(Kernel _ _ s _) -> s) (Map.elems kernelMap))

createMain :: Statements -> String
createMain body = pretty (includes ++ [function [] "main" body])

includes :: [TopLevel]
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
