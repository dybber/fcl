module Language.FCL.Program where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)

import Control.Monad (forM)

import Language.FCL.ILKernel
import CGen
import CGen.OpenCL.HostCode

type Name = String

data HostExp kernelrep =
    EInt Int
  | EVar VarName
  | EVector [HostExp kernelrep]
  | EAlloc CType (HostExp kernelrep)
  | EKernel kernelrep

------ Statements are either ------
--   - An allocation: a variable name (incl. type) and size in number of elements
--   - A kernel definition: name and representation of the kernel
--   - Assignment: name and expression
--   - Kernel call: kernel name, work group size, non-explicit arguments
-----------------------------------
data HostStmt kernelrep =
   Declare Name (HostExp kernelrep)
 | Call Name (HostExp kernelrep) [VarName]

type HostProgram kernelrep = [HostStmt kernelrep]

data Kernel =
  Kernel
    [VarName] -- implicit parameters, bound in host code (inputs)
    [CType]   -- explicit parameters, given at call site (outputs)
    TopLevel  -- kernel declaration
    ClKernel  -- kernel handle in hostcode

type KernelMap = Map Name Kernel
type AllocationMap = Map Name (CType, Int, ClDeviceBuffer)

data CompileHostState =
  CompileHostState
    { kernels :: KernelMap
    , allocations :: AllocationMap
    }

initHostState :: CompileHostState
initHostState =
  CompileHostState
    { kernels = Map.empty
    , allocations = Map.empty
    }

type ILHost a = CGen CompileHostState a

addKernel :: Name -> Kernel -> ILHost ()
addKernel name k =
  modifyState (\s -> s { kernels = Map.insert name k (kernels s) })

allocate :: ClContext -> Name -> CType -> Int -> ILHost ()
allocate ctx name ty n =
  do buf <- allocDevice ctx ReadWrite n ty
     modifyState (\s -> s { allocations = (Map.insert name (ty, n, buf) (allocations s)) })

lookupAlloc :: Name -> ILHost (Maybe (CType, Int, ClDeviceBuffer))
lookupAlloc name =
  do allocs <- getsState allocations
     return (Map.lookup name allocs)

-- Find all free variables:
---- free variables occuring in host-code are named inputs
---- free variables not in host-code are outputs, which
-----  can first be resolved at call sites
generateKernels :: ClProgram -> CompileState -> HostProgram ([VarName], ILKernel ()) -> ILHost ()
generateKernels p compstate prog =
  let
    mkKernel :: Set VarName -> Name -> ([VarName], ILKernel ()) -> ILHost ()
    mkKernel env name (explicit_params, body) =
      do (stmts, _, _) <- embed body compstate
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

    go :: Set VarName -> HostProgram ([VarName], ILKernel ()) -> ILHost ()
    go _   []                         = return ()
    go env (Declare name e : ss) =
      case e of
        EKernel k -> mkKernel env name k >> go env ss
        EAlloc ty _  -> go (Set.insert (name, pointer_t [] ty) env) ss
        _ -> error ""
    -- go env (Alloc v _ : ss)           = go (Set.insert v env) ss
    -- go env (DefineKernel name k : ss) = mkKernel env name k >> go env ss
--    go env (Assign _ _ : ss)          = go env ss
    go env (Call _ _ _ : ss)          = go env ss
      
  in go Set.empty prog


-- Generate Host-code
compileHost :: CompileState -> HostProgram ([VarName], ILKernel ()) -> ILHost ()
compileHost compstate program =
  let
    comp :: ClContext -> HostProgram a -> ILHost ()
    comp _ [] = return ()
    comp ctx (Declare v (EAlloc ty (EInt n)) : ss) =
      do allocate ctx v ty n
         comp ctx ss
--    comp _  (Assign _ _ : _) = error "not implemented"
    comp ctx ((Declare _ (EKernel _)) : ss) = comp ctx ss
    comp ctx ((Declare _ (EInt _)) : ss) = error "TODO"
    comp ctx (Call name (EInt n) es : ss) =
      do kernelMap <- getsState kernels
         case (Map.lookup name kernelMap) of
           Just (Kernel implicit _ _ kernelHandle) ->
             do -- set shared memory
                setKernelArg kernelHandle 0 int32_t 2048 Nothing
                -- set input parameters: map over kernel list, lookup allocations in environment
                let p = length implicit
                _ <- forM (zip implicit [1..p]) 
                          (\((v,ty),i) ->
                            do Just (_, size, buf) <- lookupAlloc v
                               setKernelArg kernelHandle i ty size (Just buf))
                _ <- forM (zip es [p+1..])
                          (\((v,ty),i) ->
                            do Just (_, size, buf) <- lookupAlloc v
                               setKernelArg kernelHandle i ty size (Just buf))
                -- set output parameters: map over es, lookup allocations in environment
                invokeKernel ctx kernelHandle n (256 :: Int)
                comp ctx ss
           _ -> error ""

  in do ctx <- initialize 2
        p <- buildProgram ctx "kernels.cl"
        generateKernels p compstate program
        comp ctx program
        allocs <- getsState allocations
        mapM_ releaseDeviceData (Map.map (\(_,_,a) -> a) allocs)
        kernelMap <- getsState kernels
        mapM_ releaseKernel (Map.map (\(Kernel _ _ _ handle) -> handle) kernelMap)
        releaseProgram p
        releaseContext ctx

prettyKernels :: KernelMap -> String
prettyKernels kernelMap = pretty (map (\(Kernel _ _ s _) -> s) (Map.elems kernelMap))

createMain :: Statements -> String
createMain body = pretty [function [] "main" body]
  
compile :: CompileState -> HostProgram ([VarName], ILKernel ()) -> (String, String)
compile compstate program =
 let (mainBody, _, _, finalState) =
       runCGen initHostState (compileHost compstate program)
 in (createMain mainBody,
     prettyKernels (kernels finalState))
