-- TODO IL
--
-- o converting kernel-level environment, to host-level environment
-- o convertVar that maps from Name's to Value's
-- o Pass lengths through as parameters to kernels
-- o Assigning new lengths to arrays??
-- o Deallocating in kernels?

-- TODO Compiling FCL
--  o Program monad for IL
--  o FCL to IL compilation
--  o Extend IL with necessary constructs

-- TODO nice-to-have
-- o IL parser
-- o IL type checker

module Language.FCL.IL.Compile where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)

import CGen hiding (freeVars)
import CGen.OpenCL.HostCode

import Language.FCL.IL.Syntax

data Kernel =
  Kernel
    [Name]    -- parameters, bound in host-code
    TopLevel  -- kernel declaration
    ClKernel  -- kernel handle in hostcode

data Value =
    VInt VarName
  | VBool VarName
  | VString VarName
  | VKernelArray VarName CType VarName -- size, elem type and ptr
  | VHostBuffer VarName CType ClDeviceBuffer -- size, elem type and opencl memobject

type VarEnv = Map.Map Name Value

unInt :: Value -> CExp
unInt (VInt i) = var i
unInt _ = error ("Unexpected value, expecting integer.")

unBool :: Value -> CExp
unBool (VBool i) = var i
unBool _ = error ("Unexpected value, expecting bool.")

unString :: Value -> CExp
unString (VString str) = var str
unString _ = error ("Unexpected value, expecting string.")

convertType :: ILType -> CType
convertType IntT = int32_t
convertType BoolT = bool_t
convertType (ArrayT ty) = pointer_t [] (convertType ty)

convertVar :: Name -> CGen a VarName
convertVar (x,ty) = newVar (convertType ty) x

---------------------------
-- Host generation monad --
---------------------------

-- Host monad
type ILHost a = CGen HostState a
type KernelMap = Map String Kernel

data HostState =
  HostState
    { kernels :: KernelMap
    , deviceAllocations :: Map ClDeviceBuffer (CExp, CType)
    , hostAllocations :: Set VarName
    }

initHostState :: HostState
initHostState =
  HostState
    { kernels = Map.empty
    , deviceAllocations = Map.empty
    , hostAllocations = Set.empty
    }

addKernel :: String -> Kernel -> ILHost ()
addKernel name k =
  modifyState (\s -> s { kernels = Map.insert name k (kernels s) })

-----------------------------
-- Kernel generation monad --
-----------------------------
type ILKernel a = CGen KernelState a

sharedMemPtrName :: VarName
sharedMemPtrName = ("sbase", pointer_t [attrLocal] int32_t)

data KernelState =
  KernelState { allocPtrOffset :: CExp
              , sharedMemPointer :: VarName
              }

initKernelState :: KernelState
initKernelState =
  KernelState { allocPtrOffset = constant (0 :: Int)
              , sharedMemPointer = sharedMemPtrName
              }

allocateKernel :: CType -> CExp -> ILKernel VarName
allocateKernel cty n =
  do offset <- getsState allocPtrOffset
     sbase <- getsState sharedMemPointer
     let aty = pointer_t [attrLocal] cty
     v <- letVar "arr" aty (cast aty (var sbase `addPtr` offset))
     let bytes = n `muli` (sizeOf cty)
     modifyState (\s -> s { allocPtrOffset = offset `addi` bytes })
     return v

----------------------------------------------
-- Compiling expressions - both host/kernel --
----------------------------------------------

compExp :: VarEnv -> ILExp -> CGen a Value
compExp _ (EInt i) = VInt `fmap` (letVar "ci" int32_t (constant i))
compExp _ (EBool b) = VBool `fmap` (letVar "cb" bool_t (constant b))
compExp env (EVar x) =
  do case Map.lookup x env of
       Just v -> return v
       Nothing -> error ("Undefined variable " ++ show x)
compExp env (ELength e) =
  do arr <- compExp env e
     case arr of
       (VHostBuffer size _ _) -> return (VInt size)
       (VKernelArray size _ _) -> return (VInt size)
       _ -> error "Trying to take length of non-array value."
compExp env (EBinOp op e1 e2) =
  do v1 <- compExp env e1
     v2 <- compExp env e2
     binop op v1 v2
compExp _ (EString _) = error "TODO"

binop :: BinOp -> Value -> Value -> CGen a Value
binop AddI (VInt i1) (VInt i2) = VInt `fmap` (letVar "addi" int32_t (addi (var i1) (var i2)))
binop SubI (VInt i1) (VInt i2) = VInt `fmap` (letVar "subi" int32_t (subi (var i1) (var i2)))
binop MulI (VInt i1) (VInt i2) = VInt `fmap` (letVar "muli" int32_t (muli (var i1) (var i2)))
binop DivI (VInt i1) (VInt i2) = VInt `fmap` (letVar "divi" int32_t (divi (var i1) (var i2)))
binop _ _ _ = error "binop error"

assignVar :: VarEnv -> Name -> Value -> CGen a ()
assignVar env x v =
  case (Map.lookup x env, v) of
    (Just (VInt x'), VInt v') -> assign x' (var v')
    (Just (VBool x'), VBool v') -> assign x' (var v')
    _ -> error "TODO"

assignSub :: VarEnv -> Name -> Value -> Value -> CGen a ()
assignSub env x ix v =
  case (Map.lookup x env, ix, v) of
    (Just (VInt x'), VInt ix', VInt v') -> assignArray x' (var ix') (var v')
    (Just (VBool x'), VInt ix', VBool v') -> assignArray x' (var ix') (var v')
    _ -> error "TODO"

------------------------
-- Kernel compilation --
------------------------
compKernelBody :: VarEnv -> [Stmt] -> ILKernel ()
compKernelBody env stmts =
  case stmts of
    [] -> return ()
    (Declare (x,ty) e : ss) ->
      do v <- compExp env e
         compKernelBody (Map.insert (x,ty) v env) ss
    (Alloc (x,ty) elemty e : ss) ->
       do size <- compExp env e
          sizeVar <- letVar "size" int32_t (unInt size)
          let cty = convertType elemty
          ptr <- allocateKernel cty (var sizeVar)
          let v = VKernelArray sizeVar cty ptr
          compKernelBody (Map.insert (x,ty) v env) ss
          -- TODO: deallocate?
    (ParFor _ loopVar loopBound body : ss) ->
      do ub <- compExp env loopBound
         forAllBlock (unInt ub)
                (\i -> do i' <- letVar "i" int32_t i
                          compKernelBody (Map.insert loopVar (VInt i') env) body)
         compKernelBody env ss                             
    (Synchronize : ss) ->
      do syncLocal
         compKernelBody env ss
    (Assign x e : ss) ->
      do e' <- compExp env e
         assignVar env x e'
         compKernelBody env ss
    (AssignSub x ix e : ss) ->
      do ix' <- compExp env ix
         e' <- compExp env e
         assignSub env x ix' e'
         compKernelBody env ss
    (While stopCond body : ss) ->
      do v <- compExp env stopCond
         whileLoop (unBool v) (compKernelBody env body)
         compKernelBody env ss
    (Cond cond then_ else_ : ss) ->
      do cond' <- compExp env cond
         iff (unBool cond')
             (compKernelBody env then_
             ,compKernelBody env else_)
         compKernelBody env ss
    (Distribute _ _ _ _ : _) -> error "Not possible at block level"
    (ReadIntCSV _ _ : _)     -> error "reading input-data: not possible at block level"
    (PrintIntArray _ _ : _)  -> error "printing not possible at block level"

mkKernelBody :: VarEnv -> Name -> ILExp -> [Stmt] -> ILKernel ()
mkKernelBody env loopVar loopBound body =
  do ub <- compExp env loopBound
     distrParBlock (unInt ub) (\i ->
       do i' <- letVar "i" int32_t i
          compKernelBody (Map.insert loopVar (VInt i') env) body)
  
distrParBlock :: CExp -> (CExp -> ILKernel ()) -> ILKernel ()
distrParBlock ub' f =
    do ub <- let_ "ub" int32_t ub'
       q <- let_ "blocksQ" int32_t (ub `divi` numWorkgroups)
       for q (\i -> do
                 j <- let_ "j" int32_t ((workgroupID `muli` q) `addi` i)
                 f j)
       iff (workgroupID `lti` (ub `modi` numWorkgroups))
           (do j <- let_ "j" int32_t ((numWorkgroups `muli` q) `addi` workgroupID)
               f j
           , return ())

blockSize :: CExp
blockSize = constant (256 :: Int)

-- A block-level computation using blockSize threads, to evaluate
-- a parallel map
--  - evaluating "f j" for every j in [0..n-1]
forAllBlock :: CExp -> (CExp -> ILKernel ()) -> ILKernel ()
forAllBlock n f =
    do ub <- let_ "ub" int32_t n
       q <- let_ "q" int32_t (ub `divi` blockSize)
       for q (\i -> do j <- let_ "j" int32_t ((i `muli` blockSize) `addi` localID)
                       f j)
       iff (localID `lti` (ub `modi` blockSize))
         (do j <- let_ "j" int32_t ((q `muli` blockSize) `addi` localID)
             f j
         , return ())
       syncLocal

----------------------
-- Host compilation --
----------------------
compHost :: ClContext -> ClProgram -> VarEnv -> [Stmt] -> ILHost ()
compHost ctx p env stmts =
  case stmts of
    [] -> return ()
    (PrintIntArray size arr : ss) ->
      do n <- compExp env size
         arr' <- compExp env arr
         finish ctx
         printArray ctx n arr'
         compHost ctx p env ss
    (Declare (x,ty) e : ss) ->
      do v <- compExp env e
         compHost ctx p (Map.insert (x,ty) v env) ss
    (Distribute _ x e stmts' : ss) -> -- use level for something? Only for type check?
      do distribute ctx p env x e stmts'
         compHost ctx p env ss
    (ParFor _ _ _ _ : _) -> error "parfor<grid>: Not supported yet"
    (Synchronize : ss) ->
      do finish ctx
         compHost ctx p env ss
    (ReadIntCSV (x,ty) e : ss) ->
      do filename <- compExp env e
         (valuesRead, hostPtr) <- readCSVFile int32_t filename
         v <- copyToDevice ctx int32_t (var valuesRead) (var hostPtr)
         compHost ctx p (Map.insert (x,ty) v env) ss
    (Alloc (x,ty) elemty e : ss) ->
      do size <- compExp env e
         v <- allocate ctx (convertType elemty) size
         compHost ctx p (Map.insert (x,ty) v env) ss
    (Assign x e : ss) ->
      do e' <- compExp env e
         assignVar env x e'
         compHost ctx p env ss
    (AssignSub x ix e : ss) ->
      do ix' <- compExp env ix
         e' <- compExp env e
         assignSub env x ix' e'
         compHost ctx p env ss
    (While stopCond body : ss) ->
      do v <- compExp env stopCond
         whileLoop (unBool v) (compHost ctx p env body)
         compHost ctx p env ss
    (Cond cond then_ else_ : ss) ->
      do cond' <- compExp env cond
         iff (unBool cond')
             (compHost ctx p env then_
             ,compHost ctx p env else_)
         compHost ctx p env ss

distribute :: ClContext -> ClProgram -> VarEnv -> Name -> ILExp -> [Stmt] -> ILHost ()
distribute ctx p env loopVar loopBound stmts =
  let
    createArgument :: Name -> ILHost KernelArg
    createArgument (x, ty) =
      do case Map.lookup (x,ty) env of
           Just (VHostBuffer _ _ buf) -> return (ArgBuffer buf)
           Just (VInt i) -> return (ArgScalar (convertType ty) (var i))
           Just (VBool b) -> return (ArgScalar (convertType ty) (var b))
           Just _ -> error "Strings can not be passed as arguments to kernels"
           Nothing -> error "variable not found"

    callKernel :: Kernel -> Value -> ILHost ()
    callKernel (Kernel params _ kernelHandle) n =
      do -- set shared memoryp
         let smarg = ArgSharedMemory (constant (2048 :: Int))
         -- set input parameters: map over kernel list, lookup allocations in environment
         args <- mapM createArgument params
         invokeKernel ctx kernelHandle (smarg : args)
                                       (unInt n)
                                       (constant (256 :: Int))
  in do kernelName <- newName "kernel"
        k <- mkKernel kernelName p loopVar loopBound stmts
        addKernel kernelName k -- save in monad state, to be able to free it in the end
        x <- letVar "globalSize" int32_t (constant (1024 :: Int))
        callKernel k (VInt x)

-- create parameter list (newVar)
-- create VarEnv mapping free variables to the parameter names
     -- also add the variable used as loop argument to distrPar
-- call compKernelBody with this new VarEnv
-- add shared memory argument
-- create kernel definition (using distrParBlock)
-- create/build kernel on host
-- call the kernel using the same order of arguments, as in the parameter list.
mkKernel :: String -> ClProgram -> Name -> ILExp -> [Stmt] -> ILHost Kernel
mkKernel kernelName p loopVar loopBound stmts =
  let params = Set.toList (freeVars stmts `Set.union` liveInExp loopBound)
      mkArgumentList :: [Name] -> ILHost [VarName]
      mkArgumentList = mapM convertVar
      
      mkKernelEnv :: [(Name,VarName)] -> VarEnv
      mkKernelEnv _ = undefined
        
  in do args <- mkArgumentList params
        kernelHandle <- createKernel p kernelName
        let kernelEnv = mkKernelEnv (zip params args)
        (kernelBody, _, _) <- embed (mkKernelBody kernelEnv loopVar loopBound stmts) initKernelState
        
        let fndef = kernel kernelName args kernelBody
        return (Kernel params fndef kernelHandle)

allocate :: ClContext -> CType -> Value -> ILHost Value
allocate ctx elemty size =
  do buf <- allocDevice ctx ReadWrite (unInt size) elemty
     modifyState (\s -> s { deviceAllocations = Map.insert buf (unInt size, elemty) (deviceAllocations s) })
     sizeVar <- letVar "size" int32_t (unInt size)
     return (VHostBuffer sizeVar elemty buf)

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
     return (VHostBuffer size elemty buf)

printArray :: ClContext -> Value -> Value -> ILHost ()
printArray ctx n (VHostBuffer _ ty buf) =
  do hostptr <- mmapToHost ctx buf ty (unInt n)
     for (unInt n) (\i ->
       exec void_t "printf" [string "%i: %i\\n", i, index hostptr i])
     unmmap ctx buf hostptr
printArray _ _ _ = error "Print array expects array"

---------------------
-- Compile program --
---------------------
compProgram :: [Stmt] -> ILHost ()
compProgram program =
  do ctx <- initializeContext 2
     p <- buildProgram ctx "kernels.cl"
     compHost ctx p Map.empty program
     releaseAllDeviceArrays
     releaseAllKernels
     releaseProgram p
     releaseContext ctx

releaseAllDeviceArrays :: ILHost ()
releaseAllDeviceArrays =
  do allocs <- getsState deviceAllocations
     mapM_ releaseDeviceData (Map.keys allocs)

releaseAllKernels :: ILHost ()
releaseAllKernels =
  do kernelMap <- getsState kernels
     mapM_ releaseKernel (Map.map (\(Kernel _ _ handle) -> handle) kernelMap)


prettyKernels :: KernelMap -> String
prettyKernels kernelMap = pretty (map (\(Kernel _ s _) -> s) (Map.elems kernelMap))

createMain :: Statements -> String
createMain body = pretty (includes ++ [function [] "main" body])

includes :: [TopLevel]
includes = [includeSys "stdio.h",
            includeSys "sys/time.h",
            includeSys "mcl.h",
            includeSys "fcl.h"]
                  
compile :: ILProgram -> (String, String)
compile program =
 let (mainBody, _, _, finalState) =
       runCGen initHostState (compProgram program)
 in (createMain mainBody,
     prettyKernels (kernels finalState))

compileAndOutput :: ILProgram -> FilePath -> IO ()
compileAndOutput program path =
  let (main, kernelsfile) = compile program
      mainPath = path ++ "/main.c"
      kernelPath = path ++ "/kernels.cl"
  in do writeFile mainPath main
        writeFile kernelPath kernelsfile

