-- TODO IL
--- allocation on host: array of pairs -> pair of arrays
--- conditional expression on pairs of values
--- move optimization to IL from CGen
--- typechecker
--- parser

module Language.FCL.IL.CodeGen where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)

import CGen hiding (freeVars)
import CGen.OpenCL.HostCode

import Language.FCL.CompileConfig
import Language.FCL.IL.Syntax

data Kernel =
  Kernel
    [ILName]    -- parameters, bound in host-code
    TopLevel  -- kernel declaration
    ClKernel  -- kernel handle in hostcode

data Value =
    VInt CExp
  | VBool CExp
  | VDouble CExp
  | VString CExp
  | VKernelArray ILType VarName -- size, elem type and ptr
  | VHostBuffer ILType ClDeviceBuffer -- size, elem type and opencl memobject

data Var =
    VarInt VarName
  | VarBool VarName
  | VarDouble VarName
  | VarString VarName
  | VarKernelArray ILType VarName
  | VarHostBuffer ILType ClDeviceBuffer

type VarEnv = Map.Map ILName Var

getVarName :: Var -> VarName
getVarName (VarInt x) = x
getVarName (VarBool x) = x
getVarName (VarDouble x) = x
getVarName (VarString x) = x
getVarName (VarKernelArray _ x) = x
getVarName (VarHostBuffer _ (ClDeviceBuffer x)) =  x

unInt :: Value -> CExp
unInt (VInt i) = i
unInt _ = error ("Unexpected value, expecting integer.")

unBool :: Value -> CExp
unBool (VBool i) = i
unBool _ = error ("Unexpected value, expecting bool.")

unString :: Value -> CExp
unString (VString str) = str
unString _ = error ("Unexpected value, expecting string.")

convertType :: ILType -> CType
convertType ILInt = int32_t
convertType ILBool = bool_t
convertType ILDouble = double_t
convertType ILString = string_t
convertType (ILArray ty) = pointer_t [] (convertType ty)

hostVarToKernelVar :: VarEnv -> ILName -> CGen a Var
hostVarToKernelVar env v@(x,_) =
  case Map.lookup v env of
    Just (VarInt _) -> VarInt `fmap` (newVar int32_t x)
    Just (VarBool _) -> VarBool `fmap` (newVar bool_t x)
    Just (VarDouble _) -> VarDouble `fmap` (newVar double_t x)
    Just (VarHostBuffer elemty _) -> VarKernelArray elemty `fmap` (newVar (pointer_t [attrGlobal] (convertType elemty)) x)
    Just (VarString _) -> error "string"
    Just (VarKernelArray _ _) -> error "kernel array"
    Nothing -> error ("not defined: " ++ x)

---------------------------
-- Host generation monad --
---------------------------

-- Host monad
type ILHost a = CGen HostState a
type KernelMap = Map String Kernel

data HostState =
  HostState
    { kernels :: KernelMap
    , deviceAllocations :: Map ClDeviceBuffer (CExp, ILType)
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
sharedMemPtrName = ("sbase", pointer_t [attrLocal] uint8_t)

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

compExp :: VarEnv -> ILExp -> Value
compExp _ (EInt i) = VInt (constant i)
compExp _ (EBool b) = VBool (constant b)
compExp _ (EDouble d) = VDouble (constant d)
compExp _ (EString s) = VString (string s)
compExp env (EIndex x i) =
  let v1 = compExp env i
  in case Map.lookup x env of
       (Just (VarKernelArray ILInt v)) -> VInt (index v (unInt v1))
       (Just (VarKernelArray ILBool v)) -> VBool (index v (unInt v1))
       _ -> error "not an array"
compExp env (EVar x) =
  case Map.lookup x env of
    (Just (VarInt v)) -> VInt (var v)
    (Just (VarBool v)) -> VBool (var v)
    (Just (VarDouble v)) -> VDouble (var v)
    (Just (VarString v)) -> VString (var v)
    (Just (VarKernelArray ty v)) -> VKernelArray ty v
    (Just (VarHostBuffer ty buf)) -> VHostBuffer ty buf
    Nothing -> error ("Undefined variable " ++ show x)
compExp env (EUnaryOp op e0) =
  let v0 = compExp env e0
  in unop op v0
compExp env (EBinOp op e0 e1) =
  let v0 = compExp env e0
      v1 = compExp env e1
  in binop op v0 v1
compExp env (EIf e0 e1 e2) =
  let v0 = compExp env e0
      v1 = compExp env e1
      v2 = compExp env e2
  in case (v1, v2) of
       (VInt c1, VInt c2) -> VInt (if_ (unBool v0) c1 c2)
       (VBool c1, VBool c2) -> VBool (if_ (unBool v0) c1 c2)
       (VDouble c1, VDouble c2) -> VDouble (if_ (unBool v0) c1 c2)
       (VString c1, VString c2) -> VString (if_ (unBool v0) c1 c2)
       (_, _) -> error "cond"

unop :: UnaryOp -> Value -> Value
unop AbsI (VInt i0) = VInt (absi i0)
unop AbsD (VDouble i0) = VDouble (absd i0)
unop SignI (VInt i0) = VInt (signi i0)
unop op _ = error ("operation not implemented yet: " ++ show op)

binop :: BinOp -> Value -> Value -> Value
binop AddI (VInt i1) (VInt i2) = VInt (addi i1 i2)
binop SubI (VInt i1) (VInt i2) = VInt (subi i1 i2)
binop MulI (VInt i1) (VInt i2) = VInt (muli i1 i2)
binop DivI (VInt i1) (VInt i2) = VInt (divi i1 i2)
binop MinI (VInt i1) (VInt i2) = VInt (mini i1 i2)
binop ModI (VInt i1) (VInt i2) = VInt (modi i1 i2)
binop NeqI (VInt i1) (VInt i2) = VBool (neqi i1 i2)
binop LtI (VInt i1) (VInt i2) = VBool (lti i1 i2)
binop Sll (VInt i1) (VInt i2) = VInt (sll i1 i2)
binop Srl (VInt i1) (VInt i2) = VInt (srl i1 i2)
binop op _ _ = error ("binary operation not implemented yet: " ++ show op)

assignVar :: VarEnv -> ILName -> Value -> CGen a ()
assignVar env x v =
  case (Map.lookup x env, v) of
    (Just (VarInt x'), VInt v') -> assign x' v'
    (Just (VarBool x'), VBool v') -> assign x' v'
    _ -> error "TODO assignvar"

assignSub :: VarEnv -> ILName -> Value -> Value -> CGen a ()
assignSub env x ix v =
  case (Map.lookup x env, ix, v) of
    (Just (VarKernelArray ILInt x'), VInt ix', VInt v') -> assignArray x' v' ix'
    (Just (VarKernelArray ILBool x'), VInt ix', VBool v') -> assignArray x' v' ix'
    _ -> error "TODO assignsub"

------------------------
-- Kernel compilation --
------------------------
compKernelBody :: CompileConfig -> VarEnv -> [Stmt] -> ILKernel ()
compKernelBody cfg env stmts =
  case stmts of
    [] -> return ()
    (Declare (x,ty) e : ss) ->
      do let v = compExp env e
         v' <- lett x v
         compKernelBody cfg (Map.insert (x,ty) v' env) ss
    (Alloc (x,ty) elemty e : ss) ->
       do let size = compExp env e
          sizeVar <- letVar "size" int32_t (unInt size)
          let cty = convertType elemty
          ptr <- allocateKernel cty (var sizeVar)
          v <- lett "arr" (VKernelArray elemty ptr)
          compKernelBody cfg (Map.insert (x,ty) v env) ss
          -- TODO: deallocate?
    (ParFor _ loopVar loopBound body : ss) ->
      do let ub = compExp env loopBound
         forAllBlock cfg (unInt ub)
                (\i -> do i' <- lett "ub" (VInt i)
                          compKernelBody cfg (Map.insert loopVar i' env) body)
         compKernelBody cfg env ss
    (SeqFor loopVar loopBound body : ss) ->
      do let ub = compExp env loopBound
         for (unInt ub)
             (\i -> do i' <- lett "ub" (VInt i)
                       compKernelBody cfg (Map.insert loopVar i' env) body)
         compKernelBody cfg env ss
    (Synchronize : ss) ->
      do syncLocal
         compKernelBody cfg env ss
    (Assign x e : ss) ->
      do let e' = compExp env e
         assignVar env x e'
         compKernelBody cfg env ss
    (AssignSub x ix e : ss) ->
      do let ix' = compExp env ix
         let e' = compExp env e
         assignSub env x ix' e'
         compKernelBody cfg env ss
    (While stopCond body : ss) ->
      do let v = compExp env stopCond
         whileLoop (unBool v) (compKernelBody cfg env body)
         compKernelBody cfg env ss
    (If cond then_ else_ : ss) ->
      do let cond' = compExp env cond
         iff (unBool cond')
             (compKernelBody cfg env then_
             ,compKernelBody cfg env else_)
         compKernelBody cfg env ss
    (Distribute _ loopVar loopBound body : ss) ->
      do let ub = compExp env loopBound
         forAllBlock cfg (unInt ub)
                (\i -> do i' <- lett "ub" (VInt i)
                          compKernelBody cfg (Map.insert loopVar i' env) body)
         compKernelBody cfg env ss
    (ReadIntCSV _ _ _ : _)     -> error "reading input-data: not possible at block level"
    (PrintIntArray _ _ : _)  -> error "printing not possible at block level"

mkKernelBody :: CompileConfig -> VarEnv -> ILName -> ILExp -> [Stmt] -> ILKernel ()
mkKernelBody cfg env loopVar loopBound body =
  do let ub = compExp env loopBound
     distrParBlock cfg (unInt ub) (\i ->
       do i' <- lett "ub" (VInt i)
          compKernelBody cfg (Map.insert loopVar i' env) body)
  
distrParBlock :: CompileConfig -> CExp -> (CExp -> ILKernel ()) -> ILKernel ()
distrParBlock cfg ub' f =
    do let workgroups = constant (configNumWorkGroups cfg)
       ub <- let_ "ub" int32_t ub'
       q <- let_ "blocksQ" int32_t (ub `divi` workgroups)
       for q (\i -> do
                 j <- let_ "j" int32_t ((workgroupID `muli` q) `addi` i)
                 f j)
       iff (workgroupID `lti` (ub `modi` workgroups))
           (do j <- let_ "wid" int32_t ((workgroups `muli` q) `addi` workgroupID)
               f j
           , return ())

-- A block-level computation using blockSize threads, to evaluate
-- a parallel map
--  - evaluating "f j" for every j in [0..n-1]
forAllBlock :: CompileConfig -> CExp -> (CExp -> ILKernel ()) -> ILKernel ()
forAllBlock cfg n f =
    do let blockSize = constant (configBlockSize cfg)
       ub <- let_ "ub" int32_t n
       q <- let_ "q" int32_t (ub `divi` blockSize)
       for q (\i -> do j <- let_ "j" int32_t ((i `muli` blockSize) `addi` localID)
                       f j)
       iff (localID `lti` (ub `modi` blockSize))
         (do j <- let_ "tid" int32_t ((q `muli` blockSize) `addi` localID)
             f j
         , return ())
       syncLocal

lett :: String -> Value -> CGen a Var
lett x (VInt e) = VarInt `fmap` (letVar x int32_t e)
lett x (VBool e) = VarBool `fmap` (letVar x bool_t e)
lett x (VDouble e) = VarDouble `fmap` (letVar x double_t e)
lett _ (VString _) = error "string declarations: not implemented yet"
lett _ (VKernelArray ty v) = return (VarKernelArray ty v)
lett _ (VHostBuffer ty buf) = return (VarHostBuffer ty buf)

---------------------
-- Host compilation
---------------------
compHost :: CompileConfig -> ClContext -> ClProgram -> VarEnv -> [Stmt] -> ILHost ()
compHost cfg ctx p env stmts =
  case stmts of
    [] -> return ()
    (PrintIntArray size arr : ss) ->
      do let n = compExp env size
         let arr' = compExp env arr
         finish ctx
         printArray ctx n arr'
         compHost cfg ctx p env ss
    (Declare (x,ty) e : ss) ->
      do let v = compExp env e
         v' <- lett x v
         compHost cfg ctx p (Map.insert (x,ty) v' env) ss
    (Distribute _ x e stmts' : ss) -> -- use level for something? Only for type check?
      do distribute cfg ctx p env x e stmts'
         compHost cfg ctx p env ss
    (ParFor _ _ _ _ : _) -> error "parfor<grid>: Not supported yet"
    (SeqFor _ _ _ : _) -> error "seqfor: Only available at thread-level"
    (Synchronize : ss) ->
      do finish ctx
         compHost cfg ctx p env ss
    (ReadIntCSV (x,ty) xlen e : ss) ->
      do let filename = compExp env e
         (valuesRead, hostPtr) <- readCSVFile int32_t filename
         assignVar env xlen (VInt (var valuesRead))
         v <- copyToDevice ctx ILInt (var valuesRead) (var hostPtr)
         compHost cfg ctx p (Map.insert (x,ty) v env) ss
    (Alloc (x,ty) elemty e : ss) ->
      do let size = compExp env e
         v <- allocate ctx elemty size
         compHost cfg ctx p (Map.insert (x,ty) v env) ss
    (Assign x e : ss) ->
      do let e' = compExp env e
         assignVar env x e'
         compHost cfg ctx p env ss
    (AssignSub x ix e : ss) ->
      do let ix' = compExp env ix
             e' = compExp env e
         assignSub env x ix' e'
         compHost cfg ctx p env ss
    (While stopCond body : ss) ->
      do let v = compExp env stopCond
         whileLoop (unBool v) (compHost cfg ctx p env body)
         compHost cfg ctx p env ss
    (If cond then_ else_ : ss) ->
      do let cond' = compExp env cond
         iff (unBool cond')
             (compHost cfg ctx p env then_
             ,compHost cfg ctx p env else_)
         compHost cfg ctx p env ss

distribute :: CompileConfig -> ClContext -> ClProgram -> VarEnv -> ILName -> ILExp -> [Stmt] -> ILHost ()
distribute cfg ctx p env loopVar loopBound stmts =
  let
    createArgument :: ILName -> ILHost KernelArg
    createArgument (x, ty) =
      do case Map.lookup (x,ty) env of
           Just (VarInt v) -> return (ArgScalar (convertType ty) (var v))
           Just (VarBool v) -> return (ArgScalar (convertType ty) (var v))
           Just (VarDouble v) -> return (ArgScalar (convertType ty) (var v))
           Just (VarHostBuffer _ buf) -> return (ArgBuffer buf)
           Just (VarKernelArray _ _) -> error "kernel arrays can not occur outside kernels"
           Just (VarString _) -> error "Can not use strings inside kernels"
           Nothing -> error "variable not found"

    callKernel :: Kernel -> ILHost ()
    callKernel (Kernel params _ kernelHandle) =
      do -- set shared memoryp
         let smarg = ArgSharedMemory (constant (configSharedMemory cfg))
         -- set input parameters: map over kernel list, lookup allocations in environment
         args <- mapM createArgument params
         invokeKernel ctx kernelHandle (smarg : args)
                                       (muli (constant (configBlockSize cfg)) (constant (configNumWorkGroups cfg)))
                                       (constant (configBlockSize cfg))
  in do kernelName <- newName "kernel"
        k <- mkKernel cfg env kernelName p loopVar loopBound stmts
        addKernel kernelName k -- save in monad state, to be able to free it in the end
        callKernel k

-- create parameter list (newVar)
-- create VarEnv mapping free variables to the parameter names
     -- also add the variable used as loop argument to distrPar
-- call compKernelBody with this new VarEnv
-- add shared memory argument
-- create kernel definition (using distrParBlock)
-- create/build kernel on host
-- call the kernel using the same order of arguments, as in the parameter list.
mkKernel :: CompileConfig -> VarEnv -> String -> ClProgram -> ILName -> ILExp -> [Stmt] -> ILHost Kernel
mkKernel cfg env kernelName p loopVar loopBound stmts =
  let params = Set.toList (Set.delete loopVar (freeVars stmts `Set.union` liveInExp loopBound))
      mkArgumentList :: [ILName] -> ILHost [Var]
      mkArgumentList = mapM (hostVarToKernelVar env)
      
  in do args <- mkArgumentList params
        kernelHandle <- createKernel p kernelName
        let kernelEnv = Map.fromList (zip params args)
        (kernelBody, _, _) <- embed (mkKernelBody cfg kernelEnv loopVar loopBound stmts) initKernelState
        
        let fndef = kernel kernelName (sharedMemPtrName : map getVarName args) kernelBody
        return (Kernel params fndef kernelHandle)

allocate :: ClContext -> ILType -> Value -> ILHost Var
allocate ctx elemty size =
  do buf <- allocDevice ctx ReadWrite (unInt size) (convertType elemty)
     modifyState (\s -> s { deviceAllocations = Map.insert buf (unInt size, elemty) (deviceAllocations s) })
     return (VarHostBuffer elemty buf)

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

copyToDevice :: ClContext -> ILType -> CExp -> CExp -> ILHost Var
copyToDevice ctx elemty n hostptr =
  do buf <- dataToDevice ctx ReadWrite n (convertType elemty) hostptr
     modifyState (\s -> s { deviceAllocations = Map.insert buf (n, elemty) (deviceAllocations s) })
     return (VarHostBuffer elemty buf)

printArray :: ClContext -> Value -> Value -> ILHost ()
printArray ctx n (VHostBuffer elemty buf) =
  do hostptr <- mmapToHost ctx buf (convertType elemty) (unInt n)
     exec void_t "printf" [string "["]
     let lastElem = subi (unInt n) (constant (1 :: Int))
     for lastElem (\i ->
       exec void_t "printf" [string "%i,", index hostptr i])
     exec void_t "printf" [string "%i", index hostptr lastElem]
     exec void_t "printf" [string "]\\n"]
     unmmap ctx buf hostptr
printArray _ _ _ = error "Print array expects array"

---------------------
-- Compile program --
---------------------
compProgram :: CompileConfig -> [Stmt] -> ILHost ()
compProgram cfg program =
  do ctx <- initializeContext (configVerbosity cfg)
     p <- buildProgram ctx (configKernelsFilename cfg)
     compHost cfg ctx p Map.empty program
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
createMain body = pretty (includes ++ [function int32_t [] "main" body])

includes :: [TopLevel]
includes = [includeSys "stdio.h",
            includeSys "sys/time.h",
            includeSys "mcl.h",
            includeSys "fcl.h"]
                  
codeGen :: CompileConfig -> ILProgram -> (String, String)
codeGen cfg program =
 let (mainBody, _, _, finalState) =
       runCGen initHostState (compProgram cfg program)
 in (createMain mainBody,
     prettyKernels (kernels finalState))

compileAndOutput :: CompileConfig -> ILProgram -> FilePath -> IO ()
compileAndOutput cfg program path =
  let (main, kernelsfile) = codeGen cfg program
      mainPath = path ++ "/main.c"
      kernelPath = path ++ "/kernels.cl"
  in do writeFile mainPath main
        writeFile kernelPath kernelsfile

