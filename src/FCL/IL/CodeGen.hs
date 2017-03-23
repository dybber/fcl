module FCL.IL.CodeGen (codeGen) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)

import CGen hiding (freeVars)
import CGen.Syntax (Statement(Decl), CExp(Int32E))
import CGen.OpenCL.HostCode

import FCL.IL.Analysis.Liveness
import FCL.IL.Analysis.FreeVars
import FCL.Compile.Config
import FCL.IL.Syntax
import FCL.IL.TypeCheck

data Kernel =
  Kernel
    Int -- number of shared memory arrays
    [(ILName, ILType)]  -- parameters, bound in host-code
    TopLevel  -- kernel declaration
--    ClKernel  -- kernel handle in hostcode

data Value =
    VInt CExp
  | VBool CExp
  | VDouble CExp
  | VString CExp
  | VKernelArray ILType VarName -- size, elem type and ptr
  | VHostBuffer ILType ClDeviceBuffer -- size, elem type and opencl memobject
 deriving Show

data Var =
    VarInt VarName
  | VarBool VarName
  | VarDouble VarName
  | VarString VarName
  | VarKernelArray ILType VarName
  | VarHostBuffer ILType ClDeviceBuffer
 deriving Show

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
hostVarToKernelVar env x =
  case Map.lookup x env of
    Just (VarInt _) -> VarInt `fmap` (newVar int32_t (show x))
    Just (VarBool _) -> VarBool `fmap` (newVar bool_t (show x))
    Just (VarDouble _) -> VarDouble `fmap` (newVar double_t (show x))
    Just (VarHostBuffer elemty _) -> VarKernelArray elemty `fmap` (newVar (pointer_t [attrGlobal] (convertType elemty)) (show x))
    Just (VarString _) -> error "string"
    Just (VarKernelArray _ _) -> error "kernel array"
    Nothing -> error ("not defined: " ++ show x)

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
    , typeEnv :: TypeEnv
    }

initHostState :: TypeEnv -> HostState
initHostState env =
  HostState
    { kernels = Map.empty
    , deviceAllocations = Map.empty
    , hostAllocations = Set.empty
    , typeEnv = env
    }

addKernel :: String -> Kernel -> ILHost ()
addKernel name k =
  modifyState (\s -> s { kernels = Map.insert name k (kernels s) })

-----------------------------
-- Kernel generation monad --
-----------------------------
type ILKernel a = CGen KernelState a

-- sharedMemPtrName :: VarName
-- sharedMemPtrName = ("sbase", pointer_t [attrLocal] uint8_t)

data KernelState =
  KernelState { -- allocPtrOffset :: CExp
              -- , sharedMemPointer :: VarName
              -- , 
                allocations :: [VarName]
              }

initKernelState :: KernelState
initKernelState =
  KernelState { -- allocPtrOffset = constant (0 :: Int)
              -- , sharedMemPointer = sharedMemPtrName
              -- , 
                allocations = []
              }

allocateKernel :: CType -> CExp -> ILKernel VarName
allocateKernel cty n =
  do -- offset <- getsState allocPtrOffset
     -- sbase <- getsState sharedMemPointer
     let aty = pointer_t [attrLocal] cty
     -- v <- letVar "arr" aty (cast aty (var sbase `addPtr` offset))
     -- let bytes = n `muli` (sizeOf cty)
     -- modifyState (\s -> s { allocPtrOffset = offset `addi` bytes })
     v1 <- newVar aty "shared"
     modifyState (\s -> s { allocations = v1 : allocations s })
     return v1

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
binop ModI (VInt i1) (VInt i2) = VInt (modi i1 i2)
binop AddD (VInt i1) (VInt i2) = VInt (addd i1 i2)
binop SubD (VInt i1) (VInt i2) = VInt (subd i1 i2)
binop MulD (VInt i1) (VInt i2) = VInt (muld i1 i2)
binop DivD (VInt i1) (VInt i2) = VInt (divd i1 i2)
binop LtI (VInt i1) (VInt i2) = VBool (lti i1 i2)
binop LteI (VInt i1) (VInt i2) = VBool (ltei i1 i2)
binop NeqI (VInt i1) (VInt i2) = VBool (neqi i1 i2)
binop EqI (VInt i1) (VInt i2) = VBool (eqi i1 i2)
binop Sll (VInt i1) (VInt i2) = VInt (sll i1 i2)
binop Srl (VInt i1) (VInt i2) = VInt (srl i1 i2)
binop Xor (VInt i1) (VInt i2) = VInt (xor i1 i2)
binop MinI (VInt i1) (VInt i2) = VInt (mini i1 i2)
binop MaxI (VInt i1) (VInt i2) = VInt (maxi i1 i2)
binop Land (VInt i1) (VInt i2) = VInt (land i1 i2)
binop op _ _ = error ("binary operation not implemented yet: " ++ show op)

assignVar :: VarEnv -> ILName -> Value -> CGen a ()
assignVar env x v =
  case (Map.lookup x env, v) of
    (Just (VarInt x'), VInt v') -> assign x' v'
    (Just (VarBool x'), VBool v') -> assign x' v'
    (Just (VarString x'), VString v') -> assign x' v'
    (Nothing, _) -> error ("Variable " ++ show x ++ " not defined.")
    (Just x', v') -> error ("TODO assignvar: " ++ show x' ++ " := " ++ show v')

assignSub :: VarEnv -> ILName -> Value -> Value -> CGen a ()
assignSub env x ix v =
  case (Map.lookup x env, ix, v) of
    (Just (VarKernelArray ILInt x'), VInt ix', VInt v') -> assignArray x' v' ix'
    (Just (VarKernelArray ILBool x'), VInt ix', VBool v') -> assignArray x' v' ix'
    _ -> error "TODO assignsub"

------------------------
-- Thread-level compilation --
------------------------
compThread :: CompileConfig -> VarEnv -> [Stmt a] -> ILKernel ()
compThread cfg env stmts =
  case stmts of
    [] -> return ()
    (Declare x _z e _ : ss) ->
      do let v = compExp env e
         v' <- lett (show x) v
         compThread cfg (Map.insert x v' env) ss
    (Alloc x elemty e _ : ss) ->
       do let size = compExp env e
          sizeVar <- letVar "size" int32_t (constant (configBlockSize cfg)) --(muli (unInt size) (constant (configBlockSize cfg)))
          let cty = convertType elemty
          ptr <- allocateKernel cty (var sizeVar)
          let offset = addi (var ptr) (muli localID (unInt size))
          ptr' <- letVar "threadLocal" (snd ptr) offset
          v <- lett "arr" (VKernelArray elemty ptr')
          compThread cfg (Map.insert x v env) ss
    (SeqFor loopVar loopBound body _ : ss) ->
      do let ub = compExp env loopBound
         for (unInt ub)
             (\i -> do i' <- lett "ub" (VInt i)
                       compThread cfg (Map.insert loopVar i' env) body)
         compThread cfg env ss
    (Synchronize _ : ss) ->
       -- synchronization is a no-op on thread-level
       compThread cfg env ss
    (Assign x e _ : ss) ->
      do let e' = compExp env e
         assignVar env x e'
         compThread cfg env ss
    (AssignSub x ix e _ : ss) ->
      do let ix' = compExp env ix
         let e' = compExp env e
         assignSub env x ix' e'
         compThread cfg env ss
    (While stopCond body _ : ss) ->
      do let v = compExp env stopCond
         whileLoop (unBool v) (compThread cfg env body)
         compThread cfg env ss
    (If cond then_ else_ _ : ss) ->
      do let cond' = compExp env cond
         iff (unBool cond')
             (compThread cfg env then_
             ,compThread cfg env else_)
         compThread cfg env ss
    (ParFor _ _ _ _ _ : _)     -> error "Cannot use parallel constructs on thread-level."
    (Distribute _ _ _ _ _ : _) -> error "Cannot use parallel constructs on thread-level."
    (ReadIntCSV _ _ _ _ : _)   -> error "Reading input-data is not possible at thread level."
    (PrintIntArray _ _ _ : _)  -> error "Printing not possible at thread level."
    (Benchmark _ _ _ : _)      -> error "Benchmarking not possible at thread level."

------------------------
-- Kernel compilation --
------------------------
compKernelBody :: CompileConfig -> VarEnv -> [Stmt a] -> ILKernel ()
compKernelBody cfg env stmts =
  case stmts of
    [] -> return ()
    (Declare x _ e _ : ss) ->
      do let v = compExp env e
         v' <- lett (show x) v
         compKernelBody cfg (Map.insert x v' env) ss

    -- allocate shared memory
    (Alloc x elemty e _ : ss) ->
       do let size = compExp env e
          sizeVar <- letVar "size" int32_t (unInt size)
          let cty = convertType elemty
          ptr <- allocateKernel cty (var sizeVar)
          v <- lett "arr" (VKernelArray elemty ptr)
          compKernelBody cfg (Map.insert x v env) ss

    -- lift a thread-level computation to block level
    (ParFor _ loopVar loopBound body _ : ss) ->
      do let ub = compExp env loopBound
         forAllBlock cfg (unInt ub)
                (\i -> do i' <- lett "ub" (VInt i)
                          compThread cfg (Map.insert loopVar i' env) body)
         compKernelBody cfg env ss
    -- lift a thread-level computation to block-level (on this level identical to ParFor)
    (Distribute _ loopVar loopBound body _ : ss) ->
      do let ub = compExp env loopBound
         forAllBlock cfg (unInt ub)
                (\i -> do i' <- lett "ub" (VInt i)
                          compThread cfg (Map.insert loopVar i' env) body)
         compKernelBody cfg env ss
    (Synchronize _ : ss) ->
      do syncLocal
         compKernelBody cfg env ss
    (Assign x e _ : ss) ->
      do let e' = compExp env e
         assignVar env x e'
         compKernelBody cfg env ss
    (AssignSub x ix e _ : ss) ->
      do let ix' = compExp env ix
         let e' = compExp env e
         assignSub env x ix' e'
         compKernelBody cfg env ss
    (While stopCond body _ : ss) ->
      do let v = compExp env stopCond
         whileLoop (unBool v) (compKernelBody cfg env body)
         compKernelBody cfg env ss
    (If cond then_ else_ _ : ss) ->
      do let cond' = compExp env cond
         iff (unBool cond')
             (compKernelBody cfg env then_
             ,compKernelBody cfg env else_)
         compKernelBody cfg env ss
    (SeqFor _ _ _ _ : _)      -> error "seqfor: Only available at thread-level"
    (ReadIntCSV _ _ _ _ : _)  -> error "Reading input-data is not possible at block level"
    (PrintIntArray _ _ _ : _) -> error "Printing not possible at block level"
    (Benchmark _ _ _ : _)     -> error "Benchmarking not possible at block level"

mkKernelBody :: CompileConfig -> VarEnv -> ILName -> ILExp -> [Stmt a] -> ILKernel ()
mkKernelBody cfg env loopVar loopBound body =
  let ub = compExp env loopBound
  in distrParBlock cfg (unInt ub) (\i ->
       do i' <- lett "ub" (VInt i)
          compKernelBody cfg (Map.insert loopVar i' env) body)
  
distrParBlock :: CompileConfig -> CExp -> (CExp -> ILKernel ()) -> ILKernel ()
distrParBlock cfg (Int32E ub) f =
    do let workgroups = configNumWorkGroups cfg
       let q = fromIntegral ub `div` workgroups
       let r = fromIntegral ub `mod` workgroups
       if q == 1
         then do j <- let_ "j" int32_t (workgroupID `muli` constant q)
                 f j
         else if q > 1
              then for (constant q)
                     (\i ->
                         do j <- let_ "j" int32_t ((workgroupID `muli` constant q) `addi` i)
                            f j)
              else return ()
       if r > 0
         then iff (workgroupID `lti` constant r)
                  (do j <- let_ "wid" int32_t ((constant workgroups `muli` constant q) `addi` workgroupID)
                      f j
                  , return ())
         else return ()
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
forAllBlock cfg (Int32E n) f =
    do let blockSize = configBlockSize cfg
       let q = fromIntegral n `div` blockSize
       let r = fromIntegral n `mod` blockSize
       if q == 1
         then do j <- let_ "j" int32_t localID
                 f j
         else if q > 1
              then for (constant q)
                       (\i -> do j <- let_ "j" int32_t ((i `muli` constant blockSize) `addi` localID)
                                 f j)
              else return ()
       if r > 0
         then iff (localID `lti` constant r)
                 (do j <- let_ "tid" int32_t ((constant q `muli` constant blockSize) `addi` localID)
                     f j
                 , return ())
         else return ()
       syncLocal
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
compHost :: CompileConfig -> ClContext -> VarEnv -> [Stmt a] -> ILHost ()
compHost cfg ctx env stmts =
  case stmts of
    [] -> return ()
    (PrintIntArray size arr _ : ss) ->
      do let n = compExp env size
         let arr' = compExp env arr
         finish ctx
         printArray ctx n arr'
         compHost cfg ctx env ss
    (Benchmark iterations ss0 _ : ss) ->
      do let n = compExp env iterations
         benchmark ctx n (compHost cfg ctx env ss0)
         compHost cfg ctx env ss
    (Declare x ty e _ : ss) ->
      do let v = compExp env e
         v' <- lett (show x) v
         compHost cfg ctx (Map.insert x v' env) ss
    (Distribute _ x e stmts' _ : ss) -> -- use level for something? Only for type check?
      do distribute cfg ctx env x e stmts'
         compHost cfg ctx env ss
    (ParFor _ _ _ _ _ : _) -> error "parfor<grid>: Not supported yet"
    (Synchronize _ : ss) ->
      do finish ctx
         compHost cfg ctx env ss
    (ReadIntCSV x xlen e _ : ss) ->
      do let filename = compExp env e
         (valuesRead, hostPtr) <- readCSVFile int32_t filename
         v <- copyToDevice ctx ILInt (var valuesRead) (var hostPtr)
         compHost cfg ctx (Map.insert xlen (VarInt valuesRead) (Map.insert x v env)) ss
    (Alloc x elemty e _ : ss) ->
      do let size = compExp env e
         v <- allocate ctx elemty size
         compHost cfg ctx (Map.insert x v env) ss
    (Assign x e _ : ss) ->
      do let e' = compExp env e
         assignVar env x e'
         compHost cfg ctx env ss
    (AssignSub x ix e _ : ss) ->
      do let ix' = compExp env ix
             e' = compExp env e
         assignSub env x ix' e'
         compHost cfg ctx env ss
    (While stopCond body _ : ss) ->
      do let v = compExp env stopCond
         whileLoop (unBool v) (compHost cfg ctx env body)
         compHost cfg ctx env ss
    (If cond then_ else_ _ : ss) ->
      do let cond' = compExp env cond
         iff (unBool cond')
             (compHost cfg ctx env then_
             ,compHost cfg ctx env else_)
         compHost cfg ctx env ss
    (SeqFor _ _ _ _ : _) -> error "seqfor: Only available at thread-level"

distribute :: CompileConfig -> ClContext -> VarEnv -> ILName -> ILExp -> [Stmt a] -> ILHost ()
distribute cfg ctx env loopVar loopBound stmts =
  let
    createArgument :: (ILName, ILType) -> ILHost KernelArg
    createArgument (x, ty) =
      do case Map.lookup x env of
           Just (VarInt v) -> return (ArgScalar (convertType ty) (var v))
           Just (VarBool v) -> return (ArgScalar (convertType ty) (var v))
           Just (VarDouble v) -> return (ArgScalar (convertType ty) (var v))
           Just (VarHostBuffer _ buf) -> return (ArgBuffer buf)
           Just (VarKernelArray _ _) -> error "kernel arrays can not occur outside kernels"
           Just (VarString _) -> error "Can not use strings inside kernels"
           Nothing -> error "variable not found"

    callKernel :: String -> Kernel -> ILHost ()
    callKernel kernelName (Kernel i params _) =
      do -- set shared memory
         let smarg = replicate i (ArgSharedMemory (constant (configSharedMemory cfg)))
         -- set input parameters: map over kernel list, lookup allocations in environment
         args <- mapM createArgument params
         let kernelHandle = ClKernel (kernelName, kernelCType)
         invokeKernel ctx kernelHandle (smarg ++ args)
                                       (muli (constant (configBlockSize cfg)) (constant (configNumWorkGroups cfg)))
                                       (constant (configBlockSize cfg))

    profile :: String -> Kernel -> ILHost ()
    profile kernelName (Kernel i params _) =
      do -- set shared memoryp
         let smarg = replicate i (ArgSharedMemory (constant (configSharedMemory cfg)))
         -- set input parameters: map over kernel list, lookup allocations in environment
         args <- mapM createArgument params
         let kernelHandle = ClKernel (kernelName, kernelCType)
         tdiff <- profileKernel ctx kernelHandle (smarg ++ args)
                                                 (muli (constant (configBlockSize cfg)) (constant (configNumWorkGroups cfg)))
                                                 (constant (configBlockSize cfg))
         let psum = (kernelName ++ "_sum_seconds", CDouble)
             counter = (kernelName ++ "_counter", CWord64)
         assign psum (var psum `addi` ((i2d (var tdiff)) `divd` (constant (1000000000.0 :: Double))))
         assign counter (var counter `addi` (constant (1 :: Int)))

  in do kernelName <- newName "kernel"
        k <- mkKernel cfg env kernelName loopVar loopBound stmts
        addKernel kernelName k -- save in monad state, to be able to free it in the end
        if configProfile cfg
          then profile kernelName k
          else callKernel kernelName k

-- create parameter list (newVar)
-- create VarEnv mapping free variables to the parameter names
     -- also add the variable used as loop argument to distrPar
-- call compKernelBody with this new VarEnv
-- add shared memory argument
-- create kernel definition (using distrParBlock)
-- create/build kernel on host
-- call the kernel using the same order of arguments, as in the parameter list.
mkKernel :: CompileConfig -> VarEnv -> String -> ILName -> ILExp -> [Stmt a] -> ILHost Kernel
mkKernel cfg env kernelName loopVar loopBound stmts =
  let params = Set.toList (Set.delete loopVar (freeVars stmts `Set.union` liveInExp loopBound))
      mkArgumentList :: [ILName] -> ILHost [Var]
      mkArgumentList = mapM (hostVarToKernelVar env)

  in do args <- mkArgumentList params
        tyenv <- getsState typeEnv
        let param_tys = map (\k -> maybe (error "not found") id (Map.lookup k tyenv)) params
        let kernelEnv = Map.fromList (zip params args)
        (kernelBody, _, finalKernelState) <- embed (mkKernelBody cfg kernelEnv loopVar loopBound stmts) initKernelState
        let allocs = allocations finalKernelState
            fndef = kernel kernelName (allocs ++ map getVarName args) kernelBody
        return (Kernel (length allocs) (zip params param_tys) fndef)

allocate :: ClContext -> ILType -> Value -> ILHost Var
allocate ctx elemty size =
  do buf <- allocDevice ctx ReadWrite (unInt size) (convertType elemty)
     modifyState (\s -> s { deviceAllocations = Map.insert buf (unInt size, convertType elemty) (deviceAllocations s) })
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
     modifyState (\s -> s { deviceAllocations = Map.insert buf (n, convertType elemty) (deviceAllocations s) })
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

now :: String -> ILHost CExp
now x =
  do v <- eval x uint64_t "now" []
     return (var v)

stderr :: CExp
stderr = definedConst "stderr" (CCustom "File" Nothing)

benchmark :: ClContext -> Value -> ILHost () -> ILHost ()
benchmark ctx (VInt n) body =
  do t0 <- now "t0"
     allocs <- getsState deviceAllocations
     -- warm up run
     modifyState (\s -> s { deviceAllocations = Map.empty })
     body
     finish ctx
     releaseAllDeviceArrays
     finish ctx
     -- detect the allocations done in the kernel
     modifyState (\s -> s { deviceAllocations = Map.empty })
     for n (\_ ->
             do body
                finish ctx
                -- deallocate everything before exiting the loop
                releaseAllDeviceArrays)
     -- allocsAfter <- getsState deviceAllocations
     -- let sizes = map (\(k,cty) -> k `muli` sizeOf cty) (Map.elems allocsAfter)
     -- let totalTransferredBytes = foldl addi (constant (0 :: Int)) sizes
     -- reset allocations
     modifyState (\s -> s { deviceAllocations = allocs })
     t1 <- now "t1"
     let formatString1 = "Benchmark (%i repetitions): %f ms per run\\n"
         -- formatString2 = "Throughput (%i repetitions): %.4f GiB/s, data transferred per run: %.4f MiB\\n"
     milliseconds_per_iter <- let_ "ms" CDouble (divd (i2d (subi t1 t0)) (i2d n))
     -- seconds <- let_ "seconds" CDouble (milliseconds_per_iter `divd` (constant (1000.0 :: Double)))
     -- mebibytes <- let_ "mib" CDouble (totalTransferredBytes `divd` (constant (1024.0*1024.0 :: Double)))
     -- gebibytes <- let_ "gib" CDouble (mebibytes `divd` (constant (1024.0 :: Double)))
     -- throughput <- let_ "throughput" CDouble (gebibytes `divd` seconds)
     exec void_t "fprintf" [stderr, string formatString1, n, milliseconds_per_iter]
     -- exec void_t "fprintf" [stderr, string formatString2, n, throughput, mebibytes]
benchmark _ _ _ = error "Benchmark expects int as first argument (number of iterations)."

---------------------
-- Compile program --
---------------------
initializeCounter :: String -> CGen () ()
initializeCounter kernelName =
  do let psum = (kernelName ++ "_sum_seconds", CDouble)
         counter = (kernelName ++ "_counter", CInt32)
     addStmt (Decl psum (constant (0 :: Double)) ())
     addStmt (Decl counter (constant (0 :: Int)) ())

printCounter :: String -> CGen () ()
printCounter kernelName =
  do let formatString = "Kernel %s timing: %f ms per execution (%d executions)\\n"
         psum = var (kernelName ++ "_sum_seconds", CDouble)
         counter = var (kernelName ++ "_counter", CInt32)
     milliseconds <- let_ "ms" CDouble ((psum `muli` (constant (1000.0 :: Double))) `divd` (i2d counter))
     exec void_t "fprintf" [stderr, string formatString, string kernelName, milliseconds, counter]

compProgram :: CompileConfig -> TypeEnv -> [Stmt a] -> CGen () KernelMap
compProgram cfg env program =
  -- compile body
  let ctxVar = ClContext ("ctx", contextCType)
      pVar = ClProgram ("program", programCType)
      body = compHost cfg ctxVar Map.empty program
      (stmts, _, _, finalState) = runCGen (initHostState env) body
      -- collect the generated kernels
      kernels' = kernels finalState
      allocs = deviceAllocations finalState
  in
    do initializeContext ctxVar (configVerbosity cfg)
       exec void_t "initializeTimer" []
       if configProfile cfg
          then mapM_ initializeCounter (Map.keys kernels')
          else return ()
       buildProgram ctxVar pVar (configKernelsFilename cfg)
       kernelHandles <- mapM (createKernel pVar) (Map.keys kernels')
       addStmts stmts
       if configProfile cfg
          then mapM_ printCounter (Map.keys kernels')
          else return ()
       mapM_ releaseDeviceData (Map.keys allocs)
       mapM_ releaseKernel kernelHandles
       releaseProgram pVar
       releaseContext ctxVar
       return kernels'

releaseAllDeviceArrays :: ILHost ()
releaseAllDeviceArrays =
  do allocs <- getsState deviceAllocations
     mapM_ releaseDeviceData (Map.keys allocs)

prettyKernels :: KernelMap -> String
prettyKernels kernelMap = pretty (map (\(Kernel _ _ s) -> s) (Map.elems kernelMap))

createMain :: Statements -> String
createMain body = pretty (includes ++ [function int32_t [] "main" body])

includes :: [TopLevel]
includes = [includeSys "fcl.h",
            includeSys "stdio.h",
            includeSys "sys/time.h",
            includeSys "mcl.h"]
                  
codeGen :: CompileConfig -> TypeEnv -> ILProgram a -> (String, String)
codeGen cfg env program =
 let (mainBody, _, kernels') = evalCGen () (compProgram cfg env program)
 in (createMain mainBody,
     prettyKernels kernels')
