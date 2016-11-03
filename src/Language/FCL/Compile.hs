module Language.FCL.Compile
  (compileKernel, compileKernels)
where

import qualified Data.Map as Map
import Control.Monad (liftM)

import Language.FCL.SourceRegion
import Language.FCL.Syntax
import CGen
--import Language.CGen.Syntax (IExp(IntE))

warpSize :: CExp
warpSize = constant (32 :: Int)

type IL a = CGen CompileState a

data CompileState =
  CompileState { kernelConfig :: KernelConfig
               , allocPtrOffset :: CExp
               , sharedMemPointer :: VarName
               }

initializeState :: KernelConfig -> CompileState
initializeState cfg =
  CompileState { kernelConfig = cfg
               , allocPtrOffset = constant (0 :: Int)
               , sharedMemPointer = error "Shared memory not be initialized!" -- TODO, not nice.
               } 

allocate :: Type -> CExp -> IL VarName
allocate ty n =
  do offset <- getsState allocPtrOffset
     sbase <- getsState sharedMemPointer
     let cty = convertType ty
     case sizeOf cty of
       Just bsize ->
          do let aty = pointer_t [attrLocal] cty
             v <- letVar "arr" aty (cast aty (var sbase `addPtr` offset))
             let bytes = n `muli` (constant bsize)
             modifyState (\s -> s { allocPtrOffset = offset `addi` bytes })
             return v
       Nothing -> error "unknown size of ty (in allocate)"

type Writer a = a -> CExp -> IL ()

data Array a = ArrPull CExp Type (CExp -> a)
             | ArrPush CExp Type (Writer a -> IL ())

mapArray :: (a -> b) -> Type -> Array a -> Array b
mapArray f outType (ArrPull len _ g) =
  ArrPull len outType (f . g)
mapArray f outType (ArrPush len _ g) =
  ArrPush len outType (\w -> g (\e ix -> do let v = f e
                                            w v ix))

size :: Array a -> CExp
size (ArrPull len _ _)       = len
size (ArrPush len _ _)       = len

baseType :: Array a -> Type
baseType (ArrPull _ (PullArrayT bty) _)       = bty
baseType (ArrPull _ bty _)       = bty
baseType (ArrPush _ bty _)       = bty

forceTo :: Writer Tagged -> Array Tagged -> IL ()
forceTo writer (ArrPush _ _ m) = m writer
forceTo _ (ArrPull _ _ _) = error "blah"

createPull :: VarName -> Type -> CExp -> Array Tagged
createPull name ty n = ArrPull n ty (\i -> TagInt (name ! i))
                                         -- TODO ^ This should pack
                                         -- according elem type (ty),
                                         -- currently only supports
                                         -- integers
data Tagged = TagInt CExp
            | TagBool CExp
            | TagDouble CExp
            | TagArray (Array Tagged)
            | TagFn (Tagged -> Tagged)
            | TagPair Tagged Tagged
            | TagProgram (IL Tagged)

instance Show (Array a) where
  show (ArrPull _ _ _) = "<<pull-array>>"
  show (ArrPush _ _ _) = "<<push-array>>"

instance Show Tagged where
  show (TagInt e) = "TagInt(" ++ show e ++ ")"
  show (TagDouble e) = "TagDouble(" ++ show e ++ ")"
  show (TagBool e) = "TagBool(" ++ show e ++ ")"
  show (TagArray e) = "TagArray: " ++ show e
  show (TagFn _) = "TagFn"
  show (TagPair e0 e1) = "TagPair(" ++ show e0 ++ ", " ++ show e1 ++ ")"
  show (TagProgram _) = "TagProgram"

type VarEnv = Map.Map Name Tagged

emptyEnv :: VarEnv
emptyEnv = Map.empty

compileKernels :: Int -> [Definition Type] -> [TopLevel]
compileKernels optIterations = map (compileKernel optIterations)

compileKernel :: Int -> Definition Type -> TopLevel
compileKernel optIterations def =
  let e = defBody def
      kernel_name = defVar def
      kernel_body = do
         sbase <- addParam "sbase" (pointer_t [attrLocal] uint8_t)
         modifyState (\s -> s { sharedMemPointer = sbase })
         compile 0 emptyEnv (typeOf e) e
      initialState = initializeState (defKernelConfig def)
  in fst (generateKernel initialState optIterations kernel_name kernel_body)

addArgument :: Type -> IL Tagged
addArgument (PullArrayT bty) =
  do arrVar <- addParam "arrInput" (pointer_t [attrGlobal] (convertType bty))
     lenVar <- addParam "lenInput" int32_t
     return (TagArray (createPull arrVar bty (var lenVar)))
addArgument (PushArrayT _ _) = error "Push arrays can not be kernel arguments"
addArgument ty =
  do v <- addParam "input" (convertType ty)
     return (tagExp ty (var v))

compile :: Int -> VarEnv -> Type -> Exp Type -> IL ()
compile i env (ty :> ty') e =
  do taggedExp <- addArgument ty
     let varName = "argument" ++ show i
     compile (i+1) (Map.insert varName taggedExp env) ty' (App e (Var varName ty Missing))
compile _ env _ e =
  case compBody env e of
    TagProgram body -> do
      kernelBody <- body
      case kernelBody of
        TagInt i -> do
          varOut <- addParam "output" int32_t
          assign varOut i
        TagBool b -> do
          varOut <- addParam "output" bool_t
          assign varOut b
        TagDouble d -> do
          varOut <- addParam "output" double_t
          assign varOut d
        TagFn _ -> error "compileFun: Cannot return functions"
        TagPair _ _ -> error "compileFun: TODO return pairs" -- TODO
        TagArray arr -> do
          varOut <- addParam "arrOutput" (pointer_t [attrGlobal] (convertType (baseType arr)))
          let writer tv i =
                case tv of
                  TagInt v -> assignArray varOut v i
                  t -> error (show t)
          forceTo writer arr
        TagProgram _ -> error "TODO"
    v -> error ("Not a program, but: " ++ show v)

tagExp :: Type -> CExp -> Tagged
tagExp IntT e    = TagInt e
tagExp DoubleT e = TagDouble e
tagExp BoolT e   = TagDouble e
tagExp t _       = error ("tagExp: " ++ show t)

convertType :: Type -> CType
convertType IntT              = int32_t
convertType DoubleT           = double_t
convertType BoolT             = bool_t
convertType (PullArrayT ty)   = pointer_t [] (convertType ty)
convertType (PushArrayT _ ty) = pointer_t [] (convertType ty)
convertType (_ :> _)          = error "convertType: functions can not be used as arguments to kernels or occur in arrays"
convertType (_ :-> _)         = error "convertType: functions can not be used as arguments to kernels or occur in arrays"
convertType (_ :*: _)         = error "convertType: tuples not yet support in argument or results from kernels (on the TODO!)"
convertType (VarT _)          = error "convertType: All type variables should have been resolved by now"
convertType (ProgramT _ _)    = error "convertType: cannot convert Program type"

lets :: String -> Tagged -> IL Tagged
lets name s =
  case s of
    TagInt x -> liftM TagInt (let_ name int32_t x)
    TagBool x -> liftM TagBool (let_ name bool_t x)
    TagDouble x -> liftM TagDouble (let_ name double_t x)
    TagFn _ -> return s
    TagProgram _ -> return s
    TagPair x y -> do x' <- lets name x
                      y' <- lets name y
                      return (TagPair x' y')
    TagArray (ArrPull len bty idx) ->
      do (n',_) <- letsVar "len" (TagInt len)
         return (TagArray (ArrPull (var n') bty idx))
    TagArray (ArrPush len bty wf) ->
      do (n',_) <- letsVar "len" (TagInt len)
         return (TagArray (ArrPush (var n') bty wf))

letsVar :: String -> Tagged -> IL (VarName, Tagged)
letsVar name s =
  case s of
    TagInt x -> do var0 <- letVar name int32_t x
                   return (var0, TagInt (var var0))
    TagBool x -> do var0 <- letVar name bool_t x
                    return (var0, TagBool (var var0))
    TagDouble x -> do var0 <- letVar name double_t x
                      return (var0, TagDouble (var var0))
    TagFn _ -> error "letsVar TagFn" -- TODO, Impossible - what to do? Just err?
    TagPair _ _ -> error "letsVar TagPair" -- TODO
    TagArray _ -> error "letsVar TagArray" -- TODO
    TagProgram _ -> error "letsVar TagProgram"

compBody :: Map.Map Name Tagged -> Exp Type -> Tagged
compBody _ (IntScalar i _)    = TagInt (constant i)
compBody _ (DoubleScalar d _) = TagDouble (constant d)
compBody _ (BoolScalar b _)   = TagBool (constant b)
compBody env (App e0 e1) =
  case compBody env e0 of
    TagFn f -> f (compBody env e1)
    _ -> error "Unexpected value at function position in application"
compBody env (Pair e0 e1 _) =
  (TagPair (compBody env e0) (compBody env e1))
compBody env (Proj1E e reg) =
  case compBody env e of
    TagPair v0 _ -> v0
    _ -> error (show reg ++ ": fst expects a pair as argument")
compBody env (Proj2E e reg) =
  case compBody env e of
    TagPair _ v1 -> v1
    _ -> error (show reg ++ ": snd expects a pair as argument")
compBody env (Var x _ reg) =
  case Map.lookup x env of
    Just v -> v
    Nothing -> error (show reg ++ ": Variable not defined: " ++ x)
compBody env (Lamb x _ e _ _) =
  TagFn (\v -> compBody (Map.insert x v env) e)
compBody env (LambLvl _ e _ _) = compBody env e
compBody env (AppLvl e _) = compBody env e
compBody env (Let x e0 e1 _ _) =
  compBody (Map.insert x (compBody env e0) env) e1
compBody env (UnOp op e0 reg) =
  compileUnOp op (compBody env e0) reg
compBody env (BinOp op e0 e1 reg) =
  compileBinOp op (compBody env e0) (compBody env e1) reg
compBody env (Cond e0 e1 e2 _ reg) =
  case compBody env e0 of
    (TagBool b0) -> case (compBody env e1, compBody env e2) of
                     (TagInt i1, TagInt i2) -> TagInt (if_ b0 i1 i2)
                     (TagBool b1, TagBool b2) -> TagBool (if_ b0 b1 b2)
                     (TagArray _, TagArray _) -> error (show reg ++ ": TODO: not possible yet")
                     (TagFn _, TagFn _) -> error (show reg ++ ": TODO: yet to be implemented")
                     (_,_) -> error (show reg ++ ": branches are differing")
    _ -> error "Expecting boolean expression as conditional argument in branch"
compBody env (GeneratePull e0 e1 reg) =
  let (_ :> ty1) = typeOf e1
  in case (compBody env e0, compBody env e1) of
      (TagInt e0', TagFn f) -> (TagArray (ArrPull e0' ty1 (\i -> f (TagInt i))))
      _ -> error (show reg ++ ": generate expects integer expression as first argument and function as second argument")
compBody env (MapPull e0 e1 reg) = map_ env e0 e1 reg
compBody env (MapPush e0 e1 reg) = map_ env e0 e1 reg
compBody env (LengthPull e0 reg) = length_ env e0 reg
compBody env (LengthPush e0 reg) = length_ env e0 reg
compBody env (Index e0 e1 reg) =
  case (compBody env e0, compBody env e1) of
    (TagArray (ArrPull _ _ idx), TagInt i) -> idx i
    _ -> error (show reg ++ ": Index expects array and integer argument")
compBody env (Force e0 reg) =
  case compBody env e0 of
    TagArray arr -> TagProgram (TagArray <$> force arr)
    _ -> error (show reg ++ ": force expects array as argument")
compBody env (Push lvl e0 _) =
  case compBody env e0 of
    TagArray arr -> TagArray (push lvl arr)
    _ -> error "pull expects array"
compBody env (While e0 e1 e2 _) =
  whileArray (compBody env e0) (compBody env e1) (compBody env e2)
compBody env (WhileSeq e0 e1 e2 _) =
  whileSeq (compBody env e0) (compBody env e1) (compBody env e2)
compBody env (Interleave i ixf e0 reg) =
  let PullArrayT (ProgramT _ (PushArrayT lvl _)) = typeOf e0
  in
     case (compBody env i, compBody env ixf, compBody env e0) of
       (TagInt rn, TagFn ixf', TagArray arr) -> TagProgram (return (TagArray (interleave lvl rn ixf' arr)))
       _ -> error (show reg ++ " Interleave should be given an integer, a function and an array.")
compBody env (Return _ e _)   =
  TagProgram (return (compBody env e))
compBody env (Bind e0 e1 _)   =
  case (compBody env e0, compBody env e1) of
    (TagProgram v0, TagFn f) ->
      TagProgram (do m0 <- v0
                     case f m0 of
                       TagProgram m1 -> m1
                       _ -> error "TODO")
    _ -> error "TODO"
-- compBody _ (BlockSize _)   = do
--   s <- getState
--   return (TagInt (constant (configBlockSize (kernelConfig s))))

length_ :: Map.Map Name Tagged -> Exp Type -> Region -> Tagged
length_ env e0 reg = do
  case compBody env e0 of
    TagArray arr -> TagInt (size arr)
    e -> error (show reg ++ ": Length expects array as argument got " ++ show e)

map_ :: Map.Map Name Tagged -> Exp Type -> Exp Type -> Region -> Tagged
map_ env e0 e1 reg =
  let (_ :> outType) = typeOf e0
  in case (compBody env e0, compBody env e1) of
       (TagFn f, TagArray arr) -> TagArray (mapArray f outType arr)
       (f,e) -> error (concat [show reg,
                           ": ",
                           "Map expects function as first argument and",
                           "an array as second argument, got:\n    ",
                           show f,
                           "\nand\n    ",
                           show e])

force :: Array Tagged -> IL (Array Tagged)
force (ArrPull _ _ _) = error ("force: forcing a pull-array should raise type error." ++
                               "Needs iteration scheme before it can be forced.")
force arr = do
  let len = size arr                     -- calculate size of complete nested array structure
  name <- allocate (baseType arr) len    -- allocate shared memory
  let writer tv i =                      -- creater writer function (right now: only integer arrays supported!)
        case tv of
          TagInt v -> assignArray name v i
          e -> error (show e)
  forceTo writer arr                     -- recursively generate loops for each layer
  return (createPull name (baseType arr) len)

push :: Level -> Array Tagged -> Array Tagged
push lvl (ArrPull len bty idx) =
  ArrPush len bty
          (\wf -> forAll lvl len
                    (\i -> wf (idx i) i))
push _ (ArrPush _ _ _) = error "force: push can only be applied to pull-arrays, how did this force appear?"

interleave :: Level -> CExp -> (Tagged -> Tagged) -> Array Tagged -> Array Tagged
interleave lvl n f (ArrPull len (ProgramT _ (PushArrayT _ bty)) idx) =
  ArrPush (len `muli` n) bty (\wf -> 
    distrPar lvl len $ \bix -> do
      let arrp = idx bix
      let writer' a ix = wf a (unInt (f (TagPair (TagInt bix) (TagInt ix))))
      case arrp of
        TagProgram m ->
          do arrp' <- m
             case arrp' of
               TagArray arrp'' -> forceTo writer' arrp''
               _ -> error "expected pull-array of programs of push-arrays as argument to Concat"
        _ -> error "Interleave should be applied to an array of arrays!")
interleave _ _ _ t = error ("interleave only accepts pull-arrays of push-arrays. Got: " ++ show t)

whileArray :: Tagged -> Tagged -> Tagged -> Tagged
whileArray (TagFn cond) (TagFn step) (TagArray arr@(ArrPush _ _ _)) =
  TagProgram $
    do -- Declare array
       len <- lets "len" (TagInt (size arr))
       var_array <- allocate (baseType arr) (unInt len)
       (var_len,_) <- letsVar "arraySize" len

       let writer tv i =
             case tv of
               TagInt v -> assignArray var_array v i
               e -> error (show e)

       forceTo writer arr

       let vararr = TagArray (createPull var_array (baseType arr) (var var_len))

       (var_cond,_) <- letsVar "cond" (cond vararr) -- stop condition
       whileLoop (var var_cond) $
         do let arr' = step vararr
            len' <- lets "len" (TagInt (size (unArray arr')))
            let arr'' = case unArray arr' of
                          ArrPush _ ty wf -> ArrPush (unInt len') ty wf
                          _ -> error "step-function in while loop didn't return a pull array"
            forceTo writer arr''
            assign var_len (unInt len')
            assign var_cond (unBool (cond (TagArray arr'')))
       return vararr
whileArray (TagFn _) (TagFn _) _ = error "third argument to while should be a push-array"
whileArray _ _ _ = error "first two arguments to while should be conditional and step function, respectively"

whileSeq :: Tagged -> Tagged -> Tagged -> Tagged
whileSeq (TagFn cond) (TagFn step) v =
  TagProgram $
    do (var0, var0tagged) <- letsVar "loopVar" v
       case cond var0tagged of
         TagBool b -> whileLoop b
                       (case step var0tagged of
                          TagInt x -> assign var0 x
                          TagBool x -> assign var0 x
                          TagDouble x -> assign var0 x
                          _ -> error "unsupported type in whileSeq")
         _ -> error "Conditional in 'while- should be boolean typed"  
       return v
whileSeq _ _ _ = error "incompatible arguments for whileSeq"

distrPar :: Level -> CExp -> (CExp -> IL ()) -> IL ()
distrPar (Step (Step Zero)) ub' f =
    do ub <- let_ "ub" int32_t ub'
       q <- let_ "blocksQ" int32_t (ub `divi` numWorkgroups)
       for q (\i -> do
                 j <- let_ "j" int32_t ((workgroupID `muli` q) `addi` i)
                 f j)
       iff (workgroupID `lti` (ub `modi` numWorkgroups))
           (do j <- let_ "j" int32_t ((numWorkgroups `muli` q) `addi` workgroupID)
               f j
           , return ())
distrPar (Step Zero) ub' f = -- warp level
    do ub <- let_ "ub" int32_t ub'
       cfg <- kernelConfig <$> getState
       numWarps <- let_ "numWarps" int32_t (constant (configBlockSize cfg `div` configWarpSize cfg))
       warpsQ <- let_ "warpsQ" int32_t (ub `divi` numWarps)
       warpsR <- let_ "warpsR" int32_t (ub `modi` numWarps)
       lwid <- let_ "lwid" int32_t (localID `divi` warpSize)
       for warpsQ
         (\i -> do warpID <- let_ "warpID" int32_t ((lwid `muli` warpsQ) `addi` i)
                   f warpID)
       iff (lwid `lti` warpsR)
           (do warpID <- let_ "warpID" int32_t ((numWarps `muli` warpsQ) `addi` lwid)
               f warpID
           , return ())
distrPar lvl _ _ = error ("Unsupported level in distrPar: " ++ show lvl)

forAll :: Level -> CExp -> (CExp -> IL ()) -> IL ()
forAll (Step (Step Zero)) ub' f =
    do ub <- let_ "ub" int32_t ub'
       s <- getState
       let blockSize = constant (configBlockSize (kernelConfig s))
       q <- let_ "q" int32_t (ub `divi` blockSize)
       for q (\i -> do v <- let_ "j" int32_t ((i `muli` blockSize) `addi` localID)
                       f v)
       iff (localID `lti` (ub `modi` blockSize))
         (do v <- let_ "j" int32_t ((q `muli` blockSize) `addi` localID)
             f v
         , return ())
       syncLocal
forAll (Step Zero) ub' f =
  do ub <- let_ "ub" int32_t ub'
     q <- let_ "q" int32_t (ub `divi` warpSize)
     r <- let_ "r" int32_t (ub `modi` warpSize)
     wid <- let_ "wid" int32_t (localID `modi` warpSize)
     for q (\i -> do warpID <- let_ "warpID" int32_t ((i `muli` warpSize) `addi` wid)
                     f warpID)
     iff (wid `lti` r)
       (do warpID <- let_ "warpID" int32_t ((q `muli` warpSize) `addi` wid)
           f warpID
       , return ())
     syncLocal
forAll lvl _ _ = error ("Unsupported level in forAll: " ++ show lvl)

compileBinOp :: BinOp -> Tagged -> Tagged -> Region -> Tagged
compileBinOp AddI (TagInt i0) (TagInt i1) _ = TagInt (addi i0 i1)
compileBinOp SubI (TagInt i0) (TagInt i1) _ = TagInt (subi i0 i1)
compileBinOp MulI (TagInt i0) (TagInt i1) _ = TagInt (muli i0 i1)
compileBinOp DivI (TagInt i0) (TagInt i1) _ = TagInt (divi i0 i1)
compileBinOp ModI (TagInt i0) (TagInt i1) _ = TagInt (modi i0 i1)
compileBinOp MinI (TagInt i0) (TagInt i1) _ = TagInt (mini i0 i1)
compileBinOp EqI  (TagInt i0) (TagInt i1) _ = TagBool (eqi i0 i1)
compileBinOp NeqI (TagInt i0) (TagInt i1) _ = TagBool (neqi i0 i1)
compileBinOp op _ _ reg =
  error (concat [show reg,
                 ": ",
                 "Unexpected arguments to unary operator ",
                 show op,
                 ", expecting integer expression."])

compileUnOp :: UnOp -> Tagged -> Region -> Tagged
compileUnOp AbsI  (TagInt i0) _ = TagInt (absi i0)
compileUnOp SignI (TagInt i0) _ = TagInt (signi i0)
compileUnOp op _ reg =
  error (concat [show reg,
                 ": ",
                 "Unexpected arguments to unary operator ",
                 show op,
                 ", expecting integer expression."])

unInt :: Tagged -> CExp
unInt (TagInt i) = i
unInt _          = error "expected int"

unBool :: Tagged -> CExp
unBool (TagBool e) = e
unBool _           = error "expected bool"

unArray :: Tagged -> Array Tagged
unArray (TagArray e) = e
unArray _            = error "expected array"
