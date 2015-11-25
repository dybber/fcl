module Language.ObsidianLight.Compile where

import qualified Data.Map as Map
import Control.Monad (liftM)

import Language.ObsidianLight.Syntax
import Language.GPUIL

type PushFn a = ((a -> CExp -> Program ()) -> Program ())

data Idx a = Pull (CExp -> Program a)
           | Push (PushFn a)

data Array a = Array { arrayLen :: CExp
                     , arrayElemType :: CType
                     , arrayLevel :: Level
                     , arrayFun :: Idx a
                     }
  deriving Show

data Tagged = TagInt CExp
            | TagBool CExp
            | TagDouble CExp
            | TagArray (Array Tagged)
            | TagFn (Tagged -> Program Tagged)
            | TagPair Tagged Tagged

instance Show (Idx a) where
  show (Pull _) = "<pull array>"
  show (Push _) = "<push array>"

instance Show Tagged where
  show (TagInt e) = "TagInt(" ++ show e ++ ")"
  show (TagDouble e) = "TagDouble(" ++ show e ++ ")"
  show (TagBool e) = "TagBool(" ++ show e ++ ")"
  show (TagArray e) = "TagArray: " ++ show e
  show (TagFn _) = "TagFn"
  show (TagPair e0 e1) = "TagPair(" ++ show e0 ++ ", " ++ show e1 ++ ")"

type VarEnv = Map.Map Variable Tagged

emptyEnv :: VarEnv
emptyEnv = Map.empty

compile :: String -> Exp Type -> Kernel NoType
compile name e = generateKernel name (compileFun emptyEnv e >> return ())

-- Convert pull arrays to push arrays
push :: Array a -> Array a
push arr =
  case arrayFun arr of
    Pull idx -> arr { arrayFun = Push (\writer ->
                                          forAll
                                            (arrayLevel arr)
                                            (arrayLen arr)
                                            (\i -> do value <- idx i
                                                      writer value i))
                    }
    Push _ -> arr

compileFun :: Map.Map Variable Tagged -> Exp Type -> Program ()
compileFun env (Lamb x (ArrayT lvl ty) e _) = do
  arrVar <- addParam "arrInput" (pointer [attrGlobal] (convertType ty))
  lenVar <- addParam "lenInput" int
  let taggedExp = tagArray ty lvl (var lenVar) arrVar
  compileFun (Map.insert x taggedExp env) e
compileFun env (Lamb x ty e _) = do
  v <- addParam "input" (convertType ty)
  compileFun (Map.insert x (tagExp ty (var v)) env) e
compileFun env e = do
  body <- compBody env e
  case body of
    TagInt i -> do
      varOut <- addParam "output" int
      assign varOut i
    TagBool b -> do
      varOut <- addParam "output" bool
      assign varOut b
    TagDouble d -> do
      varOut <- addParam "output" double
      assign varOut d
    TagFn _ -> error "compileFun: Cannot return functions"
    TagPair _ _ -> error "compileFun: TODO return pairs" -- TODO
    TagArray (Array{arrayElemType = CPtr _ _}) -> 
      error "compileFun: cannot return array of arrays"
    TagArray arr -> do
      varOut <- addParam "arrOutput" (pointer [attrGlobal] (arrayElemType arr))
      varLen <- addParam "lenOutput" int
      let f = getPushFn (push arr)
      f (\(TagInt i) -> assignArray varOut i)
      assign varLen (arrayLen arr)

tagArray :: Type -> Level -> CExp -> VarName -> Tagged
tagArray (ArrayT _ _) _ _ _ = error "Kernels can not accept arrays of arrays"
tagArray (_ :> _) _ _ _     = error "Kernels can not accept arrays of functions"
tagArray (_ :*: _) _ _ _    = error "Kernels can not accept arrays of tuples"
tagArray ty lvl len v =
  TagArray $
    Array { arrayElemType = convertType ty
          , arrayLen = len
          , arrayLevel = lvl
          , arrayFun = Pull (\ix -> return (TagInt (v ! ix)))
          }

tagExp :: Type -> CExp -> Tagged
tagExp IntT e = TagInt e
tagExp DoubleT e = TagDouble e
tagExp BoolT e = TagDouble e
tagExp _ _ = error "tagExp"

convertType :: Type -> CType
convertType IntT = int
convertType DoubleT = double
convertType BoolT = bool
convertType (ArrayT _ ty) = pointer [] (convertType ty)
convertType (_ :> _) = error "convertType: functions can not be used as arguments to kernels or occur in arrays"
convertType (_ :*: _) = error "convertType: tuples not yet support in argument or results from kernels (on the TODO!)"

compBody :: Map.Map Variable Tagged -> Exp Type -> Program Tagged
compBody _ (IntScalar i) = return (TagInt (constant i))
compBody _ (DoubleScalar d) = return (TagDouble (constant d))
compBody _ (BoolScalar b) = return (TagBool (constant b))
compBody env (App e0 e1) = do
  v0 <- compBody env e0
  case v0 of
    TagFn f -> f =<< compBody env e1
    _ -> error "Unexpected value at function position in application"
compBody env (Pair e0 e1) = do
  v0 <- compBody env e0
  v1 <- compBody env e1
  return (TagPair v0 v1)
compBody env (Proj1E e) = do
  v <- compBody env e
  case v of
    TagPair v0 _ -> return v0
    _ -> error "Projection expects Pair as argument"
compBody env (Proj2E e) = do
  v <- compBody env e
  case v of
    TagPair _ v1 -> return v1
    _ -> error "Projection expects Pair as argument"
compBody env (Var x _) =
  case Map.lookup x env of
    Just v -> return v
    Nothing -> error "Variable not defined"
compBody env (Lamb x _ e _) =
  return . TagFn $ \v ->
    do v' <- lets "v" v
       compBody (Map.insert x v' env) e
compBody env (Let x e0 e1 _) = do
  v0 <- compBody env e0
  x0 <- lets x v0
  compBody (Map.insert x x0 env) e1
compBody env (UnOp op e0) = do
  v0 <- compBody env e0
  return (compileUnOp op v0)
compBody env (BinOp op e0 e1) = do
  v0 <- compBody env e0
  v1 <- compBody env e1
  return (compileBinOp op v0 v1)
compBody env (Cond e0 e1 e2 _) = do
  v0 <- compBody env e0
  v1 <- compBody env e1
  v2 <- compBody env e2
  case v0 of
    (TagBool b0) -> case (v1, v2) of
                     (TagInt i1, TagInt i2) -> return . TagInt $ if_ b0 i1 i2
                     (TagBool b1, TagBool b2) -> return . TagBool $ if_ b0 b1 b2
                     (TagArray _, TagArray _) -> error "TODO: not possible yet"
                     (TagFn _, TagFn _) -> error "TODO: yet to be implemented"
                     (_,_) -> error "branches are differing"
    _ -> error "Expecting boolean expression as conditional argument in branch"
compBody env (Generate lvl e0 e1) = do
  v0 <- compBody env e0
  v1 <- compBody env e1
  case (v0, v1) of
    (TagInt e0', TagFn f) ->
      return . TagArray $ Array { arrayElemType = int -- TODO, when type inference is done,
                                                      -- put something sensible here
                                , arrayLen = e0'
                                , arrayLevel = lvl
                                , arrayFun = Pull (\i -> f (TagInt i))
                                }
    _ -> error "Generate expects integer expression as first argument and function as second argument"
compBody env (Map e0 e1) = do
  f' <- compBody env e0
  e' <- compBody env e1
  case (f', e') of
    (TagFn f, TagArray (Array n ty lvl idx)) ->
      case idx of
        Pull g -> return $ TagArray $
                    Array { arrayElemType = ty -- TODO this seems wrong, find the correct return type
                          , arrayLen = n
                          , arrayLevel = lvl
                          , arrayFun = Pull (\x -> g x >>= f)
                          }
        Push g -> return . TagArray $
                    Array { arrayElemType = ty -- TODO this seems wrong
                          , arrayLen = n
                          , arrayLevel = lvl
                          , arrayFun = Push (\writer -> g (\e ix -> do v <- f e
                                                                       writer v ix))
                          }
    _ -> error $ concat ["Map expects function as first argument and",
                         "pull array as second argument, got:\n    ",
                         show f',
                         "\nand\n    ",
                         show e']
compBody env (Length e0) = do
  v <- compBody env e0
  case v of
    TagArray arr -> return $ TagInt (arrayLen arr)
    e -> error $ "Length expects array as argument got " ++ show e
compBody env (Index e0 e1) = do
  v0 <- compBody env e0
  v1 <- compBody env e1
  case (v0, v1) of
    (TagArray arr, TagInt i) ->
      case arrayFun arr of
        Pull idx -> idx i
        Push _ -> do farr <- unsafeWrite arr
                     case arrayFun farr of
                          Pull idx -> idx i
                          Push _ -> error "Impossible"
    _ -> error "Index expects array and integer argument"
compBody env (ForceLocal e0) = do
  v0 <- compBody env e0
  case v0 of
    TagArray arr -> liftM TagArray (unsafeWrite arr)
    _ -> error "ComputeLocal expects array as argument"
compBody env (Fixpoint e0 e1 e2) = do
  v0 <- compBody env e0
  v1 <- compBody env e1
  v2 <- compBody env e2
  fixpoint v0 v1 v2
compBody env (Concat e0) = do
  v0 <- compBody env e0
  case v0 of
    TagArray arr ->
      case arrayFun arr of
        Push _ -> error "concat only works when the outer function is a pull array (TODO!)"
        Pull idx -> do
          arr0 <- (idx (constant (0 :: Int)))
          -- let arr0 = evalProgram (idx (constant (0 :: Int))) -- TODO: this is bad! Maybe shapes in outermost array?
          let n = arrayLen arr
          let rn = case arr0 of
                     TagArray (Array {arrayLen = l}) -> l
                     _ -> error "Concat does not work with empty arrays of arrays"
          return . TagArray $ Array { arrayElemType = arrayElemType arr
                                    , arrayLen = n `muli` rn
                                    , arrayLevel = arrayLevel arr
                                    , arrayFun = 
                                      Push (\writer -> distrPar Block n $ \bix -> do
                                                arrp <- idx bix
                                                let writer' a ix = writer a ((bix `muli` rn) `addi` ix)
                                                case arrp of
                                                  TagArray arrp' ->
                                                    case push arrp' of
                                                      (Array {arrayFun = Push p }) -> p writer'
                                                      _ -> error "Concat only works with inner-arrays of type push"
                                                  _ -> error "Should not happen")
                                    }
    _ -> error "Concat should be given an array as first argument"

lets :: String -> Tagged -> Program Tagged
lets name s =
  case s of
    TagInt x -> liftM TagInt (let_ name int x)
    TagBool x -> liftM TagBool (let_ name bool x)
    TagDouble x -> liftM TagDouble (let_ name double x)
    TagFn _ -> return s
    TagPair x y -> do x' <- lets name x
                      y' <- lets name y
                      return (TagPair x' y')
    TagArray x -> do (n',_) <- letsVar "len" (TagInt (arrayLen x))
                     return (TagArray x {arrayLen = var n'}) -- TODO: materialize??

letsVar :: String -> Tagged -> Program (VarName, Tagged)
letsVar name s =
  case s of
    TagInt x -> do var0 <- letVar name int x
                   return (var0, TagInt (var var0))
    TagBool x -> do var0 <- letVar name int x
                    return (var0, TagBool (var var0))
    TagDouble x -> do var0 <- letVar name int x
                      return (var0, TagDouble (var var0))
    TagFn _ -> error "letsVar TagFn" -- TODO, Impossible - what to do? Just err?
    TagPair _ _ -> error "letsVar TagPair" -- TODO
    TagArray _ -> error "letsVar TagArray" -- TODO

getPushFn :: Array a -> PushFn a
getPushFn (Array {arrayFun = Push f }) = f
getPushFn _ = error ""

unBool :: Tagged -> CExp
unBool (TagBool e) = e
unBool _ = error "expected bool"
unArray :: Tagged -> Array Tagged
unArray (TagArray e) = e
unArray _ = error "expected array"

fixpoint :: Tagged -> Tagged -> Tagged -> Program Tagged
fixpoint (TagFn cond) (TagFn step) (TagArray arr) =
  do -- Declare array
     var_array <- allocate (arrayElemType arr) (arrayLen arr)
     (var_len,_) <- letsVar "arraySize" (TagInt (arrayLen arr))

     -- Materialize before loop
     let f = getPushFn (push arr)
     f (\(TagInt i) -> assignArray var_array i)
     syncLocal

     -- Loop using the same array
     let vararr = arr { arrayFun = Pull (\ix -> return (TagInt (index var_array ix)))
                      , arrayLen = var var_len
                      }
     cond' <- liftM unBool (cond (TagArray vararr))
     (var_cond,_) <- letsVar "cond" (TagBool cond')
     while (Language.GPUIL.not (var var_cond)) $
       do -- step
          arr' <- liftM unArray (step (TagArray vararr))
          assign var_len (arrayLen arr')
          let new_arr = push (arr' {arrayLen = var var_len})
          let f' = getPushFn new_arr
          -- materialize before next iteration
          f' (\(TagInt i) -> assignArray var_array i)
          cond' <- liftM unBool (cond (TagArray new_arr))
          assign var_cond cond'
     return (TagArray vararr)
fixpoint (TagFn cond) (TagFn step) v =
  do (var0, var0tagged) <- letsVar "loopVar" v
     cond' <- cond var0tagged
     case cond' of
       TagBool b -> while b (do v' <- step var0tagged
                                case v' of
                                  TagInt x -> assign var0 x
                                  TagBool x -> assign var0 x
                                  TagDouble x -> assign var0 x
                                  _ -> error ""
                                )
       _ -> error "Conditional in Fixpoint should be boolean typed"  
     return v
fixpoint _ _ _ = error ""

-- TODO: What will happen with arrays of arrays?
--
-- TODO: Maybe we could just handle everything about array-of-tuples,
-- to tuples of arrays here?
unsafeWrite :: Array Tagged -> Program (Array Tagged)
unsafeWrite arr =
  let parr = push arr
  in case arrayFun parr of
       Push f -> do name <- allocate (arrayElemType parr) (arrayLen parr)
                    f (\tx -> case tx of
                                TagInt i -> assignArray name i
                                e -> error (show e))
                    -- TODO ^ This should unpack according elem type, currently only supports integers
                    --        How do we support arrays of arrays?
                    
                    -- TODO: Also, the "name" tag should be
                    -- "warpIx"/"tid" depending on the level, but
                    -- would be nicer if that could be handled at a
                    -- lower level. Maybe give the level as argument
                    -- to assignArray?
                    syncLocal
                    return (pullFrom name (arrayLen parr) (arrayLevel parr))
       Pull _ -> error "This should not be possible, array was just converted to push array."

pullFrom :: VarName -> CExp -> Level -> Array Tagged
pullFrom name@(_, CPtr _ ty) n lvl =
  Array { arrayElemType = ty
        , arrayLen = n
        , arrayLevel = lvl
        , arrayFun = Pull (\i -> return (TagInt (name ! i)))
                                         -- TODO ^ This should pack
                                         -- according elem type (ty),
                                         -- currently only supports
                                         -- integers
        }
pullFrom _ _ _ = error "pullFrom: must be applied to pointer-typed variable"

compileBinOp :: BinOp -> Tagged -> Tagged -> Tagged
compileBinOp AddI (TagInt i0) (TagInt i1) = TagInt (addi i0 i1)
compileBinOp SubI (TagInt i0) (TagInt i1) = TagInt (subi i0 i1)
compileBinOp MulI (TagInt i0) (TagInt i1) = TagInt (muli i0 i1)
compileBinOp DivI (TagInt i0) (TagInt i1) = TagInt (divi i0 i1)
compileBinOp ModI (TagInt i0) (TagInt i1) = TagInt (modi i0 i1)
compileBinOp MinI (TagInt i0) (TagInt i1) = TagInt (mini i0 i1)
compileBinOp EqI (TagInt i0) (TagInt i1) = TagBool (eqi i0 i1)
compileBinOp op _ _ =
  error $ concat ["Unexpected arguments to binary operator ",
                  show op,
                  ", expecting integer expression."]


compileUnOp :: UnOp -> Tagged -> Tagged
compileUnOp AbsI  (TagInt i0) = TagInt (absi i0)
compileUnOp SignI (TagInt i0) = TagInt (signi i0)
compileUnOp op _ =
  error $ concat ["Unexpected arguments to unary operator ",
                  show op,
                  ", expecting integer expression."]
