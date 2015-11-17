module Language.ObsidianLight.Compile where

import Control.Monad (liftM)

import Language.ObsidianLight.Syntax
import Language.GPUIL
import qualified Data.Map as Map

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

compileFun :: Map.Map Variable Tagged -> Exp Type -> Program Tagged
compileFun env (Lamb x ty e _) = do
  v <- addParam "input" (convertType ty)
  compileFun (Map.insert x (tagExp ty $ var v) env) e
compileFun env e = comp env e

tagExp :: Type -> CExp -> Tagged
tagExp IntT e = TagInt e
tagExp DoubleT e = TagDouble e
tagExp BoolT e = TagBool e
tagExp ty e = error ("tagExp: " ++ show ty ++ " -- " ++ show e)
-- TODO

convertType :: Type -> CType
convertType IntT = int
convertType DoubleT = double
convertType BoolT = bool
convertType (ArrayT _ ty) = pointer [] (convertType ty)
convertType (_ :> _) = error "convertType: functions can not be used as arguments to kernels or occur in arrays"
convertType (_ :*: _) = error "convertType: tuples not yet support in argument or results from kernels (on the TODO!)"


comp :: Map.Map Variable Tagged -> Exp Type -> Program Tagged
comp _ (IntScalar i) = return (TagInt (constant i))
comp _ (DoubleScalar d) = return (TagDouble (constant d))
comp _ (BoolScalar b) = return (TagBool (constant b))
comp env (App e0 e1) = do
  v0 <- comp env e0
  case v0 of
    TagFn f -> f =<< comp env e1
    _ -> error "Unexpected value at function position in application"
comp env (Pair e0 e1) = do
  v0 <- comp env e0
  v1 <- comp env e1
  return (TagPair v0 v1)
comp env (Proj1E e) = do
  v <- comp env e
  case v of
    TagPair v0 _ -> return v0
    _ -> error "Projection expects Pair as argument"
comp env (Proj2E e) = do
  v <- comp env e
  case v of
    TagPair _ v1 -> return v1
    _ -> error "Projection expects Pair as argument"
comp env (Var x _) =
  case Map.lookup x env of
    Just v -> return v
    Nothing -> error "Variable not defined"
comp env (Lamb x ty e _) = return . TagFn $ \v -> comp (Map.insert x v env) e
comp env (Let x e0 e1 _) = do -- TODO: do actual let-declaration (requires type information)
  v0 <- comp env e0
  comp (Map.insert x v0 env) e1
comp env (UnOp op e0) = do
  v0 <- comp env e0
  return (compileUnOp op v0)
comp env (BinOp op e0 e1) = do
  v0 <- comp env e0
  v1 <- comp env e1
  return (compileBinOp op v0 v1)
comp env (Cond e0 e1 e2 _) = do
  v0 <- comp env e0
  v1 <- comp env e1
  v2 <- comp env e2
  case v0 of
    (TagBool b0) -> case (v1, v2) of
                     (TagInt i1, TagInt i2) -> return . TagInt $ if_ b0 i1 i2
                     (TagBool b1, TagBool b2) -> return . TagBool $ if_ b0 b1 b2
                     (TagArray _, TagArray _) -> error "TODO: not possible yet"
                     (TagFn _, TagFn _) -> error "TODO: yet to be implemented"
                     (_,_) -> error "branches are differing"
    _ -> error "Expecting boolean expression as conditional argument in branch"
comp env (Generate lvl e0 e1) = do
  v0 <- comp env e0
  v1 <- comp env e1
  case (v0, v1) of
    (TagInt e0', TagFn f) ->
      return . TagArray $ Array { arrayElemType = int -- TODO, when type inference is done,
                                                      -- put something sensible here
                                , arrayLen = e0'
                                , arrayLevel = lvl
                                , arrayFun = Pull (\i -> f (TagInt i))
                                }
    _ -> error "Generate expects integer expression as first argument and function as second argument"
comp env (Map e0 e1) = do
  f' <- comp env e0
  e' <- comp env e1
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
    _ -> error "Map expects function as first argument and pull array as second argument"
comp env (Length e0) = do
  v <- comp env e0
  case v of
    TagArray arr -> return $ TagInt (arrayLen arr)
    e -> error $ "Length expects array as argument got " ++ show e
comp env (Index e0 e1) = do
  v0 <- comp env e0
  v1 <- comp env e1
  case (v0, v1) of
    (TagArray arr, TagInt i) ->
      case arrayFun arr of
        Pull idx -> idx i
        Push _ -> do farr <- unsafeWrite arr
                     case arrayFun farr of
                          Pull idx -> idx i
                          Push _ -> error "Impossible"
    _ -> error "Index expects array and integer argument"
comp env (ForceLocal e0) = do
  v0 <- comp env e0
  case v0 of
    TagArray arr -> liftM TagArray (unsafeWrite arr)
    _ -> error "ComputeLocal expects array as argument"
comp env (Fixpoint e0 e1 e2) = do
  v0 <- comp env e0
  v1 <- comp env e1
  v2 <- comp env e2
  fixpoint v0 v1 v2

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
    TagArray x -> error "lets TagArray" -- liftM TagArray (let_ name (arrayElemType x) x)

letsVar :: String -> Tagged -> Program (VarName, Tagged)
letsVar name s =
  case s of
    TagInt x -> do var0 <- letVar name int x
                   return (var0, TagInt (var var0))
    TagBool x -> do var0 <- letVar name int x
                    return (var0, TagBool (var var0))
    TagDouble x -> do var0 <- letVar name int x
                      return (var0, TagDouble (var var0))
    TagFn _ -> error "letsVar TagFn"
    TagPair x y -> error "letsVar TagPair"
    TagArray x -> error "letsVar TagArray" -- liftM TagArray (let_ name (arrayElemType x) x)

getPushFn :: Array a -> PushFn a
getPushFn (Array {arrayFun = Push f }) = f
getPushFn _ = error ""

unBool :: Tagged -> CExp
unBool (TagBool e) = e
unBool _ = error ""
unArray :: Tagged -> Array Tagged
unArray (TagArray e) = e
unArray _ = error ""

fixpoint :: Tagged -> Tagged -> Tagged -> Program Tagged
fixpoint (TagFn cond) (TagFn step) (TagArray arr) =
  do -- Declare array
     var_array <- allocate (arrayElemType arr) (arrayLen arr)
     (var_len,_) <- letsVar "arraySize" (TagInt (arrayLen arr))

     -- Materialize before loop
     let f = getPushFn (push arr)
     f (\(TagInt i) -> assignArray var_array i)
     syncLocal

     -- Loop using the the same array
     let vararr = arr { arrayFun = Pull (\ix -> return (TagInt (index var_array ix)))
                      , arrayLen = var var_len
                      }
     cond' <- liftM unBool (cond (TagArray vararr))
     while (Language.GPUIL.not cond') $
       do -- step
          arr' <- liftM unArray (step (TagArray vararr))
          assign var_len (arrayLen arr')
          let f' = getPushFn $ push (arr' {arrayLen = var var_len})
          -- materialize before next iteration
          f' (\(TagInt i) -> assignArray var_array i)
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
pullFrom name@(_, ty) n lvl =
  Array { arrayElemType = ty
        , arrayLen = n
        , arrayLevel = lvl
        , arrayFun = Pull (\i -> return (TagInt (name ! i)))
                                         -- TODO ^ This should pack
                                         -- according elem type (ty),
                                         -- currently only supports
                                         -- integers
        }


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
