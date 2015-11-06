module Language.ObsidianLight.Compile where

import Language.ObsidianLight.Syntax
import Language.GPUIL
import qualified Data.Map as Map

data Idx a = Pull (Exp -> Program a)
           | Push ((a -> Exp -> Program ()) -> Program ())

data Array a = Array { arrayLen :: Exp
                     , arrayElemType :: Type
                     , arrayLevel :: Level
                     , arrayFun :: Idx a
                     }

data Tagged = TagInt Exp
            | TagBool Exp
            | TagArray (Array Tagged)
            | TagFn (Tagged -> Program Tagged)
            | TagPair Exp

type VarEnv = Map.Map VarName Tagged

emptyEnv :: VarEnv
emptyEnv = Map.empty

-- Convert pull arrays to push arrays
push :: Array a -> Array a
push arr =
  case arrayFun arr of
    Pull idx -> arr { arrayFun = Push (\writer -> forAll
                                                      (arrayLevel arr)
                                                      (arrayLen arr)
                                                      (\i -> do value <- idx i
                                                                writer value i))
                    }
    Push _ -> arr

compileBinOp :: BinOp -> Tagged -> Tagged -> Tagged
compileBinOp AddI (TagInt i0) (TagInt i1) = TagInt (addi i0 i1)
compileBinOp SubI (TagInt i0) (TagInt i1) = TagInt (subi i0 i1)
compileBinOp MulI (TagInt i0) (TagInt i1) = TagInt (muli i0 i1)
compileBinOp DivI (TagInt i0) (TagInt i1) = TagInt (divi i0 i1)
compileBinOp ModI (TagInt i0) (TagInt i1) = TagInt (modi i0 i1)
compileBinOp MinI (TagInt i0) (TagInt i1) = TagInt (mini i0 i1)
compileBinOp EqI (TagInt i0) (TagInt i1) = TagBool (eqi i0 i1)
compileBinOp op _ _ = error $ concat ["Unexpected arguments to binary operator ",
                                      show op,
                                      ", expecting integer expression."]

compile :: Map.Map VarName Tagged -> OExp -> Program Tagged
compile _ (IntScalar i) = return (TagInt (constant i))
compile _ (BoolScalar i) = return (TagBool (constant i))
compile env (App e0 e1) = do
  v0 <- compile env e0
  case v0 of
    TagFn f -> f =<< compile env e1
    _ -> error "Unexpected value at function position in application"
compile env (Var x) =
  case Map.lookup x env of
    Just v -> return v
    Nothing -> error "Variable not defined"
compile env (Lamb x e) = return . TagFn $ \v -> compile (Map.insert x v env) e
compile env (Let x e0 e1) = do
  v0 <- compile env e0
  compile (Map.insert x v0 env) e1
compile env (BinOp op e0 e1) = do
  v0 <- compile env e0
  v1 <- compile env e1
  return (compileBinOp op v0 v1)
compile env (Cond e0 e1 e2) = do
  v0 <- compile env e0
  v1 <- compile env e1
  v2 <- compile env e2
  case v0 of
    (TagBool b0) -> case (v1, v2) of
                     (TagInt i1, TagInt i2) -> return . TagInt $ if_ b0 i1 i2
                     (TagBool b1, TagBool b2) -> return . TagBool $ if_ b0 b1 b2
                     (TagArray _, TagArray _) -> error "TODO: not possible yet"
                     (TagFn _, TagFn _) -> error "TODO: yet to be implemented"
                     (_,_) -> error "branches are differing"
    _ -> error "Expecting boolean expression as conditional argument in branch"
compile env (Generate lvl e0 e1) = do
  v0 <- compile env e0
  v1 <- compile env e1
  case (v0, v1) of
    (TagInt e0', TagFn f) ->
      return . TagArray $ Array { arrayElemType = int -- TODO, when type inference is done,
                                                      -- put something sensible here
                                , arrayLen = e0'
                                , arrayLevel = lvl
                                , arrayFun = Pull (\i -> f (TagInt i))
                                }
    _ -> error "Generate expects integer expression as first argument and function as second argument"
compile env (Map e0 e1) = do
  f' <- compile env e0
  e' <- compile env e1
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
compile env (Length e0) = do
  v <- compile env e0
  case v of
    TagArray arr -> return $ TagInt (arrayLen arr)
    _ -> error "Length expects array as argument"
compile env (Index e0 e1) = do
  v0 <- compile env e0
  v1 <- compile env e1
  case (v0, v1) of
    (TagArray arr, TagInt i) ->
      case arrayFun arr of
        Pull idx -> idx i
        Push _ -> do farr <- unsafeWrite arr
                     case arrayFun farr of
                          Pull idx -> idx i
                          Push _ -> error "Impossible"
    _ -> error "Index expects array and integer argument"
compile env (ForceLocal e0) = do
  v0 <- compile env e0
  case v0 of
    TagArray arr -> TagArray `fmap` unsafeWrite arr
    _ -> error "ComputeLocal expects array as argument"

-- TODO: What will happen with arrays of arrays?
--
-- TODO: Maybe we could just handle everything about array-of-tuples,
-- to tuples of arrays here?
unsafeWrite :: Array Tagged -> Program (Array Tagged)
unsafeWrite arr =
  let parr = push arr
  in case arrayFun parr of
       Push f -> do name <- allocate (arrayElemType parr) (arrayLen parr)
                    f (\(TagInt i) -> assignArray name i)
                    -- TODO ^ This should unpack according elem type, currently only supports integers
                    
                    -- TODO: Also, the "name" tag should be
                    -- "warpIx"/"tid" depending on the level, but
                    -- would be nicer if that could be handled at a
                    -- lower level. Maybe give the level as argument
                    -- to assignArray?
                    return $ pullFrom name (arrayLen parr) (arrayLevel parr)
       Pull _ -> error "This should not be possible, array was just converted to push array."

pullFrom :: (VarName, Type) -> Exp -> Level -> Array Tagged
pullFrom name@(_, ty) n lvl =
  Array { arrayElemType = ty
        , arrayLen = n
        , arrayLevel = lvl
        , arrayFun = Pull (\i -> return (TagInt $ name ! i))
                                         -- TODO ^ This should pack
                                         -- according elem type,
                                         -- currently only supports
                                         -- integers
        }
