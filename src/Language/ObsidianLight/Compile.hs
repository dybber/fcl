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
            | TagArray (Array Tagged)
            | TagFn (Tagged -> Tagged)

type VarEnv = Map.Map VarName Tagged

emptyEnv :: VarEnv
emptyEnv = Map.empty

-- Convert pull arrays to push arrays
push :: Array a -> Array a
push arr =
  case arrayFun arr of
    Pull idx -> arr { arrayFun = Push (\writer -> for (arrayLen arr)
                                                      (\i -> do value <- idx i
                                                                writer value i))
                    }
    Push _ -> arr

compile :: Map.Map VarName Tagged -> OExp -> Tagged
compile _ (IntScalar i) = TagInt (constant i)
compile env (App e0 e1) =
  case compile env e0 of
    TagFn f -> f (compile env e1)
    _ -> error "Unexpected value at function position in application"
compile env (Var x) =
  case Map.lookup x env of
    Just v -> v
    Nothing -> error "Variable not defined"
compile env (Lamb x e) = TagFn $ \v -> compile (Map.insert x v env) e
compile env (BinOp AddI e0 e1) =
  case (compile env e0, compile env e1) of
    (TagInt i0, TagInt i1) -> TagInt $ addi i0 i1
    _ -> error "Unexpected arguments to binary operator AddI, expecting integer expression."
compile env (Generate lvl e0 e1) =
  case (compile env e0, compile env e1) of
    (TagInt e0', TagFn f) ->
      TagArray $ Array { arrayElemType = undefined
                       , arrayLen = e0'
                       , arrayLevel = lvl
                       , arrayFun = Pull (\i -> return $ f (TagInt i))
                       }
    _ -> error "Generate expects integer expression as first argument and function as second argument"
compile env (Map e0 e1) =
  case (compile env e0, compile env e1) of
    (TagFn f, TagArray (Array n ty lvl idx)) ->
      case idx of
        Pull g -> TagArray $ Array { arrayElemType = ty
                                   , arrayLen = n
                                   , arrayLevel = lvl
                                   , arrayFun = Pull (\x -> g x >>= return . f)
                                   }
        Push g -> TagArray $ Array { arrayElemType = ty
                                   , arrayLen = n
                                   , arrayLevel = lvl
                                   , arrayFun = Push (\writer -> g (\e ix -> writer (f e) ix))
                                   }
    _ -> error "Map expects function as first argument and pull array as second argument"
compile env (Length e0) =
  case compile env e0 of
    TagArray arr -> TagInt (arrayLen arr)
    _ -> error "Length expects array as argument"
compile env (ComputeLocal e0) =
  case compile env e0 of
    TagArray arr -> TagArray (unsafeWritePush (push arr))
    _ -> error "ComputeLocal expects array as argument"

unsafeWritePush :: Array Tagged -> Array Tagged
unsafeWritePush = undefined
-- unsafeWritePush parr =
--   case arrayLevel parr of
--     Block -> 
