module FCL.Compile.Value where

import qualified Data.Map as Map
-- import Control.Monad.Trans.Reader
-- import Control.Monad.Trans.Except
-- import Control.Monad.Trans.Class
-- import Control.Monad

import FCL.Core.SourceRegion
import FCL.Core.Identifier
import FCL.Core.Monotyped
import FCL.Core.MonoLevel


import FCL.IL.Cons


-- data CompileError =
--     UndefinedVariable Identifier
--   | ArrayInConditional
--   | FunctionInConditional
--   | ProgramInConditional
--   | InternalError String
--  deriving Show

-- type Compile a = ReaderT CompileEnv
--                          (Except CompileError)
--                          a

-- runCompile :: Compile a -> CompileEnv -> Either CompileError a
-- runCompile m env = runExcept (runReaderT m env)

-- throwError :: CompileError -> Compile a
-- throwError = lift . throwE

type CompileEnv = Map.Map Identifier Binding

data Binding = Predefined (Type -> Value)
             | UserDefined Value

emptyEnv :: CompileEnv
emptyEnv = Map.empty

-- withUserDefined :: Identifier -> Value -> Compile a -> Compile a
-- withUserDefined x v = local (Map.insert x (UserDefined v))

lookupSymbol :: Identifier -> Type -> CompileEnv -> Maybe Value
lookupSymbol x ty env =
  case Map.lookup x env of
    Just (Predefined f) -> Just (f ty)
    Just (UserDefined d) -> Just d
    Nothing -> Nothing
