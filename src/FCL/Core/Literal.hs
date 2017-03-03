module FCL.Core.Literal where

data Literal = LiteralInt Int
             | LiteralBool Bool
             | LiteralDouble Double
             | LiteralString String
  deriving (Show, Eq, Ord)
