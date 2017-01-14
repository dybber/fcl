module FCL.Core.Identifier where

newtype Identifier = Identifier String
  deriving (Show, Eq, Ord)

identToString :: Identifier -> String
identToString (Identifier s) = s
