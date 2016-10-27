-- | Source code locations, used in error reporting
module Language.FCL.SourceRegion where

import Text.Parsec

data Position = Position { fileName :: FilePath
                         , line :: Int
                         , column :: Int }
                deriving (Eq, Ord)

data Region = Region Position Position
            | Missing
  deriving (Eq, Ord)

---------------------
-- Pretty printing --
---------------------

printXY :: Position -> String
printXY p = show (line p) ++ "." ++ show (column p)

instance Show Position where
  show p = show (fileName p) ++ ":" ++ printXY p

instance Show Region where
  show (Region p1 p2)
    | p1 == p2  = show p1
    | otherwise = show p1 ++ "-" ++ show p2
  show Missing = "<missing>"

----------------------
-- Parsec utilities --
----------------------

-- | Convert Parsec `SourcePos` to `Position`
fromSourcePos :: SourcePos -> Position
fromSourcePos pos = Position { fileName = sourceName pos
                             , line = sourceLine pos
                             , column = sourceColumn pos }


-- | Convert two Parsec `SourcePos` positions to a `Region`
newRegion :: SourcePos -> SourcePos -> Region
newRegion pos1 pos2 = Region (fromSourcePos pos1)
                             (fromSourcePos pos2)

withRegion :: Monad m => ParsecT s u m (Region -> b) -> ParsecT s u m b
withRegion p = do
  pos1 <- getPosition
  f <- p
  pos2 <- getPosition
  return (f (newRegion pos1 pos2))
