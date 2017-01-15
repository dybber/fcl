-- | Source code locations, used in error reporting
module FCL.Core.SourceRegion where

data Position = Position { fileName :: FilePath
                         , line :: Int
                         , column :: Int }
              | Missing
  deriving (Eq, Ord)

data SourceRegion = SourceRegion Position Position
  deriving (Eq, Ord)

newRegion :: Position -> Position -> SourceRegion
newRegion = SourceRegion

class SourceInfo a where
  getSourceRegion :: a -> SourceRegion

beginPosition :: SourceRegion -> Position
beginPosition (SourceRegion pos1 _) = pos1

endPosition :: SourceRegion -> Position
endPosition (SourceRegion _ pos2) = pos2

---------------------
-- Pretty printing --
---------------------

printXY :: Position -> String
printXY p = show (line p) ++ "." ++ show (column p)

instance Show Position where
  show (p@(Position fname _ _)) = show fname ++ ":" ++ printXY p
  show Missing = "<<missing>>"

instance Show SourceRegion where
  show (SourceRegion p1 p2)
    | p1 == p2  = show p1
    | otherwise = show p1 ++ "-" ++ show p2
