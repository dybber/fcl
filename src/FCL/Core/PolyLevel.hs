module FCL.Core.PolyLevel where
  
newtype LvlVar = LvlVar Int
  deriving (Eq, Ord, Show)

data Level = Zero | Step Level | VarL LvlVar
  deriving (Eq, Ord, Show)

isPolymorphicLevel :: Level -> Bool
isPolymorphicLevel lvl =
  case lvl of
    VarL _ -> True
    Zero -> False
    Step lvl0 -> isPolymorphicLevel lvl0

threadLevel, blockLevel, gridLevel :: Level
threadLevel = Zero
blockLevel  = Step threadLevel
gridLevel   = Step blockLevel
