module FCL.Core.MonoLevel where

data Level = Zero | Step Level
  deriving (Eq, Show)
