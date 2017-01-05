module Language.FCL.CompileConfig where

data CompileConfig =
  CompileConfig
    { configBlockSize :: Int
    , configSharedMemory :: Int -- in bytes
    , configNumWorkGroups :: Int
    , configWarpSize  :: Int
    , configOptimizeIterations :: Int
    , configKernelsFilename :: FilePath
    , configVerbosity :: Int
    }
  deriving (Show, Eq)

defaultCompileConfig :: CompileConfig
defaultCompileConfig =
  CompileConfig
    { configBlockSize = 256
    , configWarpSize = 32
    , configNumWorkGroups = 2048
    , configSharedMemory = 2048 -- bytes
    , configOptimizeIterations = 20
    , configKernelsFilename = "kernels.cl"
    , configVerbosity = 0
    }
