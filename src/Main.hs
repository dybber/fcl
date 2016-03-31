module Main where

import System.Exit (exitSuccess, exitFailure)
import System.IO (stderr, hPutStrLn)
import System.IO.Error (tryIOError)

import System.Environment (getArgs, getProgName)
import System.Console.GetOpt

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

import Paths_fcl

import Language.FCL

---------------
---- Monad ----
---------------
type CLI = ReaderT Options (ExceptT CompilerError IO)

runCLI :: Options -> CLI a -> IO a
runCLI opts m = do
  result <- runExceptT (runReaderT m opts)
  case result of
    Left err -> exitErr (show err)
    Right r -> return r

liftEither :: (err -> CompilerError) -> Either err a -> CLI a
liftEither f (Left l) = lift (throwE (f l))
liftEither _ (Right r) = return r

----------------------------
-- Command line arguments --
----------------------------
data Options =
  Options { fclVerbosity :: Int
          , fclOptimizeIterations :: Int
          , fclStopAfterParsing :: Bool
          , fclEval :: Bool
          , fclShowUsageAndExit :: Bool
          , fclOutputFile :: String
          }

defaultOptions :: Options
defaultOptions =
  Options { fclVerbosity = 0
          , fclOptimizeIterations = 10
          , fclStopAfterParsing = False
          , fclEval = False
          , fclShowUsageAndExit = False
          , fclOutputFile = "kernels.cl"
          }

optionDescriptions :: [OptDescr (Options -> Options)]
optionDescriptions = 
  [ Option "v" ["verbose"]    (NoArg (\opt -> opt {fclVerbosity = 3})) "Verbose output"
  , Option "h" ["help"]       (NoArg (\opt -> opt {fclShowUsageAndExit = True})) "Show help for this command"
  , Option []  ["parse-only"] (NoArg (\opt -> opt {fclStopAfterParsing = True})) "Parse and print syntax tree"
  , Option []  ["eval"]       (NoArg (\opt -> opt {fclEval = True})) "Parse and interpret main-function"
  , Option "o" ["output"]     (ReqArg (\out -> (\opt -> opt {fclOutputFile = out})) "FILE") "where to emit OpenCL kernels"
  ]

showUsageAndExit :: CLI ()
showUsageAndExit =
  liftIO $
    do prog <- getProgName
       let heading = "Help for FCL (" ++ prog ++ ")"
       (putStrLn (usageInfo heading optionDescriptions))
       exitFailure

parseOptions :: [String] -> Either [String] ([String], Options)
parseOptions args = do
  case getOpt RequireOrder optionDescriptions args of
    (opts, files, []) -> Right (files, foldl (.) id opts defaultOptions)
    (_, _, errs) -> Left errs

-------------------
-- Error logging --
-------------------
logError :: String -> CLI ()
logError msg = do
  verbosity <- asks fclVerbosity
  if verbosity >= 0
    then liftIO (putStrLn msg)
    else return ()

logWarn :: String -> CLI ()
logWarn msg = do
  verbosity <- asks fclVerbosity
  if verbosity >= 1
    then liftIO (putStrLn msg)
    else return ()

logInfo :: String -> CLI ()
logInfo msg = do
  verbosity <- asks fclVerbosity
  if verbosity >= 2
    then liftIO (putStrLn msg)
    else return ()

logDebug :: String -> CLI ()
logDebug msg = do
  verbosity <- asks fclVerbosity
  if verbosity >= 3
    then liftIO (putStrLn msg)
    else return ()

-------------
-- Parsing --
-------------
readFileSafe :: FilePath -> CLI String
readFileSafe filename = do
  contents <- liftIO (tryIOError (readFile filename))
  liftEither IOError contents

parseFile :: String -> CLI (Program Untyped)
parseFile fname = do
  contents <- readFileSafe fname
  logInfo ("Parsing " ++ fname ++ ".")
  liftEither ParseError (parseTopLevel fname contents)

parseFiles :: [String] -> CLI (Program Untyped)
parseFiles files = do
  definitions <- mapM parseFile files
  return (concat definitions)

-----------------
-- Compilation --
-----------------
compileFiles :: Program Untyped -> CLI String
compileFiles ast =
  do logInfo "Typechecking."
     es <- liftEither TypeError (infer ast)
     mapM_ (logInfo . (" " ++) . showType) es
     logInfo "Inlining."
     let esInline = inline es
     logDebug ("AST after inlining: " ++ show esInline)
     logInfo (show (length esInline) ++ " kernels.")
     logInfo "Compiling."
     optIter <- asks fclOptimizeIterations
     let cp = compileKernels optIter esInline
     return (unlines (Prelude.map renderKernel cp))

exitErr :: String -> IO a
exitErr msg = do
  hPutStrLn stderr ("Error: " ++ msg)
  exitFailure

----------------------
-- Main entry point --
----------------------
extension :: FilePath -> String
extension = reverse . takeWhile ((/=) '.') . reverse

main :: IO ()
main = do
  args <- getArgs
  case parseOptions args of
    Left l -> error (show l)
    Right (filenames, opts) -> runCLI opts (compile filenames opts)

compile :: [FilePath] -> Options -> CLI ()
compile filenames opts =
  do when (fclShowUsageAndExit opts) showUsageAndExit

     prelude <- liftIO (getDataFileName "lib/prelude.fcl")
     ast <- parseFiles (prelude : filenames)

     let extensions = map extension filenames
     when (all (== "fcl") extensions) (error "I can only handle .fcl files.")

     when (fclStopAfterParsing opts) (liftIO (print ast >> exitSuccess))

     if fclEval opts
       then do v <- liftEither EvalError (return (eval ast))
               liftIO (print v)
       else do kernels <- compileFiles ast
               liftIO (writeFile (fclOutputFile opts) kernels)
