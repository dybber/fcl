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

data CompilerError = IOError IOError
                   | ParseError ParseError
                   | TypeError TypeError
                   | EvalError String
  deriving Show

exitErr :: String -> IO a
exitErr msg = do
  hPutStrLn stderr ("Error: " ++ msg)
  exitFailure

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
          , fclNoPrelude :: Bool
          , fclStopAfterParsing :: Bool
          , fclStopAfterTypeCheck :: Bool
          , fclEval :: Bool
          , fclShowUsageAndExit :: Bool
          , fclOutputFile :: String
          }

defaultOptions :: Options
defaultOptions =
  Options { fclVerbosity = 0
          , fclOptimizeIterations = 10
          , fclNoPrelude = False
          , fclStopAfterParsing = False
          , fclStopAfterTypeCheck = False
          , fclEval = False
          , fclShowUsageAndExit = False
          , fclOutputFile = "kernels.cl"
          }

optionDescriptions :: [OptDescr (Options -> Options)]
optionDescriptions = 
  [ Option "v" ["verbose"]    (NoArg (\opt -> opt {fclVerbosity = 2})) "Verbose output"
  , Option "d" ["debug"]      (NoArg (\opt -> opt {fclVerbosity = 3})) "Debug output"
  , Option "h" ["help"]       (NoArg (\opt -> opt {fclShowUsageAndExit = True})) "Show help for this command"
  , Option []  ["no-prelude"] (NoArg (\opt -> opt {fclNoPrelude = True})) "Do not include the FCL prelude"
  , Option []  ["parse-only"] (NoArg (\opt -> opt {fclStopAfterParsing = True})) "Parse and print syntax tree"
  , Option "t" ["tc-only"]    (NoArg (\opt -> opt {fclStopAfterTypeCheck = True})) "Parse, type check and print top-level types"
  , Option []  ["eval"]       (NoArg (\opt -> opt {fclEval = True})) "Parse and interpret main-function"
  , Option "o" ["output"]     (ReqArg (\out -> (\opt -> opt {fclOutputFile = out})) "FILE") "where to emit OpenCL kernels"
  ]

showUsageAndExit :: CLI ()
showUsageAndExit =
  liftIO $
    do prog <- getProgName
       let heading = "Help for FCL (" ++ prog ++ ")"
       putStr (usageInfo heading optionDescriptions)
       exitFailure

parseOptions :: [String] -> Either [String] ([String], Options)
parseOptions args = do
  case getOpt RequireOrder optionDescriptions args of
    ([], [], []) -> Right ([], defaultOptions {fclShowUsageAndExit = True})
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
compileFiles :: Program Type -> CLI String
compileFiles ast =
  do logInfo "Inlining."
     let inlined = inline ast
     logInfo (show (length inlined) ++ " kernels.")

     logInfo "Simplifying."
     let simpl = simplify inlined

     logInfo "Typechecking again."
     typed_ast <- liftEither TypeError (typeinfer simpl)
     mapM_ (logInfo . (" " ++) . showType) typed_ast
     --logDebug ("AST after inlining and simplify: " ++ show typed_ast)

     logDebug (prettyPrintProgram typed_ast)
     
     logInfo "Compiling."
     optIter <- asks fclOptimizeIterations
     let cp = compileKernels optIter typed_ast
     return (pretty OpenCL cp)

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

     prelude  <- liftIO (getDataFileName "lib/prelude.fcl")

     let files = if fclNoPrelude opts
                   then filenames
                   else prelude : filenames

     let extensions = map extension filenames
     when (any (/= "fcl") extensions) (liftIO (exitErr "I can only handle .fcl files."))
     
     ast <- parseFiles files
     let (ast', i) = initTyVars ast
     logDebug ("AST: " ++ show ast')

     when (fclStopAfterParsing opts) (liftIO exitSuccess)

     logInfo "Typechecking."
     typed_ast <- liftEither TypeError (typeinfer ast')
     mapM_ (logInfo . (" " ++) . showType) typed_ast

     when (fclStopAfterTypeCheck opts) (liftIO exitSuccess)

     if fclEval opts
       then do v <- liftEither EvalError (return (eval typed_ast))
               liftIO (print v)
       else do kernels <- compileFiles typed_ast
               liftIO (writeFile (fclOutputFile opts) kernels)
