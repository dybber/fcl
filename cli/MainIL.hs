module Main where

import System.Exit (exitSuccess, exitFailure)
import System.IO (stderr, hPutStrLn)
import System.IO.Error (tryIOError)

import System.Environment (getArgs, getProgName)
import System.Console.GetOpt

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

import FCL.IL.Parser
import FCL.IL.Syntax (ILProgram)
import FCL.IL.NumberVars
import FCL.IL.TypeCheck
import FCL.IL.Pretty

import FCL.IL.Optimise     (optimise)
import FCL.IL.CodeGen      (codeGen)
import FCL.Compile.Config  (CompileConfig(..), defaultCompileConfig)

---------------
---- Monad ----
---------------
data CompilerError = ParseError ParseError
                   | TypeError TypeError
                   | IOError IOError
                   | CLIError String

type CLI = ReaderT Options (ExceptT CompilerError IO)

runCLI :: Options -> CLI a -> IO a
runCLI opts m = do
  result <- runExceptT (runReaderT m opts)
  case result of
    Left err -> exitErr err
    Right r -> return r

liftEither :: (err -> CompilerError) -> Either err a -> CLI a
liftEither f (Left l) = lift (throwE (f l))
liftEither _ (Right r) = return r

throw :: CompilerError -> CLI a
throw err = lift (throwE err)

exitErr :: CompilerError -> IO a
exitErr err =
  let msg =
        case err of
          ParseError e -> show e
          TypeError e -> show e
          IOError e -> show e
          CLIError m -> m
  in do hPutStrLn stderr msg
        exitFailure

-------------------
-- Error logging --
-------------------
message :: String -> CLI ()
message = liftIO . putStrLn

logError :: String -> CLI ()
logError msg = do
  verbosity <- asks ilVerbosity
  if verbosity >= 0
    then message msg
    else return ()

logWarn :: String -> CLI ()
logWarn msg = do
  verbosity <- asks ilVerbosity
  if verbosity >= 1
    then message msg
    else return ()

logInfo :: String -> CLI ()
logInfo msg = do
  verbosity <- asks ilVerbosity
  if verbosity >= 2
    then message msg
    else return ()

logDebug :: String -> CLI ()
logDebug msg = do
  verbosity <- asks ilVerbosity
  if verbosity >= 3
    then message msg
    else return ()

----------------------------
-- Command line arguments --
----------------------------
data Command = Typecheck
             | Compile
             | Help
             | DumpParsed
             | DumpOptimised
  deriving Eq

data Options =
  Options { ilVerbosity :: Int
          , ilOptimizeIterations :: Int
          , ilCommand :: Command
          , ilOutputFile :: String
          , ilProfile :: Bool
          }

defaultOptions :: Options
defaultOptions =
  Options { ilVerbosity = 0
          , ilOptimizeIterations = 10
          , ilCommand = Compile
          , ilOutputFile = "main"
          , ilProfile = False
          }

optionDescriptions :: [OptDescr (Options -> Options)]
optionDescriptions = 
  [ Option "h" ["help"]              (NoArg (\opt -> opt {ilCommand = Help})) "Show help for this command"
  , Option "t" ["typecheck"]         (NoArg (\opt -> opt {ilCommand = Typecheck})) "Type check and print top-level types"
--  , Option []  ["eval"]              (NoArg (\opt -> opt {ilCommand = Eval})) "Run interpreter (entry-point: main)"
  , Option "o" ["output"]            (ReqArg (\out -> (\opt -> opt {ilOutputFile = out})) "FILE") "Specify stem of output files. Writes files <name>.c and <name>.cl)"
  , Option []  ["dump-parsed"] (NoArg (\opt -> opt {ilCommand = DumpParsed})) "Dump intermediate representation."
  , Option []  ["dump-optimised"] (NoArg (\opt -> opt {ilCommand = DumpOptimised})) "Dump intermediate representation."
  , Option "v" ["verbose"]           (NoArg (\opt -> opt {ilVerbosity = 2})) "Verbose output"
  , Option "d" ["debug"]             (NoArg (\opt -> opt {ilVerbosity = 3})) "Debugging output"
  , Option "p" ["profile"]           (NoArg (\opt -> opt {ilProfile = True})) "Profile kernel calls & data transfers."
  , Option "O" ["optimise"]          (ReqArg (\n -> \opt -> opt {ilOptimizeIterations = read n}) "ITERATIONS") "Optimization iterations (defaults to 10)."
  ]

parseOptions :: [String] -> Either [String] ([String], Options)
parseOptions args = do
  case getOpt RequireOrder optionDescriptions args of
    ([], [], []) -> Right ([], defaultOptions {ilCommand = Help})
    (opts, files, []) -> Right (files, foldl (.) id opts defaultOptions)
    (_, _, errs) -> Left errs

-------------
-- Parsing --
-------------
readFileChecked :: FilePath -> CLI String
readFileChecked filename = do
  contents <- liftIO (tryIOError (readFile filename))
  liftEither IOError contents

writeFileChecked :: FilePath -> String -> CLI ()
writeFileChecked filename contents = do
  status <- liftIO (tryIOError (writeFile filename contents))
  liftEither IOError status

parseFile :: FilePath -> CLI (ILProgram ())
parseFile fname = do
  contents <- readFileChecked fname
  logInfo ("Parsing " ++ fname ++ ".")
  liftEither ParseError (parseProgram fname contents)

parseFiles :: [FilePath] -> CLI (ILProgram ())
parseFiles files = do
  asts <- mapM parseFile files
  return (concat asts)

--------------
-- Commands --
--------------
showUsageAndExit :: CLI ()
showUsageAndExit =
  liftIO $
    do prog <- getProgName
       let heading = "Help for FCL (" ++ prog ++ ")"
       putStr (usageInfo heading optionDescriptions)
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
    Right (filenames, opts) -> runCLI opts (dispatch filenames opts)

onCommand :: Command -> CLI () -> CLI ()
onCommand cmd m =
  do opts <- ask
     when (ilCommand opts == cmd)
          (m >> liftIO exitSuccess)

dispatch :: [FilePath] -> Options -> CLI ()
dispatch filenames opts =
  do onCommand Help showUsageAndExit

     let extensions = map extension filenames
     when (any (/= "il") extensions)
          (throw (CLIError "I can only handle .il files."))

     ast <- parseFiles filenames
     onCommand DumpParsed (message (prettyIL ast))

     logInfo "Numbering."
     let numbered = rename ast
     
     logInfo "Typechecking."
     type_env <- liftEither TypeError (typecheck numbered)

     optIter <- asks ilOptimizeIterations
     let optimised = optimise optIter ast

     onCommand DumpOptimised (message (prettyIL optimised))

     verbosity <- asks ilVerbosity
     outputFile <- asks ilOutputFile
     profile <- asks ilProfile
     let cfile = outputFile ++ ".c"
         kernelsFile = outputFile ++ ".cl"
         cfg = defaultCompileConfig { configKernelsFilename = kernelsFile
                                    , configOptimizeIterations = optIter
                                    , configVerbosity = verbosity
                                    , configProfile = profile
                                    }

     onCommand Compile $
       do let (main_, kernels) = codeGen cfg type_env optimised
          writeFileChecked cfile main_
          writeFileChecked kernelsFile kernels
