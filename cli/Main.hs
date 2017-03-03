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

import Paths_fcl
import FCL


import qualified FCL.Core.Polytyped as Poly
import qualified FCL.External.Syntax as E

---------------
---- Monad ----
---------------

data CompilerError = FCLError FCLError
                   | IOError IOError
                   | CLIError String
  deriving Show

type CLI = ReaderT Options (ExceptT CompilerError IO)

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

throw :: CompilerError -> CLI a
throw err = lift (throwE err)

data Command = ParseOnly
             | Typecheck
             | Compile
             | Help
             | DumpDesugared
             | DumpTyped
             | DumpMonomorphed
             | DumpIL
             | DumpILOptimised
  deriving Eq

----------------------------
-- Command line arguments --
----------------------------
data Options =
  Options { fclVerbosity :: Int
          , fclOptimizeIterations :: Int
          , fclNoPrelude :: Bool
          , fclCommand :: Command
          , fclOutputFile :: String
          }

defaultOptions :: Options
defaultOptions =
  Options { fclVerbosity = 0
          , fclOptimizeIterations = 10
          , fclNoPrelude = False
          , fclCommand = Compile
          , fclOutputFile = "main"
          }

optionDescriptions :: [OptDescr (Options -> Options)]
optionDescriptions = 
  [ Option "h" ["help"]             (NoArg (\opt -> opt {fclCommand = Help})) "Show help for this command"
  , Option []  ["no-prelude"]   (NoArg (\opt -> opt {fclNoPrelude = True})) "Do not include the FCL prelude"
  , Option "t" ["typecheck"]        (NoArg (\opt -> opt {fclCommand = Typecheck})) "Type check and print top-level types"
--  , Option []  ["eval"]             (NoArg (\opt -> opt {fclCommand = Eval})) "Run interpreter (entry-point: main)"
  , Option "o" ["output"]           (ReqArg (\out -> (\opt -> opt {fclOutputFile = out})) "FILE") "Specify stem of output files. Writes files <name>.c and <name>.cl)"
--  , Option []  ["test-parser"]     (NoArg (\opt -> opt {fclCommand = ParserTest})) "Parse, pretty print, parse again, check for equality."
  , Option []  ["dump-desugared"]     (NoArg (\opt -> opt {fclCommand = DumpDesugared})) "Parse, desugar."
  , Option []  ["dump-typed"] (NoArg (\opt -> opt {fclCommand = DumpTyped})) "Dump AST after typing"
  , Option []  ["dump-monomorphed"] (NoArg (\opt -> opt {fclCommand = DumpMonomorphed})) "Dump AST after monomorphization."
  , Option []  ["dump-il"]          (NoArg (\opt -> opt {fclCommand = DumpIL})) "Dump intermediate representation."
  , Option []  ["dump-il-optimised"] (NoArg (\opt -> opt {fclCommand = DumpILOptimised})) "Dump intermediate representation."
  , Option "v" ["verbose"]      (NoArg (\opt -> opt {fclVerbosity = 2})) "Verbose output"
  , Option "d" ["debug"]        (NoArg (\opt -> opt {fclVerbosity = 3})) "Debug output"
  ]

parseOptions :: [String] -> Either [String] ([String], Options)
parseOptions args = do
  case getOpt RequireOrder optionDescriptions args of
    ([], [], []) -> Right ([], defaultOptions {fclCommand = Help})
    (opts, files, []) -> Right (files, foldl (.) id opts defaultOptions)
    (_, _, errs) -> Left errs

-------------------
-- Error logging --
-------------------
message :: String -> CLI ()
message = liftIO . putStrLn

logError :: String -> CLI ()
logError msg = do
  verbosity <- asks fclVerbosity
  if verbosity >= 0
    then message msg
    else return ()

logWarn :: String -> CLI ()
logWarn msg = do
  verbosity <- asks fclVerbosity
  if verbosity >= 1
    then message msg
    else return ()

logInfo :: String -> CLI ()
logInfo msg = do
  verbosity <- asks fclVerbosity
  if verbosity >= 2
    then message msg
    else return ()

logDebug :: String -> CLI ()
logDebug msg = do
  verbosity <- asks fclVerbosity
  if verbosity >= 3
    then message msg
    else return ()

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

parseFile :: String -> CLI E.Program
parseFile fname = do
  contents <- readFileChecked fname
  logInfo ("Parsing " ++ fname ++ ".")
  liftEither FCLError (parse fname contents)

parseFiles :: [String] -> CLI E.Program
parseFiles files = do
  definitions <- concat <$> mapM parseFile files
  return definitions

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

printTypes :: Poly.Exp -> CLI ()
printTypes ast =
  case ast of
    Poly.Let ident tysc _ _ body _ ->
      do message (ident ++ " : " ++ display tysc)
         printTypes body
    _ -> return ()

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
     when (fclCommand opts == cmd)
          (m >> liftIO exitSuccess)

dispatch :: [FilePath] -> Options -> CLI ()
dispatch filenames opts =
  do onCommand Help showUsageAndExit

     let extensions = map extension filenames
     when (any (/= "fcl") extensions)
          (throw (CLIError "I can only handle .fcl files."))

     -- onCommand ParserTest (parserTest filenames)

     prelude  <- liftIO (getDataFileName "lib/prelude.fcl")
     let files = if fclNoPrelude opts
                   then filenames
                   else prelude : filenames

     ast <- parseFiles files
     onCommand ParseOnly (message (show ast))

     logInfo "Desugaring."
     desugared <- liftEither FCLError (desugar ast)
     onCommand DumpDesugared (message (displayTopLevelUntyped desugared))
     
     logInfo "Typechecking."
     (_, typed_ast) <- liftEither FCLError (infer initialTypeEnvironment desugared)
     onCommand Typecheck   (printTypes typed_ast)

     onCommand DumpTyped (message (displayTopLevelPoly typed_ast))

     monomorphed <- liftEither FCLError (monomorph initialTypeEnvironment typed_ast)
     onCommand DumpMonomorphed (message (displayTopLevelMono monomorphed))

     logInfo "Compiling."
     optIter <- asks fclOptimizeIterations
     verbosity <- asks fclVerbosity
     outputFile <- asks fclOutputFile
     let cfile = outputFile ++ ".c"
         kernelsFile = outputFile ++ ".cl"
         cfg = defaultCompileConfig { configKernelsFilename = kernelsFile
                                    , configOptimizeIterations = optIter
                                    , configVerbosity = verbosity
                                    }
         intermediate_prog = compile cfg monomorphed
     onCommand DumpIL (message (prettyIL intermediate_prog))

     let optimised = optimise optIter intermediate_prog

     onCommand DumpILOptimised (message (prettyIL optimised))

     onCommand Compile $
       do let (main_, kernels) = codeGen cfg optimised
          writeFileChecked cfile main_
          writeFileChecked kernelsFile kernels

