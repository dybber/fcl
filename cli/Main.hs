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


type CLI = ReaderT Options (ExceptT CompilerError IO)

exitErr :: CompilerError -> IO a
exitErr err =
  let msg =
        case err of
          FCLError e -> display e
          IOError e -> show e
          CLIError m -> m
  in do hPutStrLn stderr msg
        exitFailure

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

----------------------------
-- Command line arguments --
----------------------------
data Command = Typecheck
             | Compile
             | Help
             | DumpParsed
             | DumpDesugared
             | DumpTyped
             | DumpMonomorphed
             | DumpIL
             | DumpILOptimised
             | TestParser
  deriving Eq


data Options =
  Options { fclVerbosity :: Int
          , fclOptimizeIterations :: Int
          , fclNoPrelude :: Bool
          , fclCommand :: Command
          , fclOutputFile :: String
          , fclProfile :: Bool
          }

defaultOptions :: Options
defaultOptions =
  Options { fclVerbosity = 0
          , fclOptimizeIterations = 10
          , fclNoPrelude = False
          , fclCommand = Compile
          , fclOutputFile = "main"
          , fclProfile = False
          }

optionDescriptions :: [OptDescr (Options -> Options)]
optionDescriptions = 
  [ Option "h" ["help"]              (NoArg (\opt -> opt {fclCommand = Help})) "Show help for this command"
  , Option []  ["no-prelude"]        (NoArg (\opt -> opt {fclNoPrelude = True})) "Do not include the FCL prelude"
  , Option "t" ["typecheck"]         (NoArg (\opt -> opt {fclCommand = Typecheck})) "Type check and print top-level types"
--  , Option []  ["eval"]              (NoArg (\opt -> opt {fclCommand = Eval})) "Run interpreter (entry-point: main)"
  , Option "o" ["output"]            (ReqArg (\out -> (\opt -> opt {fclOutputFile = out})) "FILE") "Specify stem of output files. Writes files <name>.c and <name>.cl)"
  , Option []  ["test-parser"]       (NoArg (\opt -> opt {fclCommand = TestParser})) "Parse, pretty print, parse again, check for equality."
  , Option []  ["dump-parsed"]    (NoArg (\opt -> opt {fclCommand = DumpParsed})) "Parse. Print."
  , Option []  ["dump-desugared"]    (NoArg (\opt -> opt {fclCommand = DumpDesugared})) "Parse. Desugar. Print."
  , Option []  ["dump-typed"]        (NoArg (\opt -> opt {fclCommand = DumpTyped})) "Dump AST after typing."
  , Option []  ["dump-monomorphed"]  (NoArg (\opt -> opt {fclCommand = DumpMonomorphed})) "Dump AST after monomorphization."
  , Option []  ["dump-il"]           (NoArg (\opt -> opt {fclCommand = DumpIL})) "Dump intermediate representation."
  , Option []  ["dump-il-optimised"] (NoArg (\opt -> opt {fclCommand = DumpILOptimised})) "Dump intermediate representation."
  , Option "v" ["verbose"]           (NoArg (\opt -> opt {fclVerbosity = 2})) "Verbose output"
  , Option "d" ["debug"]             (NoArg (\opt -> opt {fclVerbosity = 3})) "Debugging output"
  , Option "p" ["profile"]           (NoArg (\opt -> opt {fclProfile = True})) "Profile kernel calls & data transfers."
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
  asts <- mapM parseFile files
  return (E.concatPrograms asts)

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

testParser :: [FilePath] -> CLI ()
testParser filenames =
 let checkfile filename =
       do logInfo "Testing parser."
          logDebug "Input:"
          contents <- readFileChecked filename
          logDebug contents

          ast <- parseFile filename
          logInfo "Pretty printing."          
          let pp1 = display ast

          logInfo "Parsing again."
          result <- liftEither FCLError (parse "<<input>>" pp1)
          let pp2 = display result

          if pp1 == pp2
            then message ("Parser test of '" ++ filename ++ "': OK.")
            else do message ("Parser test of '" ++ filename ++ "': ERROR. Pretty printed output differed after second parse.")
                    message "After first parse:"
                    message pp1
                    message "After second parse:"
                    message pp2
     
                    liftIO exitFailure
 in mapM_ checkfile filenames

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

     onCommand TestParser (testParser filenames)

     prelude  <- liftIO (getDataFileName "lib/prelude.fcl")
     let files = if fclNoPrelude opts
                   then filenames
                   else prelude : filenames

     ast <- parseFiles files
     onCommand DumpParsed (message (display ast))

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

     let intermediate_prog = compile monomorphed
     onCommand DumpIL (message (prettyIL intermediate_prog))

     optIter <- asks fclOptimizeIterations
     let optimised = optimise optIter intermediate_prog

     onCommand DumpILOptimised (message (prettyIL optimised))

     logInfo "Typechecking IL."
     type_env <- liftEither FCLError (typecheckIL optimised)

     verbosity <- asks fclVerbosity
     outputFile <- asks fclOutputFile
     profile <- asks fclProfile
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

