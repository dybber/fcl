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
-- import qualified FCL.External.Syntax as E
--import qualified FCL.Core.Syntax as C

---------------
---- Monad ----
---------------
type CLI = ReaderT Options (ExceptT CompilerError IO)

data CompilerError = IOError IOError
                   | ParseError ParseError
                   | TypeError TypeError
                   | EvalError String
                   | CLIError String
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

throw :: CompilerError -> CLI a
throw err = lift (throwE err)

data Command = ParserTest
             | Typecheck
             | Eval
             | Compile
             | Help
             | DumpAST
             | DumpASTUntyped
             | DumpASTInlined
             | DumpASTDesugared
             | DumpASTSimplified
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
  , Option []  ["eval"]             (NoArg (\opt -> opt {fclCommand = Eval})) "Run interpreter (entry-point: main)"
  , Option "o" ["output"]           (ReqArg (\out -> (\opt -> opt {fclOutputFile = out})) "FILE") "Specify stem of output files. Writes files <name>.c and <name>.cl)"
  , Option []  ["test-parser"]     (NoArg (\opt -> opt {fclCommand = ParserTest})) "Parse, pretty print, parse again, check for equality."
  , Option []  ["dump-ast-untyped"] (NoArg (\opt -> opt {fclCommand = DumpASTUntyped})) "Dump AST after parsing"
  , Option []  ["dump-ast-inlined"] (NoArg (\opt -> opt {fclCommand = DumpASTInlined})) "Dump AST after parsing and inlining"
  , Option []  ["dump-ast-desugared"] (NoArg (\opt -> opt {fclCommand = DumpASTDesugared})) "Dump AST after desugaring"
  , Option []  ["dump-ast-simplified"] (NoArg (\opt -> opt {fclCommand = DumpASTSimplified})) "Dump AST after parsing, inlining and simplifications."
  , Option []  ["dump-il"]          (NoArg (\opt -> opt {fclCommand = DumpIL})) "Dump intermediate representation."
  , Option []  ["dump-il-optimised"] (NoArg (\opt -> opt {fclCommand = DumpILOptimised})) "Dump intermediate representation."
  , Option []  ["dump-ast"]         (NoArg (\opt -> opt {fclCommand = DumpAST})) "Dump AST after typechecking"
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

parseFile :: String -> CLI [E.Definition E.Untyped]
parseFile fname = do
  contents <- readFileChecked fname
  logInfo ("Parsing " ++ fname ++ ".")
  liftEither ParseError (parseTopLevel fname contents)

parseFiles :: [String] -> CLI [E.Definition E.Untyped]
parseFiles files = do
  definitions <- mapM parseFile files
  return (concat definitions)

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

printTypes :: [E.Definition Type] -> CLI ()
printTypes ast = mapM_ (message . showType) ast

pp :: [E.Definition a] -> CLI ()
pp ast = message (prettyPrint ast)

evalAndPrint :: [C.Definition C.Type] -> CLI ()
evalAndPrint ast =
  do v <- liftEither EvalError (return (eval ast))
     liftIO (print v)

parserTest :: [FilePath] -> CLI ()
parserTest filenames =
 let checkfile filename =
       do logInfo "Testing parser."
          logDebug "Input:"
          contents <- readFileChecked filename
          logDebug contents

          ast <- parseFile filename
          logInfo "Pretty printing."          
          let pp1 = prettyPrint ast

          logInfo "Parsing again."
          result <- liftEither ParseError (parseTopLevel "<<input>>" pp1)
          let pp2 = prettyPrint result

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

     onCommand ParserTest (parserTest filenames)

     prelude  <- liftIO (getDataFileName "lib/prelude.fcl")
     let files = if fclNoPrelude opts
                   then filenames
                   else prelude : filenames

     ast <- parseFiles files
     onCommand DumpASTUntyped (dumpAST ast)
     
--      logInfo "Typechecking."
--      typed_ast <- liftEither TypeError (typeinfer ast)
--      onCommand DumpAST     (dumpAST typed_ast)
--      onCommand PrettyPrint (pp typed_ast)
--      onCommand Typecheck   (printTypes typed_ast)

--      logInfo "Typechecking again."
--      inlined_typed <- liftEither TypeError (typeinfer [inlined])
--      let desugared_ast = map desugarDefinition inlined_typed
--      desugared_typed <- liftEither TypeErrorCore (typeinferCore desugared_ast)
     
--      onCommand DumpASTDesugared (message (show desugared_typed))
     
--      -- mapM_ (logInfo . (" " ++) . showType) typed_ast3
     
--      -- logInfo "Simplifying."
--      -- let simpl = simplify defaultCompileConfig (head typed_ast2)
     
--      -- logInfo "Typechecking again."
--      -- typed_ast3 <- liftEither TypeErrorCore (typeinferCore [simpl])
--      -- -- mapM_ (logInfo . (" " ++) . showType) typed_ast3

-- --     onCommand DumpASTSimplified (message (show [desugared_ast]))
--      onCommand Eval        (evalAndPrint desugared_typed)

--      logInfo "Compiling."
--      optIter <- asks fclOptimizeIterations
--      verbosity <- asks fclVerbosity
--      outputFile <- asks fclOutputFile
--      let cfile = outputFile ++ ".c"
--          kernelsFile = outputFile ++ ".cl"
--          cfg = defaultCompileConfig { configKernelsFilename = kernelsFile
--                                     , configOptimizeIterations = optIter
--                                     , configVerbosity = verbosity
--                                     }
--          intermediate_prog = compile cfg desugared_typed
--      onCommand DumpIL (message (prettyIL intermediate_prog))

--      let optimised = optimise optIter intermediate_prog

--      onCommand DumpILOptimised (message (prettyIL optimised))

--      onCommand Compile $
--        do let (main_, kernels) = codeGen cfg optimised
--           writeFileChecked cfile main_
--           writeFileChecked kernelsFile kernels

