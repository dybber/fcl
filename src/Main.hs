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

import Language.FCL

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
             | PrettyPrint
             | Eval
             | Compile
             | Help
             | DumpAST
             | DumpASTUntyped
             | DumpASTSimplified
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
          , fclOutputFile = "kernels.cl"
          }

optionDescriptions :: [OptDescr (Options -> Options)]
optionDescriptions = 
  [ Option "h" ["help"]             (NoArg (\opt -> opt {fclCommand = Help})) "Show help for this command"
  , Option []  ["no-prelude"]   (NoArg (\opt -> opt {fclNoPrelude = True})) "Do not include the FCL prelude"
  , Option []  ["prettyprint"]      (NoArg (\opt -> opt {fclCommand = PrettyPrint})) "Typecheck and prettyprint"
  , Option "t" ["typecheck"]        (NoArg (\opt -> opt {fclCommand = Typecheck})) "Type check and print top-level types"
  , Option []  ["eval"]             (NoArg (\opt -> opt {fclCommand = Eval})) "Run interpreter (entry-point: main)"
  , Option "o" ["output"]           (ReqArg (\out -> (\opt -> opt {fclOutputFile = out})) "FILE") "where to emit OpenCL kernels"
  , Option []  ["test-parser"]     (NoArg (\opt -> opt {fclCommand = ParserTest})) "Parse, pretty print, parse again, check for equality."
  , Option []  ["dump-ast-untyped"] (NoArg (\opt -> opt {fclCommand = DumpASTUntyped})) "Dump AST after parsing"
  , Option []  ["dump-ast-simplified"] (NoArg (\opt -> opt {fclCommand = DumpASTSimplified})) "Dump AST after parsing"
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
readFileSafe :: FilePath -> CLI String
readFileSafe filename = do
  contents <- liftIO (tryIOError (readFile filename))
  liftEither IOError contents

parseFile :: String -> CLI [Definition Untyped]
parseFile fname = do
  contents <- readFileSafe fname
  logInfo ("Parsing " ++ fname ++ ".")
  liftEither ParseError (parseTopLevel fname contents)

parseFiles :: [String] -> CLI [Definition Untyped]
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

dumpAST :: Show a => [Definition a] -> CLI ()
dumpAST ast = message (show ast)

printTypes :: [Definition Type] -> CLI ()
printTypes ast = mapM_ (message . showType) ast

pp :: [Definition a] -> CLI ()
pp ast = message (prettyPrint ast)

evalAndPrint :: [Definition Type] -> CLI ()
evalAndPrint ast =
  do v <- liftEither EvalError (return (eval ast))
     liftIO (print v)

compileAndWrite :: [Definition Type] -> CLI ()
compileAndWrite ast =
  do logInfo "Inlining."
     let inlined = inline ast
     logInfo (show (length inlined) ++ " kernels.")

     logInfo "Simplifying."
     let simpl = simplify inlined

     logInfo "Typechecking again."
     typed_ast <- liftEither TypeError (typeinfer simpl)
     mapM_ (logInfo . (" " ++) . showType) typed_ast

     onCommand DumpASTSimplified (dumpAST typed_ast)
     
     logInfo "Compiling."
     optIter <- asks fclOptimizeIterations
     let (main, kernels) = compile optIter typed_ast
     outputFile <- asks fclOutputFile
     liftIO (writeFile outputFile main)
     liftIO (writeFile "kernels.cl" kernels)

parserTest :: [FilePath] -> CLI ()
parserTest filenames =
 let checkfile filename =
       do logInfo "Testing parser."
          logDebug "Input:"
          contents <- readFileSafe filename
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
     
     logInfo "Typechecking."
     typed_ast <- liftEither TypeError (typeinfer ast)

     onCommand DumpAST     (dumpAST typed_ast)
     onCommand PrettyPrint (pp typed_ast)
     onCommand Typecheck   (printTypes typed_ast)
     onCommand Eval        (evalAndPrint typed_ast)
     onCommand Compile     (compileAndWrite typed_ast)
