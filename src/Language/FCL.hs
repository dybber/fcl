module Language.FCL
 ( module Language.FCL.SmartCons,
   module Language.FCL.Library,
   Flags(..), flagDebug,
   compile, eval, Kernel(..), Value(..), renderKernel,
   compileFromFile, compileFromFiles, compileAndPrint
 ) 
where

import Language.FCL.SmartCons
import Language.FCL.Library

import Language.FCL.Parser      (parseFile)
import Language.FCL.TypeChecker (typecheck)
import Language.FCL.Eval        (interp, emptyEnv, Value(..))
import Language.FCL.Compile     (compile)
import Language.FCL.Syntax      (Definition(Definition), Prog)

import Language.GPUIL.Syntax    (Kernel(..))
import Language.GPUIL           (renderKernel)

data Flags = Flags { verbosity :: Int }

flagDebug :: Flags
flagDebug = Flags { verbosity = 3 }

-- logError :: Flags -> String -> IO ()
-- logError flags msg = 
--   if verbosity flags >= 0
--   then putStrLn msg
--   else return ()

-- logWarn :: Flags -> String -> IO ()
-- logWarn flags msg =
--   if verbosity flags >= 1
--   then putStrLn msg
--   else return ()

logInfo :: Flags -> String -> IO ()
logInfo flags msg =
  if verbosity flags >= 2
  then putStrLn msg
  else return ()

logDebug :: Flags -> String -> IO ()
logDebug flags msg =
  if verbosity flags >= 3
  then putStrLn msg
  else return ()

parseFiles :: Flags -> [String] -> IO (Prog Untyped)
parseFiles _     [] = return []
parseFiles flags (f:fs) = do
  logInfo flags ("Parsing " ++ f ++ ".")
  p <- parseFile f
  rest <- parseFiles flags fs
  return (p ++ rest)

compileFromFiles :: Flags -> [String] -> IO String
compileFromFiles flags files =
  do prog <- parseFiles flags files
     logInfo flags "Parsing done. "
     logDebug flags "Parser output: "
     mapM_ (logDebug flags . show) prog
     logInfo flags "Typechecking."
     let (e, _) = typecheck prog
     logInfo flags "Typechecking done."
     logDebug flags (show e)
     logInfo flags "Compiling."
     cp <- compile "blah" e
     return (renderKernel cp)

compileFromFile :: Flags -> String -> IO String
compileFromFile flags filename = compileFromFiles flags [filename]

compileSmart :: String -> Obs a -> IO Kernel
compileSmart name e = do
  putStrLn ("Unfolding Smart constructors: " ++ name)
  let uexp = runObs e
  print uexp
  putStrLn ("Typechecking: " ++ name)
  let (texp, _) = typecheck [Definition name Nothing uexp]
  print texp
  putStrLn ("Compiling: " ++ name)
  compile name texp

eval :: Obs a -> Value Untyped
eval e =
  let uexp = runObs e
  in interp emptyEnv uexp

compileAndPrint :: String -> Obs a -> IO ()
compileAndPrint name e = do
  kernel <- compileSmart name e
  putStrLn "Output:"
  print kernel
  putStrLn ""
  putStrLn (renderKernel kernel)
