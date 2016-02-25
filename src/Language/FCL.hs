module Language.FCL
 ( Flags(..), flagDebug,
   compileKernel, compileKernels, Kernel(..), renderKernel,
   compileFromFile, compileFromFiles
 ) 
where

import Language.FCL.Parser      (parseFile)
import Language.FCL.TypeInference (typeinfer)
import Language.FCL.Inline      (inline)
import Language.FCL.Compile     (compileKernel, compileKernels)
import Language.FCL.Syntax      (Program, typeOf, Exp, Type, Untyped, Definition(..))
import Language.FCL.PrettyPrint (prettyPrintType)

import Language.GPUIL.Syntax    (Kernel(..))
import Language.GPUIL           (renderKernel)

data Flags = Flags { verbosity :: Int
                   , optIter :: Int
                   }

flagDebug :: Flags
flagDebug = Flags { verbosity = 3
                  , optIter = 10
                  }

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

parseFiles :: Flags -> [String] -> IO (Program Untyped)
parseFiles _     [] = return []
parseFiles flags (f:fs) = do
  logInfo flags ("Parsing " ++ f ++ ".")
  p <- parseFile f
  rest <- parseFiles flags fs
  return (p ++ rest)

showType :: Definition Type -> String
showType (Definition v _ _ _ e) = v ++ " : " ++ prettyPrintType (typeOf e)

compileFromFiles :: Flags -> [String] -> IO String
compileFromFiles flags files =
  do prog <- parseFiles flags files
     logInfo flags "Parsing done. "
     logInfo flags "Typechecking."
     let es = typeinfer prog
     mapM_ (logDebug flags . (" " ++) . showType) es
     logInfo flags "Typechecking done."
     logInfo flags "Inlining."
     let esInline = inline es
     logInfo flags (show (length esInline) ++ " kernels")
     logInfo flags "Inlining done."
     -- logInfo flags "Typechecking again."
     -- let es = typeinferProg prog
     -- mapM_ (logDebug flags . (" " ++) . showType) es
     -- logInfo flags "Typechecking done."
     logDebug flags (show esInline)
     logInfo flags "Compiling."
     cp <- compileKernels (optIter flags) esInline
     return (unlines $ Prelude.map renderKernel cp)

compileFromFile :: Flags -> String -> IO String
compileFromFile flags filename = compileFromFiles flags [filename]
