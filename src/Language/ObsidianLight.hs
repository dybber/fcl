module Language.ObsidianLight
 ( module Language.ObsidianLight.SmartCons,
   module Language.ObsidianLight.Library,
   compile, compileAndPrint, eval, Kernel(..), E.Value(..)
 ) 
where

import Language.ObsidianLight.SmartCons
import Language.ObsidianLight.Library

import qualified Language.ObsidianLight.Eval        as E (interp, emptyEnv, Value(..))
import qualified Language.ObsidianLight.TypeChecker as T (typecheck)
import qualified Language.ObsidianLight.Compile     as C (compile)

import Language.GPUIL.Syntax (Kernel(..))
import Language.GPUIL (renderKernel)

compile :: String -> Obs a -> IO Kernel
compile name e = do
  putStrLn ("Unfolding Smart constructors: " ++ name)
  let uexp = runObs e
  print uexp
  putStrLn ("Typechecking: " ++ name)
  let (texp, _) = T.typecheck uexp
  print texp
  putStrLn ("Compiling: " ++ name)
  C.compile name texp

eval :: Obs a -> E.Value Untyped
eval e =
  let uexp = runObs e
  in E.interp E.emptyEnv uexp

compileAndPrint :: String -> Obs a -> IO ()
compileAndPrint name e = do
  kernel <- compile name e
  putStrLn "Output:"
  print kernel
  putStrLn ""
  putStrLn (renderKernel kernel)
