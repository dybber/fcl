module Language.FCL
 (parseTopLevel, infer, inline, eval,
  compileKernels, renderKernel, showType,
  
  Program, Untyped, Type,
  
  TypeError, ParseError, CompilerError(..)
 )
where

import Language.FCL.Parser      (parseTopLevel, ParseError)
import Language.FCL.TypeInference (typeinfer)
import Language.FCL.Inline      (inline)
import Language.FCL.Compile     (compileKernels)
import Language.FCL.Syntax      (Program, typeOf, Type, Untyped, Definition(..))
import Language.FCL.PrettyPrint (prettyPrintType)

import Language.FCL.Eval        (eval)

import Language.GPUIL           (renderKernel)


data CompilerError = IOError IOError
                   | ParseError ParseError
                   | TypeError TypeError
                   | EvalError String
  deriving Show


data TypeError = UnificationError String
  deriving (Show, Eq)

infer :: Program Untyped -> Either TypeError (Program Type)
infer prog = return (typeinfer prog)

showType :: Definition Type -> String
showType (Definition v _ _ _ e) = v ++ " : " ++ prettyPrintType (typeOf e)
