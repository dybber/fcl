module Language.FCL
 (parseTopLevel, initTyVars, typeinfer, inline, simplify,
  eval,
  compileKernels, pretty,

  showType, prettyPrintType, prettyPrintExp, prettyPrintProgram,
  
  Program, Untyped, Type,
  
  TypeError, ParseError
 )
where

import Language.FCL.Parser      (parseTopLevel, ParseError)
import Language.FCL.InitTyVars  (initTyVars)
import Language.FCL.TypeInference (typeinfer, TypeError)
import Language.FCL.Inline      (inline)
import Language.FCL.Simplify    (simplify)
import Language.FCL.Compile     (compileKernels)
import Language.FCL.Syntax      (Program, Type, Untyped)
import Language.FCL.Pretty      (showType, prettyPrintType, prettyPrintExp, prettyPrintProgram)
import Language.FCL.Eval        (eval)
import CGen                     (pretty)
