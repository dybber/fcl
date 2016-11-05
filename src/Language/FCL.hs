module Language.FCL
 (parseTopLevel, typeinfer, inline, simplify,
  eval,
  compileKernels, pretty,

  showType, prettyPrintType, prettyPrintExp, prettyPrint,
  
  Definition, Untyped, Type,
  
  TypeError, ParseError
 )
where

import Language.FCL.Parser      (parseTopLevel, ParseError)
import Language.FCL.TypeInference (typeinfer, TypeError)
import Language.FCL.Inline      (inline)
import Language.FCL.Simplify    (simplify)
import Language.FCL.Compile     (compileKernels)
import Language.FCL.Syntax      (Definition, Type, Untyped)
import Language.FCL.Pretty      (showType, prettyPrintType, prettyPrintExp, prettyPrint)
import Language.FCL.Eval        (eval)
import CGen                     (pretty)
