module Language.FCL
 (parseTopLevel, typeinfer, inline, eval,
  compileKernels, renderKernel, showType,
  
  Program, Untyped, Type,
  
  TypeError, ParseError
 )
where

import Language.FCL.Parser      (parseTopLevel, ParseError)
import Language.FCL.TypeInference (typeinfer, TypeError)
import Language.FCL.Inline      (inline)
import Language.FCL.Compile     (compileKernels)
import Language.FCL.Syntax      (Program, Type, Untyped)
import Language.FCL.PrettyPrint (showType)
import Language.FCL.Eval        (eval)
import Language.GPUIL           (renderKernel)
