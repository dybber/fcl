module FCL
 (parseTopLevel, typeinfer, desugarDefinition, inline,
  eval,
  compile, pretty,

  showType, prettyPrintType, prettyPrintExp, prettyPrint,
  
--  Definition, Untyped, Type,

  CompileConfig(..), defaultCompileConfig,
  
  TypeError, ParseError
 )
where

import FCL.External.Parser (parseTopLevel, ParseError)
import FCL.Infer           (typeinfer, TypeError)
import FCL.Desugaring      (desugarDefinition)
import FCL.Inline          (inline)
import FCL.Compile         (compile)
--import FCL.Core.Syntax     (Definition, Type, Untyped)
import FCL.Compile.Config  (CompileConfig(..), defaultCompileConfig)
import FCL.External.Pretty (showType, prettyPrintType, prettyPrintExp, prettyPrint)
import FCL.Eval            (eval)
import CGen                (pretty)
