module FCL
 (parseTopLevel, typeinfer, typeinferCore, desugarDefinition, inline,
  eval,
  compile, optimise, codeGen, prettyIL,

  showType, prettyPrintType, prettyPrintExp, prettyPrint,

  prettyC,
  
--  Definition, Untyped, Type,

  CompileConfig(..), defaultCompileConfig,
  
  TypeError, TypeErrorCore, ParseError
 )
where

import FCL.External.Parser (parseTopLevel, ParseError)
import FCL.Infer           (typeinfer, TypeError)
import FCL.Desugaring      (desugarDefinition)
import FCL.Inline          (inline)
import FCL.Compile         (compile)
import FCL.IL.Optimise     (optimise)
import FCL.IL.CodeGen      (codeGen)
import FCL.IL.Pretty       (prettyIL)
--import FCL.Core.Syntax     (Definition, Type, Untyped)
import FCL.TypeInference (typeinferCore, TypeErrorCore)
import FCL.Compile.Config  (CompileConfig(..), defaultCompileConfig)
import FCL.External.Pretty (showType, prettyPrintType, prettyPrintExp, prettyPrint)
import FCL.Eval            (eval)
import CGen                (pretty)

prettyC = pretty
