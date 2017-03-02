module FCL
 (parseProgram, typeinfer, desugar,
--  eval,
  compile, optimise, codeGen, prettyIL,

  displayTopLevelUntyped, displayTopLevelPoly, displayTopLevelMono,

  prettyC,
  
--  Definition, Untyped, Type,

  CompileConfig(..), defaultCompileConfig,
  
  TypeError, ParseError
 )
where

-- import qualified FCL.External.Syntax as Ext
import qualified FCL.Core.Untyped as Untyped
import qualified FCL.Core.Polytyped as Poly
import qualified FCL.Core.Monotyped as Mono

  
import FCL.External.Parser (parseProgram, ParseError)
import FCL.Infer           (typeinfer, TypeError)
import FCL.Desugaring      (desugar)
import FCL.Compile         (compile)
import FCL.IL.Optimise     (optimise)
import FCL.IL.CodeGen      (codeGen)
import FCL.IL.Pretty       (prettyIL)
--import FCL.Core.Syntax     (Definition, Type, Untyped)
import FCL.Compile.Config  (CompileConfig(..), defaultCompileConfig)

import Text.PrettyPrint.Leijen
import FCL.Pretty
--import FCL.Eval            (eval)
import qualified CGen                (pretty)

prettyC = CGen.pretty

display :: Pretty a => a -> String
display x = displayS (renderPretty 0.4 80 (pretty x)) ""

displayTopLevelUntyped :: Untyped.Exp -> String
displayTopLevelUntyped x = displayS (renderPretty 0.6 80 (prettyTopLevelUntyped x)) ""

displayTopLevelPoly :: Poly.Exp -> String
displayTopLevelPoly x = displayS (renderPretty 0.6 80 (prettyTopLevel x)) ""

displayTopLevelMono :: Mono.Exp -> String
displayTopLevelMono x = displayS (renderPretty 0.6 80 (prettyTopLevelMono x)) ""
