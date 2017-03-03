module FCL
 (parse,
  desugar,
  infer,
  Infer.initialTypeEnvironment,
  monomorph,
--  eval,

-- backend
  compile, optimise, codeGen, 

  -- pretty printing
  prettyIL, prettyC,
  display, displayTopLevelUntyped, displayTopLevelPoly, displayTopLevelMono,
  
--  Definition, Untyped, Type,

  CompileConfig(..), defaultCompileConfig,
  
  FCLError(..)
 )
where

import qualified FCL.External.Syntax as Ext
import qualified FCL.Core.Untyped as Untyped
import qualified FCL.Core.Polytyped as Poly
import qualified FCL.Core.Monotyped as Mono

import qualified FCL.External.Parser as Parse
import qualified FCL.Desugaring as Desugar
import qualified FCL.Infer as Infer
import qualified FCL.Monomorphization as Monomorph

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

data FCLError = ParseError Parse.ParseError
              | DesugarError Desugar.DesugarError
              | TypeError Infer.TypeError
              | MonomorphError Monomorph.MonomorphError
  deriving Show

liftEither :: (err -> FCLError) -> Either err a -> Either FCLError a
liftEither f (Left l) = Left (f l)
liftEither _ (Right r) = return r

parse :: String -> String -> Either FCLError Ext.Program
parse filename programText = liftEither ParseError (Parse.parseProgram filename programText)

desugar :: Ext.Program -> Either FCLError Untyped.Exp
desugar e = liftEither DesugarError (Desugar.desugar e)

infer :: Infer.TypeEnvironment -> Untyped.Exp -> Either FCLError (Poly.TypeScheme, Poly.Exp)
infer env e = liftEither TypeError (Infer.typeinfer env e)

monomorph :: Infer.TypeEnvironment -> Poly.Exp -> Either FCLError Mono.Exp
monomorph env e = liftEither MonomorphError (Monomorph.monomorph (Monomorph.mkInitEnv env) e)

display :: Pretty a => a -> String
display x = displayS (renderPretty 0.4 80 (pretty x)) ""

displayTopLevelUntyped :: Untyped.Exp -> String
displayTopLevelUntyped x = displayS (renderPretty 0.6 80 (prettyTopLevelUntyped x)) ""

displayTopLevelPoly :: Poly.Exp -> String
displayTopLevelPoly x = displayS (renderPretty 0.6 80 (prettyTopLevel x)) ""

displayTopLevelMono :: Mono.Exp -> String
displayTopLevelMono x = displayS (renderPretty 0.6 80 (prettyTopLevelMono x)) ""

prettyC = CGen.pretty
