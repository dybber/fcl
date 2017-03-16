module FCL
 (
  -- Frontend
  parse,
  desugar,
  infer,
  Infer.initialTypeEnvironment,
  monomorph,

  --  eval,

  -- Backend
  compile, optimise, codeGen, typecheckIL,
  CompileConfig(..), defaultCompileConfig,

  -- Pretty printers
  prettyIL, prettyC,
  display, displayTopLevelUntyped, displayTopLevelPoly, displayTopLevelMono,
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
import qualified FCL.IL.TypeCheck as IL  (typecheck, TypeEnv)
import FCL.IL.Syntax       (ILProgram)
import FCL.IL.Pretty       (prettyIL)
import FCL.Compile.Config  (CompileConfig(..), defaultCompileConfig)
import FCL.Error (FCLError(..))
import FCL.Pretty

--import FCL.Eval            (eval)
import qualified CGen                (pretty)

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

prettyC = CGen.pretty


typecheckIL :: ILProgram a -> Either FCLError IL.TypeEnv
typecheckIL prog = liftEither TypeErrorIL (IL.typecheck prog)
