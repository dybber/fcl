module FCL.Compile (compile) where

import FCL.Core.Literal
import FCL.Core.Monotyped

import FCL.Compile.Value
import FCL.Compile.CompileEnvironment
import FCL.Compile.Config

import FCL.IL.Cons
import FCL.IL.Syntax (Stmt)
import FCL.IL.Program (runProgram)

compile :: CompileConfig -> Exp -> [Stmt ()]
compile _ e =
  case compileExp defaultCompileEnvironment e of
    TagProgram p -> fst (runProgram p)
    _ -> error "'main'-function should return a value of type \"Program <grid> 'a\" for some 'a."

compileLiteral :: Literal -> Value
compileLiteral (LiteralInt i)        = TagInt (int i)
compileLiteral (LiteralDouble d)     = TagDouble (double d)
compileLiteral (LiteralBool b)       = TagBool (bool b)
compileLiteral (LiteralString s)     = TagString (string s)

compileExp :: CompileEnv -> Exp -> Value
compileExp _ (Literal l _) = compileLiteral l
compileExp _ (Unit _)      = TagUnit
compileExp env (App f e _) = app1 (compileExp env f) (compileExp env e)
compileExp env (Lamb x e _) = TagFn (\v -> compileExp (insertUserDefined x v env) e)
compileExp env (Let x e0 e1 _)    =
  let v = compileExp env e0
  in compileExp (insertUserDefined x v env) e1
compileExp env (Pair e0 e1 _)     = TagPair (compileExp env e0) (compileExp env e1)
compileExp env (Symbol x ty) =
  case lookupSymbol x ty env of
    Just v -> v
    Nothing -> error ("Undefined variable " ++ x)
compileExp env (Cond e0 e1 e2 _) =
  let b0 = unBool (compileExp env e0)
      thenBranch = compileExp env e1
      elseBranch = compileExp env e2
  in case (thenBranch, elseBranch) of
       (TagInt i1, TagInt i2) -> TagInt (if_ b0 i1 i2)
       (TagDouble d1, TagDouble d2) -> TagBool (if_ b0 d1 d2)
       (TagBool b1, TagBool b2) -> TagBool (if_ b0 b1 b2)
       (TagString s1, TagString s2) -> TagString (if_ b0 s1 s2)
       (TagArray _, TagArray _) -> error "ArrayInConditional"
       (TagFn _, TagFn _) -> error "FunctionInConditional"
       (TagProgram _, TagProgram _) -> error "ProgramInConditional"
       (_,_) -> error "Differing branches, should have been caught in type check"
