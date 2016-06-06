module Language.FCL.Simplify (simplify, simplifyExp) where

import Language.FCL.Syntax
import Language.FCL.SourceRegion


-- Right now, types are not maintained correctly by the simplifier
simplify :: Program ty -> Program ty
simplify = map simplifyDef

simplifyDef :: Definition ty -> Definition ty
simplifyDef d = d { defBody = simplifyExp (defBody d) }

simplifyExp :: Exp ty -> Exp ty
simplifyExp (UnOp op e r) = simplifyUnOp op e r
simplifyExp (BinOp op e1 e2 r) = simplifyBinOp op e1 e2 r
simplifyExp (App (Lamb x _ ebody _ _) e) = simplifyExp (apply (x, ebody) e)
simplifyExp (App e1 e2) =
  let e1' = simplifyExp e1
      e2' = simplifyExp e2
  in case e1' of
       (Lamb x _ ebody _ _) -> simplifyExp (apply (x, ebody) e2')
       _ -> App e1' e2'
simplifyExp (Lamb x1 _ (App ebody (Var x2 _ _)) _ _)
 | x1 == x2 && not (x1 `freeIn` ebody) = simplifyExp ebody
simplifyExp (Lamb x1 tya ebody tyr r) = Lamb x1 tya (simplifyExp ebody) tyr r
simplifyExp (Cond econd etrue efalse ty r) =
  case simplifyExp econd of
    BoolScalar True  _ -> simplifyExp etrue
    BoolScalar False _ -> simplifyExp efalse
    econd' -> Cond econd' (simplifyExp etrue) (simplifyExp efalse) ty r
simplifyExp (GeneratePull e1 e2 r) = GeneratePull (simplifyExp e1) (simplifyExp e2) r
simplifyExp e = e

simplifyUnOp :: UnOp -> Exp ty -> Region -> Exp ty
simplifyUnOp op e r = UnOp op (simplifyExp e) r

simplifyBinOp :: BinOp -> Exp ty -> Exp ty -> Region -> Exp ty
simplifyBinOp op e1 e2 r = BinOp op (simplifyExp e1) (simplifyExp e2) r
