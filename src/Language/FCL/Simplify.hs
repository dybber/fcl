module Language.FCL.Simplify (simplify, simplifyExp) where

import Language.FCL.Syntax
import Language.FCL.SourceRegion


-- Right now, types are not maintained correctly by the simplifier
simplify :: Program ty -> Program ty
simplify = map simplifyDef

simplifyDef :: Definition ty -> Definition ty
simplifyDef d = d { defBody = simplifyExp (defBody d) }

simplifyExp :: Exp ty -> Exp ty
simplifyExp e@(IntScalar _ _)    = e
simplifyExp e@(DoubleScalar _ _) = e
simplifyExp e@(BoolScalar _ _)   = e
simplifyExp e@(LocalSize _)      = e
simplifyExp e@(Var _ _ _)        = e
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
--simplifyExp (Let x1 e1 e2 ty r) = simplifyExp (apply (x1,e2) e1)
simplifyExp (Let x1 e1 e2 ty r) = Let x1 (simplifyExp e1) (simplifyExp e2) ty r
simplifyExp (UnOp op e r) = simplifyUnOp op e r
simplifyExp (BinOp op e1 e2 r) = simplifyBinOp op e1 e2 r
simplifyExp (Cond econd etrue efalse ty r) =
  case simplifyExp econd of
    BoolScalar True  _ -> simplifyExp etrue
    BoolScalar False _ -> simplifyExp efalse
    econd' -> Cond econd' (simplifyExp etrue) (simplifyExp efalse) ty r
simplifyExp (Vec es ety reg)           = Vec          (map (simplifyExp) es) ety reg
simplifyExp (Pair e0 e1 reg)           = Pair         (simplifyExp e0) (simplifyExp e1) reg
simplifyExp (Proj1E e0 reg)            = Proj1E       (simplifyExp e0) reg
simplifyExp (Proj2E e0 reg)            = Proj2E       (simplifyExp e0) reg
simplifyExp (Index e0 e1 reg)          = Index        (simplifyExp e0) (simplifyExp e1) reg
simplifyExp (LengthPull e0 reg)        = LengthPull   (simplifyExp e0) reg
simplifyExp (LengthPush e0 reg)        = LengthPush   (simplifyExp e0) reg
simplifyExp (While e0 e1 e2 reg)       = While        (simplifyExp e0) (simplifyExp e1) (simplifyExp e2) reg
simplifyExp (WhileSeq e0 e1 e2 reg)    = WhileSeq     (simplifyExp e0) (simplifyExp e1) (simplifyExp e2) reg
simplifyExp (GeneratePull e0 e1 reg)   = GeneratePull (simplifyExp e0) (simplifyExp e1) reg
simplifyExp (MapPull e0 e1 reg)        = MapPull      (simplifyExp e0) (simplifyExp e1) reg
simplifyExp (MapPush e0 e1 reg)        = MapPush      (simplifyExp e0) (simplifyExp e1) reg
simplifyExp (Push lvl e0 t reg)        = Push         lvl (simplifyExp e0) t reg
simplifyExp (Force e0 reg)             = Force        (simplifyExp e0) reg
simplifyExp (Concat e0 e1 reg)         = Concat       (simplifyExp e0) (simplifyExp e1) reg
simplifyExp (Interleave e0 e1 e2 reg)  = Interleave   (simplifyExp e0) (simplifyExp e1) (simplifyExp e2) reg
simplifyExp (Scanl e0 e1 e2 reg)       = Scanl        (simplifyExp e0) (simplifyExp e1) (simplifyExp e2) reg

simplifyUnOp :: UnOp -> Exp ty -> Region -> Exp ty
simplifyUnOp op e r = UnOp op (simplifyExp e) r

simplifyBinOp :: BinOp -> Exp ty -> Exp ty -> Region -> Exp ty
simplifyBinOp op e1 e2 r = BinOp op (simplifyExp e1) (simplifyExp e2) r
