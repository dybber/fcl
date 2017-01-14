-- | A few simple simplification: beta-reduction, eta-conversion
-- and a few peep-hole optimizations.
module FCL.Simplify (simplify, simplifyExp) where

import FCL.Core.SourceRegion
import FCL.Core.Syntax
import FCL.Compile.Config
import FCL.Substitution (apply, freeIn)


-- Right now, types are not maintained correctly by the simplifier
simplify :: CompileConfig -> Definition ty -> Definition ty
simplify info d = d { defBody = simplifyExp info (defBody d) }

simplifyExp :: CompileConfig -> Exp ty -> Exp ty
simplifyExp _ e@(Literal _ _)    = e
simplifyExp cfg (BlockSize r)      = Literal (LiteralInt (configBlockSize cfg)) r
simplifyExp _ e@(Var _ _ _)        = e
simplifyExp cfg (App (Lamb x _ ebody _ _) e) = simplifyExp cfg (apply (x, ebody) e)
simplifyExp cfg (App e1 e2) =
  let e1' = simplifyExp cfg e1
      e2' = simplifyExp cfg e2
  in case e1' of
       (Lamb x _ ebody _ _) -> simplifyExp cfg (apply (x, ebody) e2')
       _ -> App e1' e2'
simplifyExp cfg (Lamb x1 _ (App ebody (Var x2 _ _)) _ _)
 | x1 == x2 && not (x1 `freeIn` ebody) = simplifyExp cfg ebody
simplifyExp cfg (Lamb x1 tya ebody tyr r) = Lamb x1 tya (simplifyExp cfg ebody) tyr r
--simplifyExp cfg (Let x1 e1 e2 _ _) = simplifyExp cfg (apply (x1,e2) e1)
simplifyExp cfg (Let x1 e1@(Lamb _ _ _ _ _) e2 _ _) = simplifyExp cfg (apply (x1,e2) e1)
simplifyExp cfg (Let x1 e1 e2 ty r) =
  let e1_simpl = simplifyExp cfg e1
  in if isLiteral e1_simpl
     then simplifyExp cfg (apply (x1,e2) e1_simpl)
     else Let x1 e1_simpl (simplifyExp cfg e2) ty r
simplifyExp cfg (UnaryOp op e r) = simplifyUnOp op (simplifyExp cfg e) r
simplifyExp cfg (BinaryOp op e1 e2 r) = simplifyBinOp op (simplifyExp cfg e1) (simplifyExp cfg e2) r
simplifyExp cfg (Cond econd etrue efalse ty r) =
  case simplifyExp cfg econd of
    Literal (LiteralBool True)  _ -> simplifyExp cfg etrue
    Literal (LiteralBool False) _ -> simplifyExp cfg efalse
    econd' -> Cond econd' (simplifyExp cfg etrue) (simplifyExp cfg efalse) ty r
simplifyExp cfg (Vec es ety reg)           = Vec          (map (simplifyExp cfg) es) ety reg
simplifyExp cfg (Pair e0 e1 reg)           = Pair         (simplifyExp cfg e0) (simplifyExp cfg e1) reg
simplifyExp cfg (Proj1E e0 reg)            = Proj1E       (simplifyExp cfg e0) reg
simplifyExp cfg (Proj2E e0 reg)            = Proj2E       (simplifyExp cfg e0) reg
simplifyExp cfg (Index e0 e1 reg)          = Index        (simplifyExp cfg e0) (simplifyExp cfg e1) reg
simplifyExp cfg (LengthPull e0 reg)        = LengthPull   (simplifyExp cfg e0) reg
simplifyExp cfg (LengthPush e0 reg)        = LengthPush   (simplifyExp cfg e0) reg
simplifyExp cfg (For e0 e1 e2 reg)         = For          (simplifyExp cfg e0) (simplifyExp cfg e1) (simplifyExp cfg e2) reg
simplifyExp cfg (Power e0 e1 e2 reg)       = Power        (simplifyExp cfg e0) (simplifyExp cfg e1) (simplifyExp cfg e2) reg
simplifyExp cfg (While e0 e1 e2 reg)       = While        (simplifyExp cfg e0) (simplifyExp cfg e1) (simplifyExp cfg e2) reg
simplifyExp cfg (WhileSeq e0 e1 e2 reg)    = WhileSeq     (simplifyExp cfg e0) (simplifyExp cfg e1) (simplifyExp cfg e2) reg
simplifyExp cfg (GeneratePull e0 e1 reg)   = GeneratePull (simplifyExp cfg e0) (simplifyExp cfg e1) reg
simplifyExp cfg (MapPull e0 e1 reg)        = MapPull      (simplifyExp cfg e0) (simplifyExp cfg e1) reg
simplifyExp cfg (MapPush e0 e1 reg)        = MapPush      (simplifyExp cfg e0) (simplifyExp cfg e1) reg
simplifyExp cfg (Push lvl e0 reg)          = Push         lvl (simplifyExp cfg e0) reg
simplifyExp cfg (Force e0 reg)             = Force        (simplifyExp cfg e0) reg
simplifyExp cfg (Interleave e0 e1 e2 reg)  = Interleave   (simplifyExp cfg e0) (simplifyExp cfg e1) (simplifyExp cfg e2) reg
simplifyExp cfg (LambLvl lvlvar ebody ty reg) = LambLvl lvlvar  (simplifyExp cfg ebody) ty reg
simplifyExp cfg (AppLvl e lvl) = AppLvl (simplifyExp cfg e) lvl
simplifyExp cfg (Return lvl e0 reg)        = Return         lvl (simplifyExp cfg e0) reg
simplifyExp cfg (Bind e0 e1 reg)           = Bind         (simplifyExp cfg e0) (simplifyExp cfg e1) reg
simplifyExp cfg (ReadIntCSV e0 reg)        = ReadIntCSV (simplifyExp cfg e0) reg
simplifyExp cfg (ForceAndPrint e0 e1 reg)  = ForceAndPrint (simplifyExp cfg e0) (simplifyExp cfg e1) reg
simplifyExp cfg (Benchmark e0 e1 reg)      = Benchmark (simplifyExp cfg e0) (simplifyExp cfg e1) reg

simplifyUnOp :: UnaryOperator -> Exp ty -> SourceRegion -> Exp ty
simplifyUnOp op e r = UnaryOp op e r

simplifyBinOp :: BinaryOperator -> Exp ty -> Exp ty -> SourceRegion -> Exp ty
simplifyBinOp MulI (Literal (LiteralInt v1) _) (Literal (LiteralInt v2) _) r = Literal (LiteralInt (v1*v2)) r
simplifyBinOp op e1 e2 r = BinaryOp op e1 e2 r
