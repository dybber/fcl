module Language.FCL.Simplify (simplify, simplifyExp) where

import Language.FCL.Syntax
import Language.FCL.SourceRegion


-- Right now, types are not maintained correctly by the simplifier
simplify :: Program ty -> Program ty
simplify = map simplifyDef

simplifyDef :: Definition ty -> Definition ty
simplifyDef d = d { defBody = simplifyExp (defKernelConfig d) (defBody d) }

simplifyExp :: KernelConfig -> Exp ty -> Exp ty
simplifyExp _ e@(IntScalar _ _)    = e
simplifyExp _ e@(DoubleScalar _ _) = e
simplifyExp _ e@(BoolScalar _ _)   = e
simplifyExp cfg e@(BlockSize r)      =
  case configBlockSize cfg of
    Just x -> IntScalar x r
    Nothing -> e
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
  in if isScalar e1_simpl
     then simplifyExp cfg (apply (x1,e2) e1_simpl)
     else Let x1 e1_simpl (simplifyExp cfg e2) ty r
simplifyExp cfg (UnOp op e r) = simplifyUnOp cfg op (simplifyExp cfg e) r
simplifyExp cfg (BinOp op e1 e2 r) = simplifyBinOp cfg op (simplifyExp cfg e1) (simplifyExp cfg e2) r
simplifyExp cfg (Cond econd etrue efalse ty r) =
  case simplifyExp cfg econd of
    BoolScalar True  _ -> simplifyExp cfg etrue
    BoolScalar False _ -> simplifyExp cfg efalse
    econd' -> Cond econd' (simplifyExp cfg etrue) (simplifyExp cfg efalse) ty r
simplifyExp cfg (Vec es ety reg)           = Vec          (map (simplifyExp cfg) es) ety reg
simplifyExp cfg (Pair e0 e1 reg)           = Pair         (simplifyExp cfg e0) (simplifyExp cfg e1) reg
simplifyExp cfg (Proj1E e0 reg)            = Proj1E       (simplifyExp cfg e0) reg
simplifyExp cfg (Proj2E e0 reg)            = Proj2E       (simplifyExp cfg e0) reg
simplifyExp cfg (Index e0 e1 reg)          = Index        (simplifyExp cfg e0) (simplifyExp cfg e1) reg
simplifyExp cfg (LengthPull e0 reg)        = LengthPull   (simplifyExp cfg e0) reg
simplifyExp cfg (LengthPush e0 reg)        = LengthPush   (simplifyExp cfg e0) reg
simplifyExp cfg (While e0 e1 e2 reg)       = While        (simplifyExp cfg e0) (simplifyExp cfg e1) (simplifyExp cfg e2) reg
simplifyExp cfg (WhileSeq e0 e1 e2 reg)    = WhileSeq     (simplifyExp cfg e0) (simplifyExp cfg e1) (simplifyExp cfg e2) reg
simplifyExp cfg (GeneratePull e0 e1 reg)   = GeneratePull (simplifyExp cfg e0) (simplifyExp cfg e1) reg
simplifyExp cfg (MapPull e0 e1 reg)        = MapPull      (simplifyExp cfg e0) (simplifyExp cfg e1) reg
simplifyExp cfg (MapPush e0 e1 reg)        = MapPush      (simplifyExp cfg e0) (simplifyExp cfg e1) reg
simplifyExp cfg (Push lvl e0 t reg)        = Push         lvl (simplifyExp cfg e0) t reg
simplifyExp cfg (Force e0 reg)             = Force        (simplifyExp cfg e0) reg
simplifyExp cfg (Concat e0 e1 reg)         = Concat       (simplifyExp cfg e0) (simplifyExp cfg e1) reg
simplifyExp cfg (Interleave e0 e1 e2 reg)  = Interleave   (simplifyExp cfg e0) (simplifyExp cfg e1) (simplifyExp cfg e2) reg
simplifyExp cfg (Scanl e0 e1 e2 reg)       = Scanl        (simplifyExp cfg e0) (simplifyExp cfg e1) (simplifyExp cfg e2) reg

simplifyUnOp :: KernelConfig -> UnOp -> Exp ty -> Region -> Exp ty
simplifyUnOp cfg op e r = UnOp op e r

simplifyBinOp :: KernelConfig -> BinOp -> Exp ty -> Exp ty -> Region -> Exp ty
simplifyBinOp cfg MulI (IntScalar v1 _) (IntScalar v2 _) r = IntScalar (v1*v2) r
simplifyBinOp cfg op e1 e2 r = BinOp op e1 e2 r
