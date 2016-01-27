module Language.GPUIL.Optimise.ConstantFold
(constantFold, foldExp)
where

import Language.GPUIL.Syntax  

foldExp :: IExp -> IExp
foldExp e =
  case e of
    IntE _     -> e
    DoubleE _  -> e
    BoolE _    -> e
    Word8E _   -> e
    Word32E _  -> e
    Word64E _  -> e
    GlobalID   -> e
    LocalID    -> e
    GroupID    -> e
    LocalSize  -> e
    NumGroups  -> e
    WarpSize   -> e
    VarE _  -> e
    CastE ty0 (CastE _ e0)  -> foldExp (CastE ty0 (foldExp e0))
    CastE ty e0  -> CastE ty (foldExp e0)
    IndexE var e0 -> IndexE var (foldExp e0)
    IfE e0 e1 e2 -> foldIf (foldExp e0) (foldExp e1) (foldExp e2)
    BinOpE op e0 e1 -> foldBinOp op (foldExp e0) (foldExp e1)
    UnaryOpE op e0 -> foldUnOp op (foldExp e0)

foldIf :: IExp -> IExp -> IExp -> IExp
foldIf (BoolE True) e1 _              = e1
foldIf (BoolE False) _ e2             = e2
foldIf e0 (BoolE True) e2  | e0 == e2 = e0
foldIf e0 (BoolE False) e2 | e0 == e2 = BoolE False
foldIf e0 e1 (BoolE False) | e0 == e1 = e0
foldIf e0 e1 (BoolE True)  | e0 == e1 = BoolE True
foldIf e0 e1 e2                       = IfE e0 e1 e2
-- TODO: nested ifs

foldUnOp :: UnaryOp -> IExp -> IExp
foldUnOp Not (BoolE True) = BoolE False
foldUnOp Not (BoolE False) = BoolE True
foldUnOp Not (UnaryOpE Not e) = e
foldUnOp op e = UnaryOpE op e
-- TODO

foldBinOp :: BinOp -> IExp -> IExp -> IExp
foldBinOp AddI (IntE v0) (IntE v1) = IntE (v0 + v1)
foldBinOp AddI (IntE 0) e1 = e1
foldBinOp AddI e0 (IntE 0) = e0
foldBinOp AddI (BinOpE SubI e0 (IntE v1)) (IntE v2) =
  foldBinOp AddI e0 (IntE (v2-v1)) -- (a - b) + c ==> a + (c-b)
foldBinOp AddI (BinOpE SubI (IntE v0) e1) (IntE v2) =
  foldBinOp SubI (IntE (v0+v2)) e1 -- (a - b) + c ==> (a + c) -b

foldBinOp SubI (IntE v0) (IntE v1) = IntE (v0 - v1)
foldBinOp SubI (IntE 0) e1 = UnaryOpE NegateInt e1
foldBinOp SubI e0 (IntE 0) = e0

foldBinOp MulI (IntE v0) (IntE v1) = IntE (v0 * v1)
foldBinOp MulI (IntE 1) e1 = e1
foldBinOp MulI e0 (IntE 1) = e0
foldBinOp MulI (IntE 0) _ = IntE 0
foldBinOp MulI _ (IntE 0) = IntE 0

foldBinOp MulI (BinOpE DivI e0 (IntE v1)) (IntE v2)
  | v1 `mod` v2 == 0 = foldBinOp DivI e0 (IntE (v1 `div` v2))
                       -- (a / b) * c ==> (a / (c/b))


foldBinOp DivI (IntE v0) (IntE v1) = IntE (v0 `div` v1)
foldBinOp DivI (IntE 0) _ = IntE 0
foldBinOp DivI e0 (IntE 1) = e0

foldBinOp ModI (IntE v0) (IntE v1) = IntE (v0 `mod` v1)
foldBinOp ModI _ (IntE 1) = IntE 0

foldBinOp EqI (IntE v0) (IntE v1) | v0 == v1  = BoolE True
                                  | otherwise = BoolE False
foldBinOp LtI (IntE v0) (IntE v1) | v0 < v1  = BoolE True
                                  | otherwise = BoolE False

foldBinOp op e0 e1 = BinOpE op e0 e1

constantFold :: [Statement a] -> [Statement a]
constantFold stmts = concat $ map process stmts
 where
   process :: Statement a -> [Statement a]
   process (For v e body i)        = [For v (foldExp e) (constantFold body) i]
   process (DistrPar lvl v e body i) = [DistrPar lvl v e (constantFold body) i]
   process (ForAll lvl v e body i)   = [ForAll lvl v (foldExp e) (constantFold body) i]
   process (If e strue sfalse i) =
        case foldExp e of
          BoolE True -> constantFold strue
          BoolE False -> constantFold sfalse
          e' -> [If e' (constantFold strue)
                       (constantFold sfalse) i]
   process (SeqWhile e body i)    = [SeqWhile (foldExp e) (constantFold body) i]
   process (Decl v e i)           = [Decl v (foldExp e) i]
   process (SyncLocalMem i)       = [SyncLocalMem i]
   process (SyncGlobalMem i)      = [SyncGlobalMem i]
   process (Allocate v e i)       = [Allocate v (foldExp e) i]
   process (Assign v e i)        = [Assign v (foldExp e) i]
   process (AssignSub v e0 e1 i) = [AssignSub v (foldExp e0) (foldExp e1) i]
   process (Comment msg i)       = [Comment msg i]