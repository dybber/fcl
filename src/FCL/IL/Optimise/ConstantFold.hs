module FCL.IL.Optimise.ConstantFold
(constantFold, foldExp)
where

import FCL.IL.Syntax

import Data.Bits (shiftL, shiftR, (.&.), (.|.))

-- naive linear-time algorithm
ilog2 :: Int -> Int
ilog2 0 = -1
ilog2 n = ilog2 (n `div` 2) + 1

powers :: [Integer]
powers = map (2^) [1..(64 :: Integer)]

isPowerOfTwo :: Integral a => a -> Bool
isPowerOfTwo i = elem (toInteger i) powers

foldExp :: ILExp -> ILExp
foldExp e =
  case e of
    EInt _     -> e
    EDouble _  -> e
    EBool _    -> e
    EString _  -> e
    EVar _     -> e
    EIndex var e0 -> EIndex var (foldExp e0)
    EIf e0 e1 e2 -> foldIf (foldExp e0) (foldExp e1) (foldExp e2)
    EBinOp op e0 e1 -> foldBinOp op (foldExp e0) (foldExp e1)
    EUnaryOp op e0 -> foldUnOp op (foldExp e0)

foldIf :: ILExp -> ILExp -> ILExp -> ILExp
foldIf (EBool True) e1 _              = e1
foldIf (EBool False) _ e2             = e2
foldIf e0 (EBool True) e2  | e0 == e2 = e0
foldIf e0 (EBool False) e2 | e0 == e2 = EBool False
foldIf e0 e1 (EBool False) | e0 == e1 = e0
foldIf e0 e1 (EBool True)  | e0 == e1 = EBool True
foldIf e0 e1 e2                       = EIf e0 e1 e2
-- TODO: nested ifs

foldUnOp :: UnaryOp -> ILExp -> ILExp
-- foldUnOp Not (EBool True) = EBool False
-- foldUnOp Not (EBool False) = EBool True
-- foldUnOp Not (UnaryOpE Not e) = e
foldUnOp op e = EUnaryOp op e
-- TODO

foldBinOp :: BinOp -> ILExp -> ILExp -> ILExp
foldBinOp AddI (EInt v0) (EInt v1) = EInt (v0 + v1)
foldBinOp AddI (EInt 0) e1 = e1
foldBinOp AddI e0 (EInt 0) = e0
foldBinOp AddI (EBinOp SubI e0 (EInt v1)) (EInt v2) =
  foldBinOp AddI e0 (EInt (v2-v1)) -- (a - b) + c ==> a + (c-b)
foldBinOp AddI (EBinOp SubI (EInt v0) e1) (EInt v2) =
  foldBinOp SubI (EInt (v0+v2)) e1 -- (a - b) + c ==> (a + c) -b

foldBinOp SubI (EInt v0) (EInt v1) = EInt (v0 - v1)
--foldBinOp SubI (EInt 0) e1 = EUnaryOp NegateInt e1
foldBinOp SubI e0 (EInt 0) = e0

foldBinOp MulI (EInt v0) (EInt v1) = EInt (v0 * v1)
foldBinOp MulI (EInt 1) e1 = e1
foldBinOp MulI e0 (EInt 1) = e0
foldBinOp MulI (EInt 0) _ = EInt 0
foldBinOp MulI _ (EInt 0) = EInt 0

foldBinOp MulI (EBinOp DivI e0 (EInt v1)) (EInt v2)
  | v1 `mod` v2 == 0 = foldBinOp DivI e0 (EInt (v1 `div` v2))
                       -- (a / b) * c ==> (a / (c/b))
foldBinOp DivI (EInt v0) (EInt v1) = EInt (v0 `div` v1)
foldBinOp DivI (EInt 0) _ = EInt 0
foldBinOp DivI e0 (EInt 1) = e0
foldBinOp DivI e0 e1@(EInt v) =
  if isPowerOfTwo v
  then foldBinOp Srl e0 (EInt (ilog2 v))
  else EBinOp DivI e0 e1
foldBinOp DivI e0 e1 | e0 == e1 = EInt 1

foldBinOp ModI (EInt v0) (EInt v1) = EInt (v0 `mod` v1)
foldBinOp ModI _ (EInt 1) = EInt 0
foldBinOp ModI e0 e1@(EInt v) =
  if isPowerOfTwo v
  then foldBinOp Land e0 (EInt (v-1))
  else EBinOp ModI e0 e1

foldBinOp ModI e0 e1 | e0 == e1 = EInt 0

foldBinOp EqI (EInt v0) (EInt v1) | v0 == v1  = EBool True
                                  | otherwise = EBool False
foldBinOp NeqI (EInt v0) (EInt v1) | v0 /= v1  = EBool True
                                   | otherwise = EBool False
foldBinOp LtI (EInt v0) (EInt v1) | v0 < v1  = EBool True
                                  | otherwise = EBool False
foldBinOp GtI (EInt v0) (EInt v1) = EBool (v0 > v1)

foldBinOp Sll (EInt v0) (EInt v1) = EInt (shiftL v0 v1)
foldBinOp Srl (EInt v0) (EInt v1) = EInt (shiftR v0 v1)
foldBinOp Land (EInt v0) (EInt v1) = EInt (v0 .&. v1)
foldBinOp Lor (EInt v0) (EInt v1) = EInt (v0 .|. v1)

foldBinOp MinI (EInt v0) (EInt v1) = EInt (min v0 v1)

foldBinOp op e0 e1 = EBinOp op e0 e1

constantFold :: [Stmt a] -> [Stmt a]
constantFold stmts = process stmts --concat (map process stmts)
 where
   process :: [Stmt a] -> [Stmt a]
   process [] = []
   process (  s1@(Assign v e _)
            : s2@(While econd _ _)
            : ss) =
       case (econd, e) of
         (EVar v2, EBool False) | v == v2 -> process1 s1 ++ process ss
         _ -> process1 s1 ++ process1 s2 ++ process ss
   process (s : ss) = process1 s ++ process ss

   
   process1 :: Stmt a -> [Stmt a]
   process1 (SeqFor v e body i) =
     case foldExp e of
       EInt 0 -> []
       EInt 1 -> [Declare v ILInt (EInt 0) i] ++ constantFold body
       e' -> [SeqFor v e' (constantFold body) i]
   process1 (ParFor lvl v e body i)          = [ParFor lvl v (foldExp e) (constantFold body) i]
   process1 (Distribute lvl v e body i)          = [Distribute lvl v (foldExp e) (constantFold body) i]
   process1 (If e strue sfalse i) =
        case foldExp e of
          EBool True -> constantFold strue
          EBool False -> constantFold sfalse
          e' -> [If e' (constantFold strue)
                       (constantFold sfalse) i]
   process1 (While e body i)        = [While (foldExp e) (constantFold body) i]
   process1 (Declare v ty e i)      = [Declare v ty (foldExp e) i]
   process1 (Assign v e i)          = [Assign v (foldExp e) i]
   process1 (AssignSub v e0 e1 i)   = [AssignSub v (foldExp e0) (foldExp e1) i]
   process1 (Alloc v ty e i)        = [Alloc v ty (foldExp e) i]
   process1 (Synchronize i)         = [Synchronize i]
   process1 (ReadIntCSV v1 v2 e i)  = [ReadIntCSV v1 v2 (foldExp e) i]
   process1 (PrintIntArray e1 e2 i) = [PrintIntArray e1 e2 i]
   process1 (PrintDoubleArray e1 e2 i) = [PrintDoubleArray e1 e2 i]
   process1 (Benchmark e body i)    = [Benchmark (foldExp e) (constantFold body) i]

