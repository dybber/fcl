module FCLTest_Large where

import Language.FCL.Syntax
import Language.FCL.TypeInference


-- Simple tests

-- Untyped constructors
reduceSeq f b x = ReduceSeqE f b x NoType
join_ x = JoinE x NoType
mapWorkgroup f x = MapE Workgroup f x NoType
mapLocal f x = MapE Local f x NoType
toGlobal x = ToGlobalE x
split x y = SplitE x y NoType
app f e = AppE f e NoType
var x = VarE x NoType
lam x e = LamE x NoType e NoType
add x y = BinOpE AddD x y
add_uncurry = lam "y"
                  ((Proj1E (var "y")) `add` (absD (Proj2E (var "y"))))
absD x = UnaryOpE AbsD x

-- Example from Dubach-paper
-- The outer reduceSeq in the original program doesn't makes sense to
-- me!
asum_nvidia :: Exp NoType
asum_nvidia =
  let seq_sum :: Exp NoType
      seq_sum = lam "arr"
                    (reduceSeq add_uncurry
                               (DoubleE 0.0)
                               (var "arr"))
  in
    lam "x" 
     ((join_
      . join_
      . mapWorkgroup (toGlobal (lam "x" (mapLocal seq_sum (var "x"))))
      . split (IntE 128)
      . split (IntE 2048)) (var "x"))

----
-- map_simple : (a ---thread---> b) -> Array a -> Array b
map_simple :: Exp NoType -> Exp NoType -> Exp NoType
map_simple f = mapWorkgroup (toGlobal (lam "x" (mapLocal f (var "x"))))
                . split (IntE 64)
                . split (IntE 1024)


--- fun f blockSize threadSize arr =
---   mapWorkgroup (toGlobal (fn x => mapLocal f x))
---                (split threadSize (split blockSize arr))
--- 
---
---
