module Language.ObsidianLight.Examples where

import Language.ObsidianLight.Syntax
import Language.ObsidianLight.Compile
import Prelude hiding (splitAt, zipWith)

-- Examples from Obsidian

zipWith :: Level -> OExp -> OExp -> OExp -> OExp
zipWith lvl op a1 a2 =
  Generate lvl
           (BinOp MinI (Length a1) (Length a2))
           (Lamb "ix" (App op (Pair (Index a1 (Var "ix"))
                                    (Index a2 (Var "ix")))))

splitAt :: Level -> OExp -> OExp -> OExp
splitAt lvl n arr = Pair (Generate lvl n (Lamb "x" (Index arr (Var "x"))))
                         (Generate lvl (BinOp SubI (Length arr) n) (Lamb "x" (Index arr (BinOp AddI (Var "x") n))))

halve :: Level -> OExp -> OExp
halve lvl arr = splitAt lvl (BinOp DivI (Length arr) (IntScalar 2)) arr

-- Block-level reduction
-- The array here is just a single element in a larger array
red1 :: OExp -> OExp -> OExp
red1 f arr =
  Fixpoint (Lamb "arr"
                 (Let "y" (halve Block (Var "arr")) $
                  ComputeLocal (zipWith Block f (Proj1E (Var "y")) (Proj2E (Var "y")))))
           (Lamb "arr" (BinOp EqI (IntScalar 1) (Length (Var "arr"))))
           arr

addi = Lamb "x1" (Lamb "x2" (BinOp AddI (Var "x1") (Var "x2")))

test :: Array Tagged
test =
  case compile emptyEnv (red1 addi undefined) of
    TagArray arr -> arr
