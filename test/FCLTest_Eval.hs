module FCLTest_Eval (tests) where

import Test.QuickCheck
import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Language.FCL.Eval
import Language.FCL.Syntax
import Language.FCL.Eval.ArrayLib (fromList, toList)

import FCLGen_Syntax

fcl = eval emptyEnv


prop_deterministic :: Exp Type -> Bool
prop_deterministic e = fcl e == fcl e

prop_if_true, prop_if_false :: Exp Type -> Exp Type -> Bool
prop_if_true e1 e2 = fcl (IfE (BoolE True) e1 e2 (typeOf e1)) == fcl e1
prop_if_false e1 e2 = fcl (IfE (BoolE False) e1 e2 (typeOf e1)) == fcl e2

prop_apply e =
  let t = typeOf e
  in fcl (AppE (LamE "x" t (VarE "x" t) (t :> t)) e t) == fcl e

prop_let e =
  let t = typeOf e
  in fcl (LetE "x" (TypeScheme [] t) e (VarE "x" t) t) == fcl e

--test_const_int :: Assertion
--test_const_int = fcl (IntE 4) @?= (IntV 4 :: Value NoType)

tests :: [Test]
tests =  [ -- testCase "eval (IntE 4)" test_const_int
--           testProperty "eval deterministc" prop_deterministic
          testProperty "eval if true" prop_if_true
         , testProperty "eval if false" prop_if_false
         , testProperty "eval eta-conversion" prop_apply
         , testProperty "eval let" prop_apply
         ]
