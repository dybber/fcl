module FCLTest_TypeInference (tests) where

import Test.QuickCheck
import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Language.FCL.Eval
import Language.FCL.Syntax
import Language.FCL.TypeInference

import FCLGen_Syntax

prop_deterministic :: Exp Type -> Bool
prop_deterministic e = typeinfer emptyTEnv e == typeinfer emptyTEnv e


-- This does not fly, as the infered type might be more general
-- than the type originally specified for the expression.
--prop_reconstruct :: Exp Type -> Bool
--prop_reconstruct e = typeinfer emptyTEnv e == typeOf e

--test_const_int :: Assertion
--test_const_int = fcl (IntE 4) @?= (IntV 4 :: Value NoType)

tests :: [Test]
tests =  [ -- testCase "eval (IntE 4)" test_const_int
           testProperty "infer deterministc" prop_deterministic
--         , testProperty "reconstruct" prop_reconstruct
         ]
