module TestFCL where


--import qualified Test.HUnit as H
import Test.QuickCheck
import Distribution.TestSuite.QuickCheck

import Language.FCL.Eval.ArrayLib


prop_testMapA :: [Int] -> Bool
prop_testMapA a = toList (mapA (+1) (fromList a)) == map (+1) a

prop_testReduceA :: [Int] -> Bool
prop_testReduceA a = reduceA (uncurry (+)) 0 (fromList a) == foldl (+) 0 a

prop_testSplitJoin :: Positive Int -> Int -> Property
prop_testSplitJoin (Positive split) n =
  forAll (vector (n*split) :: Gen [Int]) $ \a ->
     a == (toList . joinA . splitA split . fromList) a

tests :: IO [Test]
tests = return [ testProperty "mapA" prop_testMapA
               , testProperty "reduceA" prop_testReduceA
               , testProperty "splitJoin" prop_testSplitJoin
               ]



-- testEval :: Eq ty => String -> Exp ty -> Value ty -> Bool
-- testEval name e_in v_expected =
--   testProperty name $ eval emptyEnv e_in == v_expected
