{-# LANGUAGE FlexibleInstances #-}
module FCLGen_Syntax where

import Control.Applicative
import Control.Monad (replicateM)
import Test.QuickCheck

import Language.FCL.Syntax

-- Not generating any polymorphic types right now
instance Arbitrary Type where
  arbitrary = sized genType

instance Arbitrary (Exp Type) where
  arbitrary = do ty <- arbitrary
                 sized $ \n -> genExp emptyGenEnv ty 0

genType s = oneof [ return IntT
                  , return DoubleT
                  , return BoolT
                  , (:>) <$> subtype <*> subtype
                  , (:*:) <$> subtype <*> subtype
                  , ArrayT <$> subtype
                  ]
   where subtype = genType (s `div` 2)




data GenEnv = GenEnv { variables :: [(VarName, Type)]
                     , count :: Int
                     }

emptyGenEnv = GenEnv [] 0

newVar :: GenEnv -> Type -> (VarName, GenEnv)
newVar env typ =
  let name = "x_" ++ show (count env)
  in ( name
     , GenEnv { variables = (name, typ) : variables env
           , count = count env + 1
           }
     )


genLambda :: GenEnv -> Type -> Type -> Int -> Gen (Exp Type)
genLambda env t_in t_out s =
  let (n, env') = newVar env t_in
  in LamE n t_in <$> genExp env' t_out s <*> return t_out

genApp :: GenEnv -> Type -> Int -> Gen (Exp Type)
genApp env t_out s = do
  t_in <- arbitrary
  lam <- genLambda env t_in t_out (s `div` 2)
  e  <- genExp env t_in (s `div` 2)
  return (AppE lam e t_out)

genIf :: GenEnv -> Type -> Int -> Gen (Exp Type)
genIf env t_out s =
  IfE <$> genExp env BoolT s <*> genExp env t_out (s `div` 2)
                             <*> genExp env t_out (s `div` 2)
                             <*> return t_out

genVector :: GenEnv -> Type -> Int -> Gen (Exp Type)
genVector env t s = do
  content <- replicateM s (genExp env t 0)
  return $ VectorE content t

genProj :: GenEnv -> Type -> Int ->Gen (Exp Type)
genProj env t s = do
  t' <- arbitrary
  oneof [ Proj1E <$> genExp env (t :*: t') s
        , Proj2E <$> genExp env (t' :*: t) s]

genVar :: GenEnv -> Type -> [Gen (Exp Type)]
genVar env t =
  [ return $ VarE name typ | (name, typ) <- variables env
                           , typ == t ]

genExp :: GenEnv -> Type -> Int -> Gen (Exp Type)
genExp env t 0 =
  case t of
    IntT -> orVar (IntE <$> arbitrary)
    DoubleT -> orVar (DoubleE <$> arbitrary)
    BoolT -> orVar (BoolE <$> arbitrary)
    (t1 :> t2) -> genLambda env t1 t2 0
    (ArrayT t') -> genVector env t' 0
    (t1 :*: t2) -> PairE <$> genExp env t1 0 <*> genExp env t2 0
    _ -> IntE <$> arbitrary
  where
    orVar generators = oneof $ generators : genVar env t
genExp env t n =
  case t of
    IntT         -> orGeneric [ IntE <$> arbitrary ]
    DoubleT      -> orGeneric [ DoubleE <$> arbitrary ]
    BoolT        -> orGeneric [ BoolE <$> arbitrary ]
    (t1 :> t2)   -> orGeneric [ genLambda env t1 t2 (n-1) ]
    (ArrayT (ArrayT t')) -> orGeneric [ SplitE <$> genExp env IntT (n `div` 2)
                                               <*> genExp env t' (n `div` 2)
                                               <*> return t
                                      , genVector env (ArrayT t') n]
    (ArrayT t')  -> orGeneric [ genVector env t' n]
    (t1 :*: t2)  -> orGeneric [ PairE <$> genExp env t1 (n `div` 2)
                                      <*> genExp env t2 (n `div` 2) ]
    _ -> IntE <$> arbitrary
  where
    orGeneric :: [Gen (Exp Type)] -> Gen (Exp Type)
    orGeneric generators =
      oneof $ generators ++ [ genApp env t n
                            , genIf env t n
                            , genProj env t (n-1)]
