-- | A few simple simplification: beta-reduction, eta-conversion
-- and a few peep-hole optimizations.
module FCL.Simplify
  (simplify)
where

import qualified Data.Set as Set
import Control.Monad.Trans.State

import FCL.Core.Identifier
import FCL.Core.Literal
import FCL.Core.Monotyped

-- Right now, types are not maintained correctly by the simplifier
simplify :: Exp -> Exp
simplify e@(Literal _ _)    = e
simplify e@(Unit _)         = e
simplify e@(Symbol _ _)     = e
simplify (App (Lamb x ebody _) e _) = simplify (apply (x, ebody) e)
simplify (App e1 e2 ty) =
  let e1' = simplify e1
      e2' = simplify e2
  in case e1' of
       (Lamb x ebody _) -> simplify (apply (x, ebody) e2')
       _ -> App e1' e2' ty
simplify (Lamb x1 (App ebody (Symbol x2 _) _) _)
 | x1 == x2 && not (x1 `freeIn` ebody) = simplify ebody
simplify (Lamb x1 ebody ty) = Lamb x1 (simplify ebody) ty
--simplify (Let x1 e1 e2 _ _) = simplify (apply (x1,e2) e1)
simplify (Let x1 e1@(Lamb _ _ _) e2 _) = simplify (apply (x1,e2) e1)
simplify (Let x1 e1 e2 ty) =
  let e1_simpl = simplify e1
  in if isLiteral e1_simpl
     then simplify (apply (x1,e2) e1_simpl)
     else Let x1 e1_simpl (simplify e2) ty
simplify (Cond econd etrue efalse ty) =
  case simplify econd of
    Literal (LiteralBool True)  _ -> simplify etrue
    Literal (LiteralBool False) _ -> simplify efalse
    econd' -> Cond econd' (simplify etrue) (simplify efalse) ty
simplify (Pair e1 e2 ty) = Pair (simplify e1) (simplify e2) ty


freeIn :: Identifier -> Exp -> Bool
freeIn _ (Literal _ _)     = True
freeIn _ (Unit _)          = True
freeIn x (Symbol y _)      = x /= y
freeIn x (Pair e1 e2 _)    = all (freeIn x) [e1, e2]
freeIn x (Cond e1 e2 e3 _) = all (freeIn x) [e1, e2, e3]
freeIn x (Lamb y e _)      = x == y || freeIn x e
freeIn x (App e1 e2 _)     = freeIn x e1 && freeIn x e2
freeIn x (Let y e ebody _) = freeIn x e && (x == y || freeIn x ebody)

freeVars :: Exp -> Set.Set Identifier
freeVars (Literal _ _)     = Set.empty
freeVars (Unit _)          = Set.empty
freeVars (Symbol x _)      = Set.singleton x
freeVars (Lamb x e _)      = Set.difference (freeVars e) (Set.singleton x)
freeVars (App e1 e2 _)     = Set.union (freeVars e1) (freeVars e2)
freeVars (Let x e1 e2 _)   = Set.union (freeVars e1)
                                       (Set.difference (freeVars e2) (Set.singleton x))
freeVars (Cond e1 e2 e3 _) = Set.unions (map freeVars [e1, e2, e3])
freeVars (Pair e1 e2 _)    = Set.unions (map freeVars [e1, e2])

freshVar :: State [Identifier] Identifier
freshVar =
  do (x:xs) <- get
     put xs
     return x

apply :: (Identifier, Exp) -> Exp -> Exp
apply (x, ebody) e =
  let vars = ['x' : show (i :: Int) | i <- [0..]]
  in evalState (subst e x ebody) vars

subst :: Exp -> Identifier -> Exp -> State [Identifier] Exp
subst s x e =
  case e of
   -- Base cases
    Literal _ _ -> return e
    Unit _      -> return e
    Symbol y _
      | x == y    -> return s
      | otherwise -> return e

   -- Interesting cases
    (Lamb y ebody ty@(ty1 :> _))
      | x == y                    -> return e
      | Set.member y (freeVars s) ->
          do z <- freshVar
             ebody' <- subst (Symbol z ty1) y ebody
             ebody'' <- subst s x ebody'
             return (Lamb z ebody'' ty)
      | otherwise ->
          do ebody' <- subst s x ebody
             return (Lamb y ebody' ty)
    (Lamb _ _ _) -> error "Lambda not of function type"
    (Let y e1 e2 ty)
      | x == y                    -> return e
      | Set.member y (freeVars s) ->
          do z <- freshVar
             e1' <- subst s x e1
             e2' <- subst (Symbol z (typeOf e1)) y e2
             e2'' <- subst s x e2'
             return (Let z e1' e2'' ty)
      | otherwise ->
          do e1' <- subst s x e1
             e2' <- subst s x e2
             return (Let y e1' e2' ty)

    -- Recurse in all other cases
    App e1 e2 ty ->
          do e1' <- subst s x e1
             e2' <- subst s x e2
             return (App e1' e2' ty)
    Cond e1 e2 e3 ty ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          e3' <- subst s x e3
          return (Cond e1' e2' e3' ty)
    Pair e1 e2 ty ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          return (Pair e1' e2' ty)

