module FCL.IL.Analysis.FreeVars (freeVars, freeInExp) where

import Data.Set hiding (map)

import FCL.IL.Analysis.Liveness (liveInExp)
import FCL.IL.Syntax


freeInExp :: Set ILName -> ILExp -> Set ILName
freeInExp bound e = liveInExp e `difference` bound


freeVars :: [Stmt a] -> Set ILName
freeVars stmts =
  let
    fv :: Set ILName -> [Stmt a] -> Set ILName
    fv _ [] = empty
    -- binding forms
    fv bound (Declare x _ e _ : ss) = freeInExp bound e `union` fv (insert x bound) ss
    fv bound (Alloc x _ e _ : ss)   = freeInExp bound e `union` fv (insert x bound) ss
    fv bound (ReadIntCSV x xlen e _ : ss) = freeInExp bound e `union` fv (insert xlen (insert x bound)) ss
    -- loops
    fv bound (Distribute _ x e body _ : ss) =
        freeInExp bound e
         `union` fv (insert x bound) body
         `union` fv bound ss
    fv bound (ParFor _ x e body _ : ss) =
        freeInExp bound e
         `union` fv (insert x bound) body
         `union` fv bound ss
    fv bound (While e body _ : ss) =
        freeInExp bound e
         `union` fv bound body
         `union` fv bound ss
    fv bound (SeqFor x e body _ : ss) =
        freeInExp bound e
         `union` fv (insert x bound) body
         `union` fv bound ss
    -- other
    fv bound (Synchronize _ : ss) = fv bound ss
    fv bound (Assign x e _ : ss) = (insert x (liveInExp e) `difference` bound) `union` fv bound ss
    fv bound (AssignSub x e0 e1 _ : ss) = ((insert x (liveInExp e0) `union` liveInExp e1) `difference` bound) `union` fv bound ss
    fv bound (If e ss0 ss1 _ : ss) =
      freeInExp bound e
      `union` fv bound ss0
      `union` fv bound ss1
      `union` fv bound ss
    fv bound (PrintIntArray e0 e1 _ : ss) = freeInExp bound e0 `union` freeInExp bound e1 `union` fv bound ss
    fv bound (Benchmark e ss0 _ : ss) =
      freeInExp bound e
       `union` fv bound ss0
       `union` fv bound ss
    
  in fv empty stmts



-- freeVarsStmt :: Set VarName -> Statement a -> (Maybe VarName, Set VarName)
-- freeVarsStmt bound stmt =
--         case stmt of
--           For n e ss _ -> (Nothing,
--                            (delete n (freeInExp bound e))
--                           `union` freeVars' (insert n bound) ss)
--           If e ss0 ss1 _ -> (Nothing,
--                              freeInExp bound e
--                              `union` freeVars' bound (ss0 ++ ss1))
--           While _ e ss _ -> (Nothing,
--                              freeInExp bound e `union` freeVars' bound ss)
--           Assign n e _ -> (Nothing,
--                            (insert n (freeInExp bound e)) `difference` bound)
--           AssignSub n e0 e1 _ -> (Nothing,
--                                   (insert n (freeInExp bound e0) `difference` bound) `union` freeInExp bound e1)
--           Decl n e _ -> (Just n, freeInExp bound e)
--           Exec e _   -> (Nothing, freeInExp bound e)
--           Comment _ _ -> (Nothing, empty)

-- freeVars' :: Set VarName -> [Statement a] -> Set VarName
-- freeVars' _ [] = empty
-- freeVars' bound (stmt:stmts) =
--   case freeVarsStmt bound stmt of
--     (Nothing, free) ->
--       free `union` freeVars' bound stmts
--     (Just newBinding, free) ->
--       free `union` freeVars' (insert newBinding bound) stmts

-- freeVars :: [Statement a] -> Set VarName
-- freeVars = freeVars' empty
