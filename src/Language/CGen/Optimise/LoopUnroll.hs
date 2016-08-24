module Language.CGen.Optimise.LoopUnroll
(unroll)
where

import Language.CGen.Syntax
import Language.CGen.Analysis (Label)

unroll :: [Statement Label] -> [Statement Label]
unroll stmts = concat (map process stmts)
 where
   process :: Statement Label -> [Statement Label]
   process (For v e body i)          = [For v e (unroll body) i]
   process (If e strue sfalse i) = [If e (unroll strue)
                                          (unroll sfalse) i]
   process (While 0 e body i)   = [While 0 e (unroll body) i]
   process (While n e body i)    = whileUnroll n e body i

   process stmt = [stmt]


whileUnroll :: Int -> IExp -> [Statement Label] -> Label -> [Statement Label]
whileUnroll n e body lbl =
  let body' = unroll body
  in [If e (body' ++ [While (n-1) e body' lbl]) [] lbl]


-- maybeUnrollWhile :: Map.Map Label (Set.Set Label) -> (VarName -> Set.Set (Label, Maybe IExp)) -> IExp -> [Statement Label] -> Label -> [Statement Label]
-- maybeUnrollWhile inSet defs e@(VarE vName) body lbl =
--   let
--     body' = unroll inSet defs body
--     -- nounroll i = [Comment ("No unroll possible " ++ show i) lbl, While False e body' lbl]
--     dounroll = body' ++ [While True e body' lbl]

--     -- bodyLbls = Set.fromList (labels body)

--     -- lookupDef i =
--     --   case lookup i (Set.toList (defs vName)) of
--     --     Nothing -> Nothing
--     --     Just e -> e

--   in dounroll -- case Map.lookup lbl inSet of
--        -- Just ins ->
--        --   let outsideLbls = Set.filter (\i -> not (Set.member i bodyLbls)) ins
--        --       prevDefs = Set.map lookupDef outsideLbls
--        --   in nounroll prevDefs
--        --      -- case  of
--        --      --   [i] ->
--        --      --     case lookupDef i of
--        --      --       Just (BoolE True) -> dounroll
--        --      --       Nothing -> nounroll 0
--        --      --   _ -> nounroll (lbl, ins, outsideLbls)
--        -- Nothing -> nounroll 2
-- maybeUnrollWhile inSet defs e body lbl = [While False e (unroll inSet defs body) lbl]
