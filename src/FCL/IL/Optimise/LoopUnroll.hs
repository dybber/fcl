module FCL.IL.Optimise.LoopUnroll
  (unroll)
where

import FCL.IL.Syntax
import FCL.IL.Analysis (Label)

-- unroll all while loops twenty iterations
numUnrolls :: Int
numUnrolls = 0

unroll :: [Stmt Label] -> [Stmt Label]
unroll stmts = concat (map process stmts)
 where
   process :: Stmt Label -> [Stmt Label]
   process (SeqFor v e body i)   = [SeqFor v e (unroll body) i]
   process (ParFor lvl v e body i)   = [ParFor lvl v e (unroll body) i]
   process (Distribute lvl v e body i)   = [Distribute lvl v e (unroll body) i]
   process (If e strue sfalse i) = [If e (unroll strue)
                                         (unroll sfalse) i]
   process (While e body i)    = whileUnroll numUnrolls e (unroll body) i

   process stmt = [stmt]


whileUnroll :: Int -> ILExp -> [Stmt Label] -> Label -> [Stmt Label]
whileUnroll 0 e body i = [While e body i]
whileUnroll n e body i = (If e body [] i) : whileUnroll (n-1) e body i


-- maybeUnrollWhile :: Map.Map Label (Set.Set Label) -> (VarName -> Set.Set (Label, Maybe ILExp)) -> ILExp -> [Stmt Label] -> Label -> [Stmt Label]
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
