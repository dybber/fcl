module FCL.IL.Optimise.LoopUnroll
--  (unroll)
where

import qualified Data.Map as Map
import Control.Monad.Trans.RWS
import FCL.IL.Syntax

-- unroll all while loops twenty iterations
numUnrolls :: Int
numUnrolls = 0

unroll :: Int -> [Stmt a] -> [Stmt a]
unroll varCount stmts = fst (evalRWS (unrollM stmts) Map.empty varCount)

unrollM :: [Stmt a] -> Unroll [Stmt a]
unrollM stmts = concat <$> (mapM process stmts)
 where
   process :: Stmt a -> Unroll [Stmt a]
   process (SeqFor v e body i) =
     do body' <- unrollM body
        return [SeqFor v e body' i]
   process (ParFor lvl v e body i) =
     do body' <- unrollM body
        return [ParFor lvl v e body' i]
   process (Distribute lvl v e body i) =
     do body' <- unrollM body
        return [Distribute lvl v e body' i]
   process (Benchmark e body i) =
     do body' <- unrollM body
        return [Benchmark e body' i]
   process (If e strue sfalse i) =
     do strue' <- unrollM strue
        sfalse' <- unrollM sfalse
        return [If e strue' sfalse' i]
   process (While e body i)    =
     do body' <- unrollM body
        whileUnroll numUnrolls e body' i
   process stmt = return [stmt]

whileUnroll :: Int -> ILExp -> [Stmt a] -> a -> Unroll [Stmt a]
whileUnroll 0 e body i = return [While e body i]
whileUnroll n e body i =
  do ifstmt <- renameLocalVars [If e body [] i]
     rest <- whileUnroll (n-1) e body i
     return (ifstmt ++ rest)

-- Environment mapping old names to new names
type Env = Map.Map ILName ILName

-- New monad with ability to generate names
type Unroll a = RWS Env () Int a

newVar :: ILName -> (ILName -> Unroll a) -> Unroll a
newVar x@(ILName name _) f = 
  do varCount <- get
     put (varCount+1)
     let newvar = ILName name varCount
     local (Map.insert x newvar) (f newvar)


renameVar :: ILName -> Unroll ILName
renameVar name =
  do env <- ask
     case Map.lookup name env of
       Just x -> return x
       Nothing -> return name

-- function that renames any variable in expression, if found in the
-- environment
renameInExp :: ILExp -> Unroll ILExp
renameInExp (EVar x) = EVar <$> renameVar x
renameInExp (EIndex x e) = EIndex <$> renameVar x <*> renameInExp e
renameInExp (EUnaryOp op e1) = EUnaryOp op <$> renameInExp e1
renameInExp (EBinOp op e1 e2) = EBinOp op <$> renameInExp e1 <*> renameInExp e2
renameInExp (EIf e1 e2 e3) = EIf <$> renameInExp e1 <*> renameInExp e2 <*> renameInExp e3
renameInExp e = return e

-- function that iterates over a program, renaming variables when
-- declared, and usage points.  variables that a free in the program
-- should not be touched.
renameLocalVars :: [Stmt a] -> Unroll [Stmt a]
renameLocalVars [] = return []
renameLocalVars (SeqFor v e body i : s) =
  newVar v (\v' -> do e' <- renameInExp e
                      body' <- renameLocalVars body
                      s' <- renameLocalVars s
                      return (SeqFor v' e' body' i : s'))
renameLocalVars (ParFor lvl v e body i : s) =
  newVar v (\v' -> do e' <- renameInExp e
                      body' <- renameLocalVars body
                      s' <- renameLocalVars s
                      return (ParFor lvl v' e' body' i : s'))
renameLocalVars (Distribute lvl v e body i : s) =
  newVar v (\v' -> do e' <- renameInExp e
                      body' <- renameLocalVars body
                      s' <- renameLocalVars s
                      return (Distribute lvl v' e' body' i : s'))
renameLocalVars (Declare v ty e i : s) =
  newVar v (\v' -> do e' <- renameInExp e
                      s' <- renameLocalVars s
                      return (Declare v' ty e' i : s'))
renameLocalVars (Alloc v ty e i : s) =
  newVar v (\v' -> do e' <- renameInExp e
                      s' <- renameLocalVars s
                      return (Alloc v' ty e' i : s'))
renameLocalVars (Assign v e i : s) =
  do v' <- renameVar v
     e' <- renameInExp e
     s' <- renameLocalVars s
     return (Assign v' e' i : s')
renameLocalVars (AssignSub v e1 e2 i : s) =
  do v' <- renameVar v
     e1' <- renameInExp e1
     e2' <- renameInExp e2
     s' <- renameLocalVars s
     return (AssignSub v' e1' e2' i : s')
renameLocalVars (ReadIntCSV v vlen e i : s) =
  newVar v (\v' ->
    newVar vlen (\vlen' ->
      do e' <- renameInExp e
         s' <- renameLocalVars s
         return (ReadIntCSV v' vlen' e' i : s')))
renameLocalVars (PrintIntArray e1 e2 i : s) =
  do e1' <- renameInExp e1
     e2' <- renameInExp e2
     s' <- renameLocalVars s
     return (PrintIntArray e1' e2' i : s')
renameLocalVars (PrintDoubleArray e1 e2 i : s) =
  do e1' <- renameInExp e1
     e2' <- renameInExp e2
     s' <- renameLocalVars s
     return (PrintDoubleArray e1' e2' i : s')
renameLocalVars (Benchmark e body i : s) =
  do e' <- renameInExp e
     body' <- renameLocalVars body
     s' <- renameLocalVars s
     return (Benchmark e' body' i : s')
renameLocalVars (While e body i : s) =
  do e' <- renameInExp e
     body' <- renameLocalVars body
     s' <- renameLocalVars s
     return (While e' body' i : s')
renameLocalVars (If e ssthen sselse i : s) =
  do e' <- renameInExp e
     ssthen' <- renameLocalVars ssthen
     sselse' <- renameLocalVars sselse
     s' <- renameLocalVars s
     return (If e' ssthen' sselse' i : s')
renameLocalVars (Synchronize i : s) =
  do s' <- renameLocalVars s
     return (Synchronize i : s')
 
-- rewrite unroll to use the above monad.

-- Make exposed unroll function accept variable count as input
-- Make optimise accept variable count as input
