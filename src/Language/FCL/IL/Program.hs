-- | Program construction monad
module Language.FCL.IL.Program
  (Program, runProgram, runProg, run, evalCGen, evalProg,
   newILName, newVar, addStmt)
where

import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class

import Language.FCL.IL.Syntax as AST

data MState =
  MState { varCount :: Int }

initialState :: MState
initialState =
  MState { varCount = 0 } 

type Program x = WriterT ([Stmt]) (State MState) x

runProgram :: Program a -> ([Stmt], Int)
runProgram m =
  let (stmts, final) = runProg m initialState
  in (stmts, varCount final)

runProg :: Program a -> MState -> ([Stmt], MState)
runProg m init' =
  let (stmts, finalState) = runState (execWriterT m) init'
  in (stmts, finalState)

evalCGen :: Program a -> ([Stmt], a)
evalCGen m =
  let (stmts, _, v) = evalProg m initialState
  in (stmts, v)

--evalProg :: CGen u a -> MState u -> ([Statement ()], MState u, a)
evalProg :: Program a -> MState -> ([Stmt], MState, a)
evalProg m init' = 
 let ((v, stmts), finalState) = runState (runWriterT m) init'
 in (stmts, finalState, v)


-- -- This is weird!
-- evalCGen :: CGen () a -> a
-- evalCGen m = fst (evalState (runWriterT m) initialState)

run :: Program () -> Program [Stmt]
run m = do
  s <- lift get
  let (stmts, s') = runProg m s
  lift (put s')
  return stmts

addStmt :: Stmt -> Program ()
addStmt stmt = tell [stmt]

newILName :: String -> Program String
newILName name = do
  c <- lift (gets varCount)
  lift (modify (\s -> s { varCount = 1 + varCount s }))
  return (name ++ "_" ++ show c) -- the underscore is important!

newVar :: ILType -> String -> Program ILName
newVar ty name = do
  x <- newILName name
  return (x, ty)
