-- | Program construction monad
module Language.CGen.Monad where

import Language.CGen.Syntax as AST

import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class

data MState = MState
              { params :: [VarName]
              , varCount :: Int
              }
initialState :: MState
initialState = MState
               { params = []
               , varCount = 0
               } 

type CExp = IExp
type IL x = WriterT ([Statement ()]) (State MState) x



runIL :: IL () -> ([Statement ()], [VarName], Int)
runIL m =
  let (stmts, finalState) = runProg m initialState
  in (stmts, reverse $ params finalState, varCount finalState)

runProg :: IL () -> MState -> ([Statement ()], MState)
runProg m init' =
  let (stmts, finalState) = runState (execWriterT m) init'
  in (stmts, finalState)

-- This is weird!
evalIL :: IL a -> a
evalIL m = fst (evalState (runWriterT m) initialState)

run :: IL () -> IL ([Statement ()])
run m = do
  s <- lift get
  let (stmts, s') = runProg m s
  lift (put s')
  return stmts

addStmt :: Statement () -> IL ()
addStmt stmt = tell [stmt]

newVar :: CType -> String -> IL VarName
newVar ty name = do
  c <- lift (gets varCount)
  lift (modify (\s -> s { varCount = 1 + varCount s }))
  return (name ++ "_" ++ show c, ty) -- the underscore is important!

addParam :: String -> CType -> IL VarName
addParam name ty = do
  v <- newVar ty name
  lift (modify (\s -> s { params = v : params s }))
  return v
