module FCL.IL.NumberVars where

import Control.Monad.Trans.State
import qualified Data.Map as Map
import FCL.IL.Syntax

rename :: [Stmt ()] -> [Stmt ()]
rename stmts = evalState (renameProg Map.empty stmts) 0

type M a = State Int a

type Env = Map.Map ILName ILName

newName :: ILName -> M ILName
newName (ILName x _) =
  do counter <- get
     put (counter + 1)
     return (ILName x counter)

renameVar :: Env -> ILName -> ILName
renameVar env x =
  case Map.lookup x env of
    Just x' -> x'
    Nothing -> error ("Not in scope " ++ show x)

renameExp :: Env -> ILExp -> ILExp
renameExp env e =
  case e of
    EInt _ -> e
    EDouble _ -> e
    EBool _ -> e
    EString _ -> e
    EVar x -> EVar (renameVar env x)
    EIndex x e1 -> EIndex (renameVar env x) (renameExp env e1)
    EUnaryOp op e1 -> EUnaryOp op (renameExp env e1)
    EBinOp op e1 e2 -> EBinOp op (renameExp env e1) (renameExp env e2)
    EIf e1 e2 e3 -> EIf (renameExp env e1) (renameExp env e2) (renameExp env e3)

renameProg :: Env -> [Stmt ()] -> M [Stmt ()]
renameProg _ [] = return []
renameProg env (s:stmts) =
  case s of
    Declare x ty e () ->
      do x' <- newName x
         stmts' <- renameProg (Map.insert x x' env) stmts
         return (Declare x' ty (renameExp env e) () : stmts')
    Alloc x ty e () ->
      do x' <- newName x
         stmts' <- renameProg (Map.insert x x' env) stmts
         return (Alloc x' ty (renameExp env e) () : stmts')
    Distribute lvl x bound body () ->
      do x' <- newName x
         body' <- renameProg (Map.insert x x' env) body
         stmts' <- renameProg env stmts
         return (Distribute lvl x' (renameExp env bound) body' () : stmts')
    ParFor lvl x bound body () ->
      do x' <- newName x
         body' <- renameProg (Map.insert x x' env) body
         stmts' <- renameProg env stmts
         return (ParFor lvl x' (renameExp env bound) body' () : stmts')
    Synchronize () ->
      do stmts' <- renameProg env stmts
         return (Synchronize () : stmts')
    Assign x e () ->
      do stmts' <- renameProg env stmts
         return (Assign (renameVar env x) (renameExp env e) () : stmts')
    AssignSub x e1 e2 () ->
      do stmts' <- renameProg env stmts
         return (AssignSub (renameVar env x) (renameExp env e1) (renameExp env e2) () : stmts')
    If e1 body1 body2 () ->
      do body1' <- renameProg env body1
         body2' <- renameProg env body2
         stmts' <- renameProg env stmts
         return (If (renameExp env e1) body1' body2' () : stmts')
    While e1 body1 () ->
      do body1' <- renameProg env body1
         stmts' <- renameProg env stmts
         return (While (renameExp env e1) body1' () : stmts')
    SeqFor x bound body () ->
      do x' <- newName x
         body' <- renameProg (Map.insert x x' env) body
         stmts' <- renameProg env stmts
         return (SeqFor x' (renameExp env bound) body' () : stmts')
    ReadIntCSV x xlen e () ->
      do x' <- newName x
         xlen' <- newName xlen
         stmts' <- renameProg (Map.insert xlen xlen' (Map.insert x x' env)) stmts
         return (ReadIntCSV x' xlen' (renameExp env e) () : stmts')
    Benchmark e1 body1 () ->
      do body1' <- renameProg env body1
         stmts' <- renameProg env stmts
         return (Benchmark (renameExp env e1) body1' () : stmts')
    PrintIntArray e1 e2 () ->
      do stmts' <- renameProg env stmts
         return (PrintIntArray (renameExp env e1) (renameExp env e2) () : stmts')
