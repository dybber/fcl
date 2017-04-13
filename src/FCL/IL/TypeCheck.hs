module FCL.IL.TypeCheck where

import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

import qualified Data.Map as Map
import FCL.IL.Syntax

typecheck :: [Stmt a] -> Either TypeError TypeEnv
typecheck prog = runExcept (execStateT (checkStmts prog) Map.empty )

type TypeEnv = Map.Map ILName ILType
data TypeError = TypeMismatch
               | NotInScope ILName
  deriving Show
type Check a = StateT TypeEnv (Except TypeError) a

throwError :: TypeError -> Check a
throwError = lift . throwE

lookupVar :: ILName -> Check ILType
lookupVar x =
  do env <- get
     case Map.lookup x env of
       Just e  -> return e
       Nothing -> throwError (NotInScope x)

infer :: ILExp -> Check ILType
infer e =
  case e of
    EInt _          -> return ILInt
    EDouble _       -> return ILDouble
    EBool _         -> return ILBool
    EString _       -> return ILString
    EVar x          -> lookupVar x
    EUnaryOp op e1  -> inferUnaryOp op e1
    EBinOp op e1 e2 -> inferBinaryOp op e1 e2
    EIndex x e' ->
      do check e' ILInt
         ty' <- lookupVar x
         case ty' of
           ILArray elemty -> return elemty
           _ -> throwError TypeMismatch
    EIf e1 e2 e3->
      do check e1 ILBool
         ty <- infer e2
         check e3 ty
         return ty

inferUnaryOp :: UnaryOp -> ILExp -> Check ILType
inferUnaryOp op e =
  case op of
    SignI -> do check e ILInt
                return ILInt
    AbsI -> do check e ILInt
               return ILInt
    AbsD -> do check e ILDouble
               return ILDouble
    CLZ -> do check e ILInt
              return ILInt
    B2I -> do check e ILBool
              return ILInt
    I2D -> do check e ILInt
              return ILDouble

inferBinaryOp :: BinOp -> ILExp -> ILExp -> Check ILType
inferBinaryOp op e1 e2 =
  if elem op [AddI, SubI, MulI, DivI, ModI, Land, Lor, Xor, Sll, Srl, MinI, MaxI]
  then do check e1 ILInt
          check e2 ILInt
          return ILInt
  else
  if elem op [AddD, SubD, MulD, DivD]
  then do check e1 ILDouble
          check e2 ILDouble
          return ILDouble
  else
  if elem op [LtI, LteI, GtI, GteI, EqI, NeqI]
  then do check e1 ILInt
          check e2 ILInt
          return ILBool
  else
  if elem op [LtD, LteD, GtD, GteD, EqD, NeqD]
  then do check e1 ILDouble
          check e2 ILDouble
          return ILBool
  else do check e1 ILBool
          check e2 ILBool
          return ILBool
   
check :: ILExp -> ILType -> Check ()
check e ty =
  do ty' <- infer e
     if (ty /= ty')
       then throwError TypeMismatch
       else return ()

addVar :: ILName -> ILType -> Check ()
addVar x ty = modify (Map.insert x ty)

checkStmts :: [Stmt a] -> Check ()
checkStmts [] = return ()
checkStmts (st:stmts) =
  do checkStmt st
     checkStmts stmts

checkStmt :: Stmt a -> Check ()
checkStmt s =
  case s of
    Declare x ty e _ ->
      do check e ty
         addVar x ty
    Alloc x ty e _ ->
      do check e ILInt
         addVar x (ILArray ty)
    Distribute _ x bound body _ ->
      do check bound ILInt
         addVar x ILInt
         checkStmts body
    ParFor _ x bound body _ ->
      do check bound ILInt
         addVar x ILInt
         checkStmts body
    Synchronize _ ->
      return ()
    Assign x e _ ->
      do ty <- lookupVar x
         check e ty
    AssignSub x e1 e2 _ ->
      do check e1 ILInt
         ty <- lookupVar x
         case ty of
           ILArray elemty -> check e2 elemty
           _ -> throwError TypeMismatch
    If e1 body1 body2 _ ->
      do check e1 ILBool
         checkStmts body1
         checkStmts body2
    While e1 body1 _ ->
      do check e1 ILBool
         checkStmts body1
    SeqFor x bound body _ ->
      do check bound ILInt
         addVar x ILInt
         checkStmts body
    ReadIntCSV x xlen e _ ->
      do check e ILString
         addVar x (ILArray ILInt)
         addVar xlen ILInt
    Benchmark e1 body1 _ ->
      do check e1 ILInt
         checkStmts body1
    PrintIntArray e1 e2 _ ->
      do check e1 ILInt
         check e2 (ILArray ILInt)
    PrintDoubleArray e1 e2 _ ->
      do check e1 ILInt
         check e2 (ILArray ILDouble)
