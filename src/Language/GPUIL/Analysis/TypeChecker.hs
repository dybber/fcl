module Language.GPUIL.Analysis.TypeChecker where

-------- TODO --------
-- * Check levels
-- * Figure out what to do with get_local_size etc
--   are they int32, int64, uint32, uint64?
----------------------
import Language.GPUIL.Syntax
import qualified Data.Map as Map

import Control.Monad (when, foldM_)

data Status = Success | Error String

typeCheck :: [VarName] -> [Statement a] -> Status
typeCheck params stmts = runErr $ checkStmts (initEnv params) stmts

newtype Env = Env (Map.Map String CType)

initEnv :: [VarName] -> Env
initEnv params = Env (Map.fromList params)

addVar :: Env -> VarName -> Err Env
addVar (Env env) (x, ty) =
  case Map.lookup x env of
    Just _  -> throw "Variable already exists"
    Nothing -> return (Env (Map.insert x ty env))

checkVar :: Env -> VarName -> Err CType
checkVar (Env env) (x,ty) =
  case Map.lookup x env of
    Just ty' | ty == ty' -> return ty
             | otherwise ->
                 throw (concat ["Annotated type ("
                               , show ty
                               , ") on variable usage, does not match type ("
                               , show ty'
                               ,") at definition (variable "
                               , x
                               , ")"])
    Nothing -> throw ("Typechecking error: Variable not defined: " ++ x)

type Err a = Either String a

runErr :: Err () -> Status
runErr e =
  case e of
    Left msg -> Error msg
    Right () -> Success

throw :: String -> Err a
throw = Left

checkStmts :: Env -> [Statement a] -> Err ()
checkStmts env stmts = foldM_ checkStmt env stmts

checkStmt :: Env -> Statement a -> Err Env
checkStmt env (If e0 ss0 ss1 _) =
  do ty0 <- checkExp env e0
     when (ty0 /= CBool) $ throw "First conditional argument should be boolean"
     checkStmts env ss0
     checkStmts env ss1
     return env
checkStmt env (Assign var e0 _) =
  do tyVar <- checkVar env var
     ty0 <- checkExp env e0
     when (tyVar /= ty0) $ throw ("Types does not match in assignment " ++ show var)
     return env
checkStmt env (AssignSub var e0 e1 _) =
  do tyVar <- checkVar env var
     ty0 <- checkExp env e0
     ty1 <- checkExp env e1
     case (tyVar, ty0, ty1) of
       (CPtr _ ty, CInt32, ty') | ty == ty' -> return env
                                | otherwise -> throw "Types does not match in assignment"
       (CPtr _ _, _, _) -> throw "Subscript should be integer typed"
       _ -> throw "Subscript assignment to non-array value"
checkStmt env (Decl var@(x,ty) e0 _) =
  do ty0 <- checkExp env e0
     when (ty0 /= ty) $ throw $ "Right hand side in declaration of " ++ x ++ " , does not match declared type."
     addVar env var
checkStmt env (SyncLocalMem _) = return env
checkStmt env (SyncGlobalMem _) = return env
checkStmt env (Comment _ _) = return env
checkStmt env (Allocate var e0 _) =
  do ty0 <- checkExp env e0
     when (ty0 /= CInt32) $ throw "Size of allocation should be specified as an integer"
     addVar env var
checkStmt env (For var@(_,ty) e0 ss _) =
  do ty0 <- checkExp env e0
     when (ty0 /= ty || ty /= CInt32) $ throw "Loop variable should be integer"
     newEnv <- addVar env var
     checkStmts newEnv ss
     return env
checkStmt env (SeqWhile e0 ss _) =
  do ty0 <- checkExp env e0
     when (ty0 /= CBool) $ throw "Loop variable should be bool in while-loop"
     checkStmts env ss
     return env

checkExp :: Env -> IExp -> Err CType
checkExp _ (IntE _)    = return CInt32
checkExp _ (DoubleE _) = return CDouble
checkExp _ (BoolE _)   = return CBool
checkExp _ (Word8E _)  = return CWord8
checkExp _ (Word32E _) = return CWord32
checkExp _ (Word64E _) = return CWord64
checkExp _ GlobalID    = return clDeviceAddressBits
checkExp _ LocalID     = return clDeviceAddressBits
checkExp _ GroupID     = return clDeviceAddressBits
checkExp _ LocalSize   = return clDeviceAddressBits
checkExp _ NumGroups   = return clDeviceAddressBits
checkExp _ WarpSize    = return clDeviceAddressBits
checkExp env (VarE var) = checkVar env var
checkExp env (UnaryOpE op e0) = do
  ty0 <- checkExp env e0
  checkUnOp op ty0
checkExp env (BinOpE op e0 e1) = do
  ty0 <- checkExp env e0
  ty1 <- checkExp env e1
  checkBinOp op ty0 ty1
checkExp env (IfE e0 e1 e2) = do
  ty0 <- checkExp env e0
  ty1 <- checkExp env e1
  ty2 <- checkExp env e2
  when (ty0 /= CBool) (throw "Conditional argument does not have type bool")
  when (ty1 /= ty2) (throw "Branches in exp-level conditional does not match")
  return ty1
checkExp env (IndexE var e0) = do
  ty0 <- checkExp env e0
  when (ty0 /= CInt32) $ throw "Indexing argument must be of integer type"
  tyVar <- checkVar env var
  case tyVar of
    CPtr _ ty -> return ty
    _         -> throw "Trying to index into non-array"
checkExp env (CastE ty e0) = do _ <- checkExp env e0
                                return ty

-- CL_DEVICE_ADDRESS_BITS
-- TODO in reality this is platform dependent and likely unsigned 64
-- bits on most platforms.
clDeviceAddressBits :: CType
clDeviceAddressBits = CInt32

checkBinOp :: BinOp -> CType -> CType -> Err CType
checkBinOp AddI CInt32 CInt32 = return CInt32
checkBinOp SubI CInt32 CInt32 = return CInt32
checkBinOp MulI CInt32 CInt32 = return CInt32
checkBinOp DivI CInt32 CInt32 = return CInt32
checkBinOp ModI CInt32 CInt32 = return CInt32
checkBinOp AddD CDouble CDouble = return CDouble
checkBinOp SubD CDouble CDouble = return CDouble
checkBinOp MulD CDouble CDouble = return CDouble
checkBinOp DivD CDouble CDouble = return CDouble
checkBinOp AddPtr (ty@(CPtr _ CWord8)) CInt32 = return ty
checkBinOp LtI  CInt32 CInt32 = return CBool
checkBinOp LteI CInt32 CInt32 = return CBool
checkBinOp GtI  CInt32 CInt32 = return CBool
checkBinOp GteI CInt32 CInt32 = return CBool
checkBinOp EqI  CInt32 CInt32 = return CBool
checkBinOp NeqI CInt32 CInt32 = return CBool
checkBinOp LtD  CDouble CDouble = return CBool
checkBinOp LteD CDouble CDouble = return CBool
checkBinOp GtD  CDouble CDouble = return CBool
checkBinOp GteD CDouble CDouble = return CBool
checkBinOp EqD  CDouble CDouble = return CBool
checkBinOp NeqD CDouble CDouble = return CBool
checkBinOp And  CBool CBool = return CBool
checkBinOp Or   CBool CBool = return CBool
checkBinOp Land CInt32 CInt32 = return CInt32
checkBinOp Lor  CInt32 CInt32 = return CInt32
checkBinOp Xor  CInt32 CInt32 = return CInt32
checkBinOp Sll  CInt32 CInt32 = return CInt32
checkBinOp Srl  CInt32 CInt32 = return CInt32
checkBinOp op ty0 ty1 = throw $ concat
                          ["Unexpected argument types ("
                          , show ty0
                          , ", "
                          , show ty1
                          , ") to binary operator "
                          , show op]

checkUnOp :: UnaryOp -> CType -> Err CType
checkUnOp Not CBool = return CBool
checkUnOp I2D CInt32 = return CDouble
checkUnOp NegateInt CInt32 = return CInt32
checkUnOp NegateBitwise CInt32 = return CInt32
checkUnOp NegateDouble CDouble = return CDouble
checkUnOp AbsI CInt32 = return CInt32
checkUnOp AbsD CDouble = return CDouble
checkUnOp Ceil CDouble = return CDouble
checkUnOp Floor CDouble = return CDouble
checkUnOp Exp CDouble = return CDouble
checkUnOp Ln CDouble = return CDouble
checkUnOp op ty0 = throw $ concat
                          ["Unexpected argument type ("
                          , show ty0
                          , ") to unary operator "
                          , show op]
