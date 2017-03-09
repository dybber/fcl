{-# LANGUAGE CPP #-}
module FCL.Desugaring where

import qualified Data.Map as Map
import Control.Monad.Trans.RWS
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import FCL.Core.Identifier
import FCL.Core.PolyLevel
import FCL.Core.Polytyped
import qualified FCL.External.Syntax as Ext
import qualified FCL.Core.Untyped as Untyped

data DesugarError =
       DesugarLevelNotInScope Ext.LvlVar
     | DesugarTyVarNotInScope Ext.TyVar
     | DesugarEmptyDo
     | DesugarDoFinalExpIsBind
 deriving Show

data DesugarEnv =
  DesugarEnv {
    lvlEnv :: Map.Map Ext.LvlVar LvlVar
  , tyEnv :: Map.Map Ext.TyVar TyVar
  }

data DesugarState =
  DesugarState {
    varCount :: Int
  }

type Desugar a = RWST DesugarEnv
                      ()
                      DesugarState
                      (Except DesugarError)
                      a

initEnv :: DesugarEnv
initEnv = DesugarEnv Map.empty Map.empty

initState :: DesugarState
initState = DesugarState 0

fresh :: Desugar Int
fresh =
  do i <- gets varCount
     modify (\s -> s { varCount = i + 1})
     return i

freshLvlVar :: Ext.LvlVar -> Desugar LvlVar
freshLvlVar _ =
  do i <- fresh
     return (LvlVar i Nothing)

freshTyVar :: Ext.TyVar -> Desugar TyVar
freshTyVar _ =
  do i <- fresh
     return (TyVar i Nothing)

freshIdent :: Desugar Identifier
freshIdent =
  do i <- gets varCount
     return ("$ignored" ++ show i)

withLvlVars :: [Ext.LvlVar] -> [LvlVar] -> Desugar a -> Desugar a
withLvlVars lvls lvlvars =
  local (\env -> env { lvlEnv = Map.union (lvlEnv env) (Map.fromList (zip lvls lvlvars)) })

withTyVars :: [Ext.TyVar] -> [TyVar] -> Desugar a -> Desugar a
withTyVars tys tyvars =
  local (\env -> env { tyEnv = Map.union (tyEnv env) (Map.fromList (zip tys tyvars)) })

desugar :: Ext.Program -> Either DesugarError Untyped.Exp
desugar (Ext.Program p) =
  do (me, _, _) <- runExcept (runRWST (desugarProgram False p) initEnv initState)
     return me

desugarProgram :: Bool -> [Ext.FunctionDefinition] -> Desugar Untyped.Exp
desugarProgram True [] = return (Untyped.Symbol "main" [])
desugarProgram False [] = return Untyped.Unit
desugarProgram hasMain (f:fs) =
  do let hasMain' = hasMain || Ext.funName f == "main"
     f' <- desugarFunction f
     body <- desugarProgram hasMain' fs
     return (f' body)

desugarFunction :: Ext.FunctionDefinition -> Desugar (Untyped.Exp -> Untyped.Exp)
desugarFunction (Ext.FunctionDefinition
                    { Ext.funName = name
                    , Ext.funSignature = sig
                    , Ext.funQuantifiedLevelVariables = lvls
                    , Ext.funParameters = params
                    , Ext.funBody = e1
                    }) =
  do lvlvars <- mapM freshLvlVar lvls
     let e1fun = foldr Ext.Lamb e1 params
     e1' <- withLvlVars lvls lvlvars (desugarExp e1fun)
     tysc <- desugarTypeScheme sig
     return (\scope -> Untyped.Let name tysc lvlvars e1' scope)

desugarLvl :: Ext.Level -> Desugar Level
desugarLvl lvl =
  case lvl of
    Ext.Zero -> return Zero
    (Ext.Step l) -> Step <$> desugarLvl l
    Ext.VarL x ->
      do env <- asks lvlEnv
         case Map.lookup x env of
           Just lvlvar -> return (VarL lvlvar)
           Nothing -> lift (throwE (DesugarLevelNotInScope x))
                         
desugarUnaryOp :: Ext.UnaryOperator -> Untyped.Exp
desugarUnaryOp op =
  case op of
    Ext.Not     -> Untyped.Symbol ("not")     []

desugarBinaryOp :: Ext.BinaryOperator -> Untyped.Exp
desugarBinaryOp op =
  case op of
    Ext.AddI    -> Untyped.Symbol ("addi") []
    Ext.SubI    -> Untyped.Symbol ("subi") []
    Ext.MulI    -> Untyped.Symbol ("muli") []
    Ext.DivI    -> Untyped.Symbol ("divi") []
    Ext.ModI    -> Untyped.Symbol ("modi") []
    Ext.EqI     -> Untyped.Symbol ("eqi")  []
    Ext.NeqI    -> Untyped.Symbol ("neqi") []
    Ext.AndI    -> Untyped.Symbol ("andi") []
    Ext.OrI     -> Untyped.Symbol ("ori")  []
    Ext.XorI    -> Untyped.Symbol ("xori") []
    Ext.ShiftLI -> Untyped.Symbol ("sll")  []
    Ext.ShiftRI -> Untyped.Symbol ("srl")  []

bind :: Level -> Untyped.Exp -> Untyped.Exp -> Untyped.Exp
bind lvl e1 e2 = Untyped.App (Untyped.App (Untyped.Symbol "bind" [lvl])  e1) e2

desugarTypeScheme :: Maybe Ext.TypeScheme -> Desugar (Maybe TypeScheme)
desugarTypeScheme Nothing = return Nothing
desugarTypeScheme (Just (Ext.TypeScheme lvls tys ty)) =
  do lvlvars <- mapM freshLvlVar lvls
     tyvars <- mapM freshTyVar tys
     ty' <- withLvlVars lvls lvlvars
              (withTyVars tys tyvars (desugarType ty))
     return (Just (TypeScheme lvlvars tyvars ty'))

desugarType :: Ext.Type -> Desugar Type
desugarType Ext.IntT     = return IntT
desugarType Ext.BoolT    = return BoolT
desugarType Ext.DoubleT  = return DoubleT
desugarType Ext.StringT  = return StringT
desugarType Ext.UnitT    = return UnitT
desugarType (Ext.VarT x) =
  do env <- asks tyEnv
     case Map.lookup x env of
       Just tyvar -> return (VarT tyvar)
       Nothing -> lift (throwE (DesugarTyVarNotInScope x))
desugarType (t1 Ext.:> t2)          = (:>) <$> desugarType t1 <*> desugarType t2
desugarType (t1 Ext.:*: t2)         = (:*:) <$> desugarType t1 <*> desugarType t2
desugarType (Ext.PullArrayT t1)     = PullArrayT <$> desugarType t1
desugarType (Ext.PushArrayT lvl t1) = PushArrayT <$> desugarLvl lvl <*> desugarType t1
desugarType (Ext.ProgramT lvl t1)   = ProgramT <$> desugarLvl lvl <*> desugarType t1

desugarDo :: Level -> [Ext.DoStmt] -> Desugar Untyped.Exp
desugarDo _ [] = lift (throwE DesugarEmptyDo)
desugarDo _ [Ext.DoExp e] = desugarExp e
desugarDo _ [Ext.DoBind _ _] = lift (throwE DesugarDoFinalExpIsBind)
desugarDo lvl (Ext.DoBind x e : rest) =
  do e' <- desugarExp e
     body <- desugarDo lvl rest
     return (bind lvl e' (Untyped.Lamb x body))
desugarDo lvl (Ext.DoExp e : rest) =
  do e' <- desugarExp e
     body <- desugarDo lvl rest
     x <- freshIdent
     return (bind lvl e' (Untyped.Lamb x body))

desugarExp :: Ext.Exp -> Desugar Untyped.Exp
desugarExp e =
  case e of
    Ext.Literal l                 -> return (Untyped.Literal l)
    Ext.Unit                      -> return Untyped.Unit
    Ext.Lamb ident ebody          -> Untyped.Lamb ident <$> desugarExp ebody
    Ext.Pair e1 e2                -> Untyped.Pair <$> desugarExp e1 <*> desugarExp e2
    Ext.App e1 e2                 -> Untyped.App <$> desugarExp e1 <*> desugarExp e2
    Ext.Cond e1 e2 e3             -> Untyped.Cond <$> desugarExp e1 <*> desugarExp e2 <*> desugarExp e3
    Ext.Let ident tysc lvls e1 e2 ->
      do lvlvars <- mapM freshLvlVar lvls
         e1' <- withLvlVars lvls lvlvars (desugarExp e1)
         e2' <- desugarExp e2
         tysc' <- desugarTypeScheme tysc
         return (Untyped.Let ident tysc' lvlvars e1' e2')
    Ext.Symbol ident lvls ->
      do lvls' <- mapM desugarLvl lvls
         return (Untyped.Symbol ident lvls')
    Ext.UnaryOp op e1              -> Untyped.App (desugarUnaryOp op) <$> desugarExp e1
    Ext.BinaryOp op e1 e2          ->
      do e1' <- desugarExp e1
         e2' <- desugarExp e2
         return (Untyped.App (Untyped.App (desugarBinaryOp op) e1') e2')
    Ext.Do lvl stmts ->
      do lvl' <- desugarLvl lvl
         desugarDo lvl' stmts
