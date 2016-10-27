module Language.FCL.Pretty (prettyPrintType, prettyPrintExp, prettyPrintDef, prettyPrintProgram, showType) where

import Text.PrettyPrint
import Language.FCL.Syntax

import Control.Monad.Trans.State
import qualified Data.Map as Map

angles :: Doc -> Doc
angles p = char '<' <> p <> char '>'

prettyPrintType :: Type -> String
prettyPrintType t = render $ evalState (ppType t) startEnv

prettyPrintExp :: Exp Type -> String
prettyPrintExp e = render $ evalState (pp e) startEnv

prettyPrintDef :: Definition Type -> String
prettyPrintDef d = render $ evalState (ppDef d) startEnv

prettyPrintProgram :: Program Type -> String
prettyPrintProgram prog = render $ evalState (ppProgram prog) startEnv

showType :: Definition Type -> String
showType (Definition v _ _ _ _ e) = v ++ " : " ++ prettyPrintType (typeOf e)


-- Names used when pretty printing type variables
tyVarNames :: [String]
tyVarNames =
  map (:[]) ['a'..'z']
   ++
  ['t' : show (i :: Int) | i <- [0..]]

-- Names used when pretty printing level variables
lvlVarNames :: [String]
lvlVarNames = ["lvl" ++ show (i :: Int) | i <- [0..]]


-- Pretty printing monad
type PP a = State PPEnv a


data PPEnv =
  PPEnv { unusedTy :: [String]
        , unusedLvl :: [String]
        , usedNames :: Map.Map Int String
        }

startEnv :: PPEnv
startEnv = PPEnv tyVarNames lvlVarNames Map.empty

getName :: Int -> PP String
getName i =
 do used <- gets usedNames
    case Map.lookup i used of
       Just name -> return name
       Nothing ->
         do (name:xs) <- gets unusedTy
            modify (\env -> env { unusedTy = xs,
                                  usedNames = Map.insert i name used })
            return name

getLvlName :: Int -> PP String
getLvlName i =
 do used <- gets usedNames
    case Map.lookup i used of
       Just name -> return name
       Nothing ->
         do (name:xs) <- gets unusedLvl
            modify (\env -> env { unusedLvl = xs,
                                  usedNames = Map.insert i name used })
            return name


ppProgram :: Program Type -> PP Doc
ppProgram ds = vcat <$> mapM ppDef ds

ppDef :: Definition Type -> PP Doc
ppDef d =
  do let fnName = text (defVar d)
     sig <- case defSignature d of
              Nothing -> return (text "")
              Just ty ->
                 do ty' <- ppType ty
                    return (text "sig" <+> fnName <+> char ':' <+> ty')
     body <- pp (defBody d)
     let decl = if defEmitKernel d
                then text "kernel"
                else text "fun"
     return (sig
             $+$
             decl <+> fnName <+> char '=' <+> body)


-- Pretty print types
ppType :: Type -> PP Doc
ppType IntT = return (text "int")
ppType BoolT = return (text "bool")
ppType DoubleT = return (text "double")
ppType (VarT (TyVar i Nothing)) =
  do name <- getName i
     return (text name)
ppType (VarT (TyVar _ (Just v))) = return (text v)
ppType (ty0 :> ty1)  =
  do ty0' <- ppType ty0
     ty1' <- ppType ty1
     return (parens (ty0' <+> text "->" <+> ty1'))
ppType (lvlvar :-> ty)  =
  do lvlvar' <- ppLvlVar lvlvar
     ty' <- ppType ty
     return (parens (angles lvlvar' <+> text "->" <+> ty'))
ppType (ty0 :*: ty1)  =
  do ty0' <- ppType ty0
     ty1' <- ppType ty1
     return (parens (ty0' <> char ',' <+> ty1'))
ppType (PullArrayT ty) = return . brackets =<< ppType ty
ppType (PushArrayT lvl ty) =
  do ty' <- ppType ty
     prettylvl <- ppLevel lvl
     return (brackets ty' <> angles prettylvl)

ppLvlVar :: LvlVar -> PP Doc
ppLvlVar (LvlVar i Nothing) =
  do name <- getLvlName i
     return (text name)
ppLvlVar (LvlVar i (Just name)) =
  return (text name <> int i)

-- Pretty print levels
ppLevel :: Level -> PP Doc
ppLevel Zero = return (text "thread")
ppLevel (Step Zero) = return (text "warp")
ppLevel (Step (Step Zero)) = return (text "block")
ppLevel (Step (Step (Step Zero))) = return (text "grid")
ppLevel (Step lvl) = do
  prettylvl <- ppLevel lvl
  return (parens (text "1+" <> prettylvl))
ppLevel (VarL lvlvar) = ppLvlVar lvlvar

pp :: Exp Type -> PP Doc
pp (IntScalar i _)       = return (int i)
pp (DoubleScalar d _)    = return (double d)
pp (BoolScalar True _)   = return (text "true")
pp (BoolScalar False _)  = return (text "false")
pp (UnOp op e1 _)        = parens <$> (ppUnOp op e1)
pp (BinOp op e1 e2 _)    = parens <$> (ppBinOp op e1 e2)
pp (Var x _ _)          =
  return (text x)
pp (Vec es _ _)          =
  do es' <- mapM pp es
     return (brackets (hsep (punctuate (char ',') es')))
pp (App e1 e2)           =
  do e1' <- pp e1
     e2' <- pp e2
     return (parens (e1' <+> e2'))
pp (Lamb x _ e1 _ _) =
  do e1' <- pp e1
     return (
           parens (text "fn" <+> text x
                   <+> text "=>" <+> e1'))
pp (AppLvl e lvl)           =
  do e' <- pp e
     lvl' <- ppLevel lvl
     return (parens (e' <+> angles lvl'))
pp (LambLvl lvlvar ebody _ _) =
  do ebody' <- pp ebody
     name <- ppLvlVar lvlvar
     return (parens (text "fn" <+> angles name
                     <+> text "=>" <+> ebody'))
pp (Let x e1 e2 _ _)    =
  do e1' <- pp e1
     e2' <- pp e2
     return (
                parens (text "let" <+> text x
                        <+> text "=" <+> e1'
                        <+> text "in" <+> e2')
            )
pp (Cond e1 e2 e3 _ _) =
  do e1' <- pp e1
     e2' <- pp e2
     e3' <- pp e3
     return (text "if" <+> e1'
             <+>
             text "then" <+> e2'
             <+>
             text "else" <+> e3')
pp (Pair e1 e2 _)            =
    do e1' <- pp e1
       e2' <- pp e2
       return (parens (e1' <> char ',' <+> e2'))
pp (Proj1E e1 _)             = parens <$> (text "fst" <+>) <$> pp e1
pp (Proj2E e1 _)             = parens <$> (text "snd" <+>) <$> pp e1
pp (Index e1 e2 _)           =
  do e1' <- pp e1
     e2' <- pp e2
     return (text "index" <+> e1' <+> e2')
pp (LengthPull e1 _)         = parens <$> (text "lengthPull" <+>) <$> pp e1
pp (LengthPush e1 _)         = parens <$> (text "lengthPush" <+>) <$> pp e1
pp (While e1 e2 e3 _)        =
  do e1' <- pp e1
     e2' <- pp e2
     e3' <- pp e3
     return (parens (text "while" <+> e1' <+> e2' <+> e3'))
pp (WhileSeq e1 e2 e3 _)        =
  do e1' <- pp e1
     e2' <- pp e2
     e3' <- pp e3
     return (parens (text "whileSeq" <+> e1' <+> e2' <+> e3'))
pp (GeneratePull e1 e2 _)    =
  do e1' <- pp e1
     e2' <- pp e2
     return (parens (text "generatePull" <+> e1' <+> e2'))
pp (MapPull e1 e2 _)    =
  do e1' <- pp e1
     e2' <- pp e2
     return (parens (text "mapPull" <+> e1' <+> e2'))
pp (MapPush e1 e2 _)    =
  do e1' <- pp e1
     e2' <- pp e2
     return (parens (text "mapPush" <+> e1' <+> e2'))
pp (Force e1 _)              =
  do e1' <- pp e1
     return (parens (text "force" <+> e1'))
pp (Push lvl e1 _)            =
  do e1' <- pp e1
     lvl' <- ppLevel lvl
     return (parens (parens (text "push" <> angles lvl' <+> e1')))
pp (Concat e1 e2 _)          =
  do e1' <- pp e1
     e2' <- pp e2
     return (parens (text "concat" <+> e1' <+> e2'))
pp (Interleave e1 e2 e3 _)          =
  do e1' <- pp e1
     e2' <- pp e2
     e3' <- pp e3
     return (parens (text "interleave" <+> e1' <+> e2' <+> e3'))
pp (BlockSize _)             = return (text "#BlockSize")
pp (Scanl e1 e2 e3 _)        =
  do e1' <- pp e1
     e2' <- pp e2
     e3' <- pp e3
     return (parens (text "scanl" <+> e1' <+> e2' <+> e3'))

ppUnOp :: UnOp -> Exp Type -> PP Doc
ppUnOp op e1 =
  let opName =
        case op of
          AbsI -> "absi"
          SignI -> "signi"
          NegateI -> "negatei"
          Not -> "not"
          I2D -> "i2d"
          B2I -> "b2i"
          CLZ -> "clz"
  in do e1' <- pp e1
        return (text opName <+> e1')

ppBinOp :: BinOp -> Exp Type -> Exp Type -> PP Doc
ppBinOp op e1 e2 =
  let opName =
        case op of
          AddI -> "addi"
          SubI -> "subi"
          MulI -> "muli"
          DivI -> "divi"
          ModI -> "modi"
          MinI -> "mini"
          EqI -> "eqi"
          NeqI -> "neqi"
          AndI -> "andi"
          OrI -> "ori"
          XorI -> "xori"
          ShiftLI -> "shiftli"
          ShiftRI -> "shiftri"
          PowI -> "powi"
          DivR -> "divr"
          PowR -> "powr"
  in do e1' <- pp e1
        e2' <- pp e2
        return (text opName <+> e1' <+> e2')


