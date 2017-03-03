{-# OPTIONS_GHC -fno-warn-orphans #-}
module FCL.Pretty where

import Prelude hiding ((<$>))
import qualified Data.Map as Map
import Text.PrettyPrint.Leijen

import FCL.Core.Literal

import qualified FCL.External.Syntax as Ext
import qualified FCL.Core.Untyped   as Untyped
import qualified FCL.Core.MonoLevel as Mono
import qualified FCL.Core.Monotyped as Mono
import qualified FCL.Core.PolyLevel as Poly
import qualified FCL.Core.Polytyped as Poly
import FCL.Substitution

-------------------------
-- Rendering as string --
-------------------------
display :: Pretty a => a -> String
display x = displayS (renderPretty 0.4 80 (pretty x)) ""

displayTopLevelUntyped :: Untyped.Exp -> String
displayTopLevelUntyped x = displayS (renderPretty 0.6 80 (prettyTopLevelUntyped x)) ""

displayTopLevelPoly :: Poly.Exp -> String
displayTopLevelPoly x = displayS (renderPretty 0.6 80 (prettyTopLevel x)) ""

displayTopLevelMono :: Mono.Exp -> String
displayTopLevelMono x = displayS (renderPretty 0.6 80 (prettyTopLevelMono x)) ""

------------
-- Common --
------------
period :: Doc
period = char '.'

nestDepth :: Int
nestDepth = 2

ppLvls :: Pretty a => [a] -> Doc
ppLvls lvls = angles (hcat (punctuate comma (map pretty lvls)))

ppParams :: [Doc] -> Doc
ppParams xs = parens (align (sep (punctuate comma xs)))

instance Pretty Literal where
  pretty (LiteralInt i) = int i
  pretty (LiteralDouble d) = double d
  pretty (LiteralBool True) = text "true"
  pretty (LiteralBool False) = text "false"
  pretty (LiteralString d) = dquotes (text d)

------------------
-- External AST --
------------------

prettyParensExt :: Ext.Exp -> Doc
prettyParensExt e =
  case e of
    Ext.App _ _        -> parens (pretty e)
    Ext.Symbol _ []    -> pretty e
    Ext.Symbol _ _     -> parens (pretty e)
    Ext.Lamb _ _       -> parens (pretty e)
    Ext.Cond _ _ _     -> parens (pretty e)
    Ext.Let _ _ _ _ _  -> parens (pretty e)
    Ext.UnaryOp _ _    -> parens (pretty e)
    Ext.BinaryOp _ _ _ -> parens (pretty e)
    Ext.Do _ _         -> parens (pretty e)
    _                  -> pretty e

instance Pretty Ext.UnaryOperator where
  pretty Ext.NegateI = text "~"
  pretty Ext.Not = text "!"

instance Pretty Ext.BinaryOperator where
  pretty Ext.AddI = text "+"
  pretty Ext.SubI = text "-"
  pretty Ext.MulI = text "*"
  pretty Ext.DivI = text "/"
  pretty Ext.ModI = text "%"
  pretty Ext.EqI = text "=="
  pretty Ext.NeqI = text "!="
  pretty Ext.AndI = text "&"
  pretty Ext.OrI = text "|"
  pretty Ext.XorI = text "|"
  pretty Ext.ShiftLI = text "<<"
  pretty Ext.ShiftRI = text ">>"


instance Pretty Ext.Exp where
  pretty e =
    case e of
      Ext.Literal l                   -> pretty l
      Ext.Unit                        -> text "()"
      Ext.Symbol x []                 -> text x
      Ext.Symbol x lvls               -> text x <+> ppLvls lvls
      Ext.Lamb x ebody                -> text "fn" <+> text x <+> text "=>" <+> pretty ebody
      Ext.App e1@(Ext.App _ _) e2 -> pretty e1 <+> prettyParensExt e2
      Ext.App e1@(Ext.Symbol _ _) e2 -> pretty e1 <+> prettyParensExt e2
      Ext.App e1 e2                   -> prettyParensExt e1 <+> prettyParensExt e2
      Ext.Pair e1 e2                  -> parens (align (pretty e1 <> comma </> pretty e2))
      Ext.Cond e1 e2 e3 ->
        text "if" <+> pretty e1 <+>
        text "then" <+> pretty e2 <+>
        text "else" <+> pretty e3
      Ext.Let x anno [] e1 e2 ->
        line <>
        text "let" <+> text x <> prettyAnnotation anno <+> text "="
        <> nest nestDepth (softline <> pretty e1) <+> text "in"
        </> pretty e2 
      Ext.Let x anno lvls e1 e2 ->
        line <>
        text "let" <+> text x <+>
        ppLvls lvls <> prettyAnnotation anno <+> text "="
        <> nest nestDepth (softline <> pretty e1) <+> text "in"
        </> pretty e2 
      Ext.UnaryOp op e1 -> pretty op <+> pretty e1
      Ext.BinaryOp op e1 e2 -> pretty op <+> pretty e1 <+> pretty e2
      Ext.Do lvl stmt ->
        text "do" <+> angles (pretty lvl) </>
          indent 2 (braces (cat (punctuate (char ';' <> softline) (map pretty stmt))))

instance Pretty Ext.DoStmt where
  pretty (Ext.DoExp e) = pretty e
  pretty (Ext.DoBind ident e) = text ident <+> text "<-" <+> pretty e

instance Pretty Ext.FunctionDefinition where
  pretty (Ext.FunctionDefinition
           { Ext.funName = name
           , Ext.funSignature = sig
           , Ext.funQuantifiedLevelVariables = lvlvars
           , Ext.funParameters = params
           , Ext.funBody = expr
           }) =
    let ppSignature =
           case sig of
             Nothing -> empty
             Just tysc -> text "sig" <+> text name <+> colon <+> pretty tysc <> line
    in
      ppSignature
      <> text "fun" <+> text name
      <> (if null lvlvars
          then empty
          else space <> angles (cat (punctuate comma (map pretty lvlvars))))
      <> (if null params
          then empty
          else space <> hsep (map text params))
      <+> text "="
      <+> pretty expr     

instance Pretty Ext.Program where
  pretty (Ext.Program defs) = cat (punctuate (line <> line) (map pretty defs))

-------------------
-- Untyped AST --
-------------------
-- pretty print, where the expression is properly parenthesized
prettyParensUntyped :: Untyped.Exp -> Doc
prettyParensUntyped e =
  case e of
    Untyped.App _ _       -> parens (pretty e)
    Untyped.Symbol _ []   -> pretty e
    Untyped.Symbol _ _    -> parens (pretty e)
    Untyped.Lamb _ _      -> parens (pretty e)
    Untyped.Cond _ _ _    -> parens (pretty e)
    Untyped.Let _ _ _ _ _ -> parens (pretty e)
    _                     -> pretty e

prettyAnnotation :: Maybe Poly.TypeScheme -> Doc
prettyAnnotation Nothing = empty
prettyAnnotation (Just tysc) = space <> colon <+> pretty tysc

-- pretty print top-level let declarations without indentation
prettyTopLevelUntyped :: Untyped.Exp -> Doc
prettyTopLevelUntyped expr =
  case expr of
    Untyped.Let x anno [] e1 e2 ->
      text "let" <+> text x <> prettyAnnotation anno <+> text "="
      <> nest nestDepth (softline <> pretty e1) <+> text "in"
      <> line <> line <> prettyTopLevelUntyped e2
    Untyped.Let x anno lvls e1 e2 ->
      text "let" <+> text x <+>
      ppLvls lvls <> prettyAnnotation anno <+> text "="
      <> nest nestDepth (softline <> pretty e1) <+> text "in"
      <> line <> line <> prettyTopLevelUntyped e2
    _ -> pretty expr

instance Pretty Untyped.Exp where
  pretty e =
    case e of
      Untyped.Literal l                   -> pretty l
      Untyped.Unit                        -> text "()"
      Untyped.Symbol x []                 -> text x
      Untyped.Symbol x lvls               -> text x <+> ppLvls lvls
      Untyped.Lamb x ebody                -> text "fn" <+> text x <+> text "=>" <+> pretty ebody
      Untyped.App e1@(Untyped.App _ _) e2 -> pretty e1 <+> prettyParensUntyped e2
      Untyped.App e1@(Untyped.Symbol _ _) e2 -> pretty e1 <+> prettyParensUntyped e2
      Untyped.App e1 e2                   -> prettyParensUntyped e1 <+> prettyParensUntyped e2
      Untyped.Pair e1 e2                  -> parens (align (pretty e1 <> comma </> pretty e2))
      Untyped.Cond e1 e2 e3 ->
        text "if" <+> pretty e1 <+>
        text "then" <+> pretty e2 <+>
        text "else" <+> pretty e3
      Untyped.Let x anno [] e1 e2 ->
        line <>
        text "let" <+> text x <> prettyAnnotation anno <+> text "="
        <> nest nestDepth (softline <> pretty e1) <+> text "in"
        </> pretty e2 
      Untyped.Let x anno lvls e1 e2 ->
        line <>
        text "let" <+> text x <+>
        ppLvls lvls <> prettyAnnotation anno <+> text "="
        <> nest nestDepth (softline <> pretty e1) <+> text "in"
        </> pretty e2 

-------------------
-- Polymorph AST --
-------------------
instance Pretty Poly.LvlVar where
  pretty (Poly.LvlVar i Nothing) = char 'l' <> int i
  pretty (Poly.LvlVar _ (Just name)) = char 'l' <> text name
  
instance Pretty Poly.Level where
  pretty Poly.Zero                         = text "thread"
  pretty (Poly.Step Poly.Zero)             = text "block"
  pretty (Poly.Step (Poly.Step Poly.Zero)) = text "grid"
  pretty (Poly.Step lvl)                   = text "1+" <> pretty lvl
  pretty (Poly.VarL lvlvar)                = pretty lvlvar

instance Pretty Poly.TyVar where
  pretty (Poly.TyVar i Nothing)     = text "'tv" <> int i
  pretty (Poly.TyVar _ (Just name)) = char '\'' <> text name

prettyTyParens :: Poly.Type -> Doc
prettyTyParens t =
  case t of
    (_ Poly.:> _) -> parens (pretty t)
    _ -> pretty t

instance Pretty Poly.Type where
  pretty t =
    case t of
      Poly.IntT -> text "int"
      Poly.BoolT -> text "bool"
      Poly.DoubleT -> text "double"
      Poly.StringT -> text "string"
      Poly.UnitT -> text "()"
      (t1 Poly.:> t2) -> prettyTyParens t1 <+> text "->" <+> pretty t2
      (t0 Poly.:*: t1) -> parens (pretty t0 <> comma <+> pretty t1)
      (Poly.VarT tyvar) -> pretty tyvar
      (Poly.PullArrayT ty) -> brackets (pretty ty)
      (Poly.PushArrayT lvl ty) -> brackets (pretty ty) <> angles (pretty lvl)
      (Poly.ProgramT lvl ty) -> text "Program" <+> angles (pretty lvl) <+> prettyTyParens ty

instance Pretty Poly.TypeScheme where
  pretty (Poly.TypeScheme [] [] ty) = pretty ty
  pretty tysc =
    case renameTyVars tysc of
      Poly.TypeScheme tyvars lvlvars ty ->
        text "forall"
         <+> hsep (map pretty tyvars)
         <> (if null lvlvars
             then empty
             else space <> angles (cat (punctuate comma (map pretty lvlvars))))
         <> period
         <+> pretty ty

renameTyVars :: Poly.TypeScheme -> Poly.TypeScheme
renameTyVars (Poly.TypeScheme tyvars lvls ty) =
  let newTyVars = zipWith rename tyvars nameSource
      subst = Subst (Map.fromList (zip tyvars (map Poly.VarT newTyVars))) Map.empty
  in Poly.TypeScheme newTyVars lvls (apply subst ty)

rename :: Poly.TyVar -> String -> Poly.TyVar
rename (Poly.TyVar i _) name = Poly.TyVar i (Just name)

nameSource :: [String]
nameSource =
  map (:[]) ['a'..'z']
  ++ ["a" ++ show i | i <- [(0::Integer)..]]

prettyParens :: Poly.Exp -> Doc
prettyParens e =
  case e of
    Poly.Symbol _ [] _   -> pretty e
    Poly.Symbol _ _ _    -> parens (pretty e)
    Poly.App _ _ _       -> parens (pretty e)
    Poly.Lamb _ _ _      -> parens (pretty e)
    Poly.Let _ _ _ _ _ _ -> parens (pretty e)
    Poly.Cond _ _ _ _    -> parens (pretty e)
    _                    -> pretty e

prettyTopLevel :: Poly.Exp -> Doc
prettyTopLevel expr =
  case expr of
    Poly.Let f tysc [] e1 e2 _ ->
--      text "sig" <+> text f <+> colon <+> pretty tysc <$>
      text "let" <+> text f <+> colon <+> pretty tysc <+> text "="
      <> nest nestDepth (softline <> pretty e1) <+> text "in"
      <> line <> line <> prettyTopLevel e2
    Poly.Let f tysc lvls e1 e2 _ ->
--      text "sig" <+> text f <+> colon <+> pretty tysc <$>
      text "let" <+> text f <+>
      ppLvls lvls  <+> colon <+> pretty tysc <+> text "="
      <> nest nestDepth (softline <> pretty e1) <+> text "in"
      <> line <> line <> prettyTopLevel e2
    _ -> pretty expr

instance Pretty Poly.Exp where
  pretty e =
    case e of
      Poly.Literal l _                  -> pretty l
      Poly.Unit _                       -> text "()"
      Poly.Symbol f [] _                -> text f
      Poly.Symbol f lvls _              -> text f <+> ppLvls lvls
      Poly.App e1@(Poly.App _ _ _) e2 _ -> pretty e1 <+> prettyParens e2
      Poly.App e1@(Poly.Symbol _ _ _) e2 _ -> pretty e1 <+> prettyParens e2
      Poly.App e1 e2 _                  -> prettyParens e1 <+> prettyParens e2
      Poly.Pair e1 e2 _                 -> parens (align (pretty e1 <> comma </> pretty e2))
      Poly.Cond e1 e2 e3 _ ->
        text "if" <+> pretty e1 <+>
        text "then" <+> pretty e2 <+>
        text "else" <+> pretty e3
      Poly.Lamb x ebody (t0 Poly.:> _)  ->
        text "fn" <+> text x <+> colon <+> pretty t0 <+> text "=>" <+> pretty ebody
      Poly.Lamb _ _ _ -> error "Compiler error: Lambda not of function type (when pretty printing)."
      Poly.Let f tysc [] e1 e2 _ ->
        line <>
--        text "sig" <+> text f <+> colon <+> pretty tysc <$>
        text "let" <+> text f <+> colon <+> pretty tysc <+> text "="
        <> nest nestDepth (softline <> pretty e1) <+> text "in"
        </> pretty e2 
      Poly.Let f tysc lvls e1 e2 _ ->
        line <>
--        text "sig" <+> text f <+> colon <+> pretty tysc <$>
        text "let" <+> text f <+>
        ppLvls lvls <+> colon <+> pretty tysc <+> text "="
        <> nest nestDepth (softline <> pretty e1) <+> text "in"
        </> pretty e2 

-------------------
-- Monomorph AST --
-------------------
instance Pretty Mono.Level where
  pretty Mono.Zero = text "thread"
  pretty (Mono.Step Mono.Zero) = text "block"
  pretty (Mono.Step (Mono.Step Mono.Zero)) = text "grid"
  pretty (Mono.Step lvl) = text "1+" <> parens (pretty lvl)

prettyMonoTyParens :: Mono.Type -> Doc
prettyMonoTyParens t =
  case t of
    (_ Mono.:> _) -> parens (pretty t)
    _ -> pretty t

instance Pretty Mono.Type where
  pretty t =
    case t of
      Mono.IntT -> text "int"
      Mono.BoolT -> text "bool"
      Mono.DoubleT -> text "double"
      Mono.StringT -> text "string"
      Mono.UnitT -> text "()"
      (t1 Mono.:> t2) -> prettyMonoTyParens t1 <+> text "->" <+> pretty t2
      (t0 Mono.:*: t1) -> parens (pretty t0 <> comma <+> pretty t1)
      (Mono.PullArrayT ty) -> brackets (pretty ty)
      (Mono.PushArrayT lvl ty) -> brackets (pretty ty) <> angles (pretty lvl)
      (Mono.ProgramT lvl ty) -> text "Program" <+> angles (pretty lvl) <+> prettyMonoTyParens ty

prettyParensMono :: Mono.Exp -> Doc
prettyParensMono e =
  case e of
    Mono.App _ _ _    -> parens (pretty e)
    Mono.Lamb _ _ _   -> parens (pretty e)
    Mono.Let _ _ _ _  -> parens (pretty e)
    Mono.Cond _ _ _ _ -> parens (pretty e)
    _                 -> pretty e

prettyTopLevelMono :: Mono.Exp -> Doc
prettyTopLevelMono expr =
  case expr of
    Mono.Let x e1 e2 _ ->
        text "let" <+> text x <+> colon <+> pretty (Mono.typeOf e1) <+> text "="
        <> nest nestDepth (softline <> pretty e1) <+> text "in"
        <> line <> line <> prettyTopLevelMono e2
    _ -> pretty expr

instance Pretty Mono.Exp where
  pretty e =
    case e of
      Mono.Literal l _                  -> pretty l
      Mono.Unit _                       -> text "()"
      Mono.Symbol x _                   -> text x
      Mono.App e1@(Mono.App _ _ _) e2 _ -> pretty e1 <+> prettyParensMono e2
      Mono.App e1@(Mono.Symbol _ _) e2 _ -> pretty e1 <+> prettyParensMono e2
      Mono.App e1 e2 _                  -> prettyParensMono e1 <+> prettyParensMono e2
      Mono.Pair e1 e2 _                 -> parens (align (pretty e1 <> comma </> pretty e2))
      Mono.Cond e1 e2 e3 _ ->
        text "if" <+> pretty e1 <+>
        text "then" <+> pretty e2 <+>
        text "else" <+> pretty e3
      Mono.Lamb x ebody (t0 Mono.:> _)  ->
        text "fn" <+> text x <+> colon <+> pretty t0 <+> text "=>" <+> pretty ebody
      Mono.Lamb _ _ _ -> error "Compiler error: Lambda not of function type (when pretty printing)."
      Mono.Let x e1 e2 _ ->
        line <>
        text "let" <+> text x <+> colon <+> pretty (Mono.typeOf e1) <+> text "="
        <> nest nestDepth (softline <> pretty e1) <+> text "in"
        </> pretty e2 
