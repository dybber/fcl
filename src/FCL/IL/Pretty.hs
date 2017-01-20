module FCL.IL.Pretty (prettyIL, prettyExp) where

import Text.PrettyPrint
import FCL.IL.Syntax

indent :: Doc -> Doc
indent = nest 4

angles :: Doc -> Doc
angles p = char '<' <> p <> char '>'

ppLevel :: ILLevel -> Doc
ppLevel Thread = text "thread"
ppLevel Block = text "block"
ppLevel Grid = text "grid"

ppType :: ILType -> Doc
ppType ILInt = text "int"
ppType ILDouble = text "double"
ppType ILBool = text "bool"
ppType ILString = text "string"
ppType (ILArray ty) = text "Array" <> (brackets (ppType ty))

ppVar :: ILName -> Doc
ppVar (v,_) = text v

ppExp :: ILExp -> Doc
ppExp (EInt i) = int i
ppExp (EDouble d) = double d
ppExp (EBool True) = text "true"
ppExp (EBool False) = text "false"
ppExp (EString s) = quotes (text s)
ppExp (EVar var) = ppVar var
ppExp (EIndex var e) = ppVar var <> (brackets (ppExp e))
ppExp (EUnaryOp op e) = ppUnaryOp op e
ppExp (EBinOp op e1 e2) = ppBinaryOp op e1 e2
ppExp (EIf e1 e2 e3) =
   text "if" <+> ppExp e1
     <+> text "then" <+> ppExp e2
     <+> text "else" <+> ppExp e3

ppUnaryOp :: UnaryOp -> ILExp -> Doc
ppUnaryOp op e1 =
  let opName =
        case op of
          AbsI -> "absi"
          SignI -> "signi"
          AbsD -> "absd"
  in text opName <> parens (ppExp e1)

ppBinaryOp :: BinOp -> ILExp -> ILExp -> Doc
ppBinaryOp op e1 e2 =
  let opName =
        case op of
          AddI -> "addi"
          SubI -> "subi"
          MulI -> "muli"
          DivI -> "divi"
          ModI -> "modi"
          AddD -> "addd"
          EqI -> "eqi"
          NeqI -> "neqi"
          Land -> "and"
          Lor -> "or"
          Xor -> "xor"
          Sll -> "sll"
          Srl -> "srl"
          DivD -> "divd"
  in text opName <> parens (ppExp e1 <> comma <> ppExp e2)

ppStmt :: Stmt a -> Doc
ppStmt (Declare (var, ty) e _) =
  ppType ty <+> text var <+> char '=' <+> ppExp e <> char ';'
ppStmt (Alloc (var,tyarr) tyelem en _) =
  ppType tyarr <+> text var <+>
    text "<- alloc" <+> parens (ppType tyelem <> comma <> ppExp en) <> char ';'
ppStmt (ReadIntCSV (var,tyarr) (varlen,tylen) str _) =
  parens (ppType tyarr <+> text var <> comma <> ppType tylen <+> text varlen)
    <+> text "<- readIntCSV" <+> parens (ppExp str) <> char ';'
ppStmt (PrintIntArray e1 e2 _) =
  text "printIntArray" <+> parens (ppExp e1 <> comma <+> ppExp e2) <> char ';'
ppStmt (Assign n e _) =
  ppVar n <> text " := " <> ppExp e <> char ';'
ppStmt (AssignSub n e_idx e _) =
  ppVar n <> brackets (ppExp e_idx) <> text " := " <> ppExp e <> char ';'
ppStmt (If e ss_true [] _) =
  text "if " <> parens (ppExp e) <> text " {"
    $+$
    indent (ppStmts ss_true)
    $+$
  text "}"
ppStmt (If e ss_true ss_false _) =
  text "if " <> parens (ppExp e) <> text " {"
    $+$
    indent (ppStmts ss_true)
    $+$
  text "} else {" <> 
    indent (ppStmts ss_false)
    $+$
  text "}"
ppStmt (Distribute lvl var e body _) =
  let v = ppVar var
  in (text "distribute" <> angles (ppLevel lvl)  <+> text "(int " <> v <> text " = 0; "
        <> v <> text " < " <> ppExp e <> text "; "
        <> v <> text "++) {")
     $+$
       indent (ppStmts body)
     $+$
     text "}"
ppStmt (ParFor lvl var e body _) =
  let v = ppVar var
  in (text "forall" <> angles (ppLevel lvl)  <+> text "(int " <> v <> text " = 0; "
        <> v <> text " < " <> ppExp e <> text "; "
        <> v <> text "++) {")
     $+$
       indent (ppStmts body)
     $+$
     text "}"
ppStmt (SeqFor var e body _) =
  let v = ppVar var
  in (text "for" <+> text "(int " <> v <> text " = 0; "
        <> v <> text " < " <> ppExp e <> text "; "
        <> v <> text "++) {")
     $+$
       indent (ppStmts body)
     $+$
     text "}"

ppStmt (While e body _) =
     (text "while (" <> ppExp e <> text ") {")
     $+$
       indent (ppStmts body)
     $+$
     text "}"
ppStmt (Benchmark e body _) =
     (text "benchmark (" <> ppExp e <> text ") {")
     $+$
       indent (ppStmts body)
     $+$
     text "}"
ppStmt (Synchronize _) = text "synchronize;"

ppStmts :: [Stmt a] -> Doc
ppStmts stmts = vcat (map ppStmt stmts)

prettyIL :: [Stmt a] -> String
prettyIL program = render (ppStmts program)

prettyExp :: ILExp -> String
prettyExp e = render (ppExp e)
