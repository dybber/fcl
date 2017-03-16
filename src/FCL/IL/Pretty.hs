module FCL.IL.Pretty (prettyIL, prettyExp) where

import Prelude hiding ((<$>))
import Text.PrettyPrint.Leijen
import FCL.IL.Syntax

tabSize :: Int
tabSize = 4

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
ppVar (ILName v id) = text v <> char '_' <> int id

ppExp :: ILExp -> Doc
ppExp (EInt i) = int i
ppExp (EDouble d) = double d
ppExp (EBool True) = text "true"
ppExp (EBool False) = text "false"
ppExp (EString s) = dquotes (text s)
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
ppBinaryOp AddI e1 e2 = parens (ppExp e1 <+> text "+" <+> ppExp e2)
ppBinaryOp SubI e1 e2 = parens (ppExp e1 <+> text "-" <+> ppExp e2)
ppBinaryOp MulI e1 e2 = parens (ppExp e1 <+> text "*" <+> ppExp e2)
ppBinaryOp DivI e1 e2 = parens (ppExp e1 <+> text "/" <+> ppExp e2)
ppBinaryOp ModI e1 e2 = parens (ppExp e1 <+> text "%" <+> ppExp e2)
ppBinaryOp EqI e1 e2 = parens (ppExp e1 <+> text "==" <+> ppExp e2)
ppBinaryOp NeqI e1 e2 = parens (ppExp e1 <+> text "!=" <+> ppExp e2)
ppBinaryOp Sll e1 e2 = parens (ppExp e1 <+> text "<<" <+> ppExp e2)
ppBinaryOp Srl e1 e2 = parens (ppExp e1 <+> text ">>" <+> ppExp e2)
ppBinaryOp Land e1 e2 = parens (ppExp e1 <+> text "&" <+> ppExp e2)
ppBinaryOp Lor e1 e2 = parens (ppExp e1 <+> text "|" <+> ppExp e2)
ppBinaryOp op e1 e2 =
  let opName =
        case op of
          AddD -> "addd"
          Xor -> "xor"
          DivD -> "divd"
  in text opName <> parens (ppExp e1 <> comma <> ppExp e2)

ppStmt :: Stmt a -> Doc
ppStmt (Declare var ty e _) =
  ppType ty <+> ppVar var <+> char '=' <+> ppExp e <> char ';'
ppStmt (Alloc var tyelem en _) =
  ppVar var <+>
    text "= allocate" <> parens (ppType tyelem <> comma <> ppExp en) <> char ';'
ppStmt (ReadIntCSV var varlen str _) =
  parens (ppVar var <> comma <> ppVar varlen)
    <+> text "= readIntCSV" <> parens (ppExp str) <> char ';'
ppStmt (PrintIntArray e1 e2 _) =
  text "printIntArray" <> parens (ppExp e1 <> comma <+> ppExp e2) <> char ';'
ppStmt (Assign n e _) =
  ppVar n <+> text "=" <+> ppExp e <> char ';'
ppStmt (AssignSub n e_idx e _) =
  ppVar n <> brackets (ppExp e_idx) <+> text "=" <+> ppExp e <> char ';'
ppStmt (If e ss_true [] _) =
  text "if " <> parens (ppExp e) <> text " {"
    <$>
    indent tabSize (ppStmts ss_true)
    <$>
  text "}"
ppStmt (If e ss_true ss_false _) =
  text "if " <> parens (ppExp e) <> text " {"
    <$>
    indent tabSize (ppStmts ss_true)
    <$>
  text "} else {" <> 
    indent tabSize (ppStmts ss_false)
    <$>
  text "}"
ppStmt (Distribute lvl var e body _) =
  text "distribute" <> angles (ppLevel lvl)  <+> parens (ppVar var <+> text "<" <+> ppExp e) <+> text "{"
    <$>
      indent tabSize (ppStmts body)
    <$>
    text "}"
ppStmt (ParFor lvl var e body _) =
  text "parfor" <> angles (ppLevel lvl)  <+> parens (ppVar var <+> text "<" <+> ppExp e) <+> text "{"
     <$>
       indent tabSize (ppStmts body)
     <$>
     text "}"
ppStmt (SeqFor var e body _) =
  text "seqfor" <+> parens (ppVar var <+> text "<" <+> ppExp e) <+> text "{"
     <$>
       indent tabSize (ppStmts body)
     <$>
     text "}"
ppStmt (While e body _) =
     text "while" <> parens (ppExp e) <+> text "{"
     <$>
       indent tabSize (ppStmts body)
     <$>
     text "}"
ppStmt (Benchmark e body _) =
     text "benchmark" <> parens (ppExp e) <+> text "{"
     <$>
       indent tabSize (ppStmts body)
     <$>
     text "}"
ppStmt (Synchronize _) = text "synchronize;"

ppStmts :: [Stmt a] -> Doc
ppStmts stmts = vcat (map ppStmt stmts)

prettyIL :: [Stmt a] -> String
prettyIL program = displayS (renderPretty 0.6 80 (ppStmts program)) ""

prettyExp :: ILExp -> String
prettyExp e = displayS (renderPretty 0.6 80 (ppExp e)) ""
