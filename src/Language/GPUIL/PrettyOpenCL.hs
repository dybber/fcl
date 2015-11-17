-- | Pretty print OpenCL kernels
module Language.GPUIL.PrettyOpenCL where

import Language.GPUIL.PrettyLib
import Language.GPUIL.Syntax
import Data.List (sort)

ppAttr :: Attribute -> Doc
ppAttr Local = text "__local"
ppAttr Global = text "__global"
ppAttr Volatile = text "volatile"

ppType :: CType -> Doc
ppType Int32T       = text "int32"
ppType DoubleT      = text "double"
ppType BoolT        = text "bool" -- maybe this should just be uint32?
ppType Word8T       = text "uint8"
ppType Word32T      = text "uint32"
ppType (Ptr [] t)   = ppType t :+: char '*'
ppType (Ptr attr t) =
  hsep (map ppAttr (sort attr)) :<>: ppType t :+: char '*'

ppVar :: VarName -> Doc
ppVar (v,_) = text v

ppExp :: IExp ty -> Doc
ppExp (IntE c) = int c
ppExp (DoubleE c) = double c
ppExp (BoolE True) = text "true"
ppExp (BoolE False) = text "false"
ppExp (Word8E c) = int (fromIntegral c)
ppExp (Word32E c) = int (fromIntegral c)
ppExp (VarE n _) = ppVar n
ppExp (UnaryOpE op e) = ppUnaryOp op e
ppExp (BinOpE op e0 e1) = parens $ ppBinOp op (ppExp e0) (ppExp e1)
ppExp (IfE e0 e1 e2 _) = parens (ppExp e0 :<>: char '?' :<>:
                                 ppExp e1 :<>: char ':' :<>:
                                 ppExp e2)
ppExp (IndexE n e) = ppVar n :<>: brackets (ppExp e)
ppExp (CastE t e) = parens (ppType t) :<>: ppExp e
ppExp GlobalID = text "get_global_id(0)"
ppExp LocalID = text "get_local_id(0)"
ppExp GroupID = text "get_group_id(0)"
ppExp LocalSize = text "get_local_size(0)"
ppExp WarpSize = text "_WARPSIZE" -- TODO fetch this from device info-query
ppExp NumGroups = text "get_num_groups(0)"

--ppExp (CallFunE n e) = text n :<>: parens (sep (char ',') $ map ppExp e)

ppUnaryOp :: UnaryOp -> IExp ty -> Doc
ppUnaryOp op e =
  case op of
   I2D -> ppExp (CastE DoubleT e)
   _ -> name :+: parens (ppExp e)
  where
     name =
       case op of
         Not -> char '!'
         NegateInt -> char '-'
         NegateDouble -> char '-'
         NegateBitwise -> char '~'
         Floor -> text "floor"
         Ceil -> text "ceil"
         Exp -> text "exp"
         Ln -> text "ln"
         AbsI -> text "abs"
         AbsD -> text "abs"
         _ -> error "can not happen"

ppBinOp :: BinOp -> Doc -> Doc -> Doc
ppBinOp AddI d0 d1 = d0 :<>: char '+' :<>: d1
ppBinOp AddD d0 d1 = d0 :<>: char '+' :<>: d1
ppBinOp SubI d0 d1 = d0 :<>: char '-' :<>: d1
ppBinOp SubD d0 d1 = d0 :<>: char '-' :<>: d1
ppBinOp MulI d0 d1 = d0 :<>: char '*' :<>: d1
ppBinOp MulD d0 d1 = d0 :<>: char '*' :<>: d1
ppBinOp DivI d0 d1 = d0 :<>: char '/' :<>: d1
ppBinOp DivD d0 d1 = d0 :<>: char '/' :<>: d1
ppBinOp ModI d0 d1 = d0 :<>: char '%' :<>: d1
ppBinOp LtI  d0 d1 = d0 :<>: char '<' :<>: d1
ppBinOp LtD  d0 d1 = d0 :<>: char '<' :<>: d1
ppBinOp GtI  d0 d1 = d0 :<>: char '>' :<>: d1
ppBinOp GtD  d0 d1 = d0 :<>: char '>' :<>: d1
ppBinOp LteI d0 d1 = d0 :<>: text "<=" :<>: d1
ppBinOp LteD d0 d1 = d0 :<>: text "<=" :<>: d1
ppBinOp GteI d0 d1 = d0 :<>: text ">=" :<>: d1
ppBinOp GteD d0 d1 = d0 :<>: text ">=" :<>: d1
ppBinOp EqI  d0 d1 = d0 :<>: text "==" :<>: d1
ppBinOp EqD  d0 d1 = d0 :<>: text "==" :<>: d1
ppBinOp NeqI d0 d1 = d0 :<>: text "!=" :<>: d1
ppBinOp NeqD d0 d1 = d0 :<>: text "!=" :<>: d1
ppBinOp And  d0 d1 = d0 :<>: text "&&" :<>: d1
ppBinOp Or   d0 d1 = d0 :<>: text "||" :<>: d1
ppBinOp Land d0 d1 = d0 :<>: text "&" :<>: d1
ppBinOp Lor  d0 d1 = d0 :<>: text "|" :<>: d1
ppBinOp Xor  d0 d1 = d0 :<>: text "^" :<>: d1
ppBinOp Sll  d0 d1 = d0 :<>: text "<<" :<>: d1
ppBinOp Srl  d0 d1 = d0 :<>: text ">>" :<>: d1

ppStmt :: Statement a ty -> Doc
ppStmt (For n e body) =
  let var = ppVar n
  in (text "for (int " :+: var :+: text " = 0; "
        :+: var :+: text " < " :+: ppExp e :+: char ';'
        :+: var :+: text "++) {")
     :+:
       indent (ppStmts body)
     :+:
     Newline
     :+:
     text "}"
ppStmt (SeqWhile e body) =
     (text "while (" :+: ppExp e :+: text ") {")
     :+:
       indent (ppStmts body)
     :+:
     Newline
     :+:
     text "}"
ppStmt (If e ss_true []) =
  text "if " :+: parens (ppExp e) :+: text " {"
  :+:
    indent (ppStmts ss_true)
    :+: Newline :+:
  text "}"
ppStmt (If e ss_true ss_false) =
  text "if " :+: parens (ppExp e) :+: text " {" :+:
    indent (ppStmts ss_true)
    :+: Newline :+: 
  text "} else {" :+: 
    indent (ppStmts ss_false)
    :+: Newline :+:
  text "}"
ppStmt (Assign n e) =
  ppVar n :+: text " = " :+: unpar(ppExp e) :+: char ';'
ppStmt (AssignSub n e_idx e) =
  ppVar n :+: brackets (ppExp e_idx) :+: text " = " :+: unpar(ppExp e) :+: char ';'
ppStmt (Decl n (Just e)) =
  ppDecl n :+: text " = " :+: unpar(ppExp e) :+: char ';'
ppStmt (Decl n Nothing) =
  ppDecl n :+: char ';'
ppStmt SyncLocalMem = text "barrier(CLK_LOCAL_MEM_FENCE);"
ppStmt SyncGlobalMem = text "barrier(CLK_GLOBAL_MEM_FENCE);"
ppStmt (ForAll lvl n e body) =
  let var = ppVar n
  in (text "forall{" :+: ppLevel lvl :+: text "} (int " :+: var :+: text " = 0; "
        :+: var :+: text " < " :+: ppExp e :+: char ';'
        :+: var :+: text "++) {")
     :+:
       indent (ppStmts body)
     :+:
     Newline
     :+:
     text "}"
ppStmt (DistrPar _ _ _ _) =
  error $ concat ["Cannot pretty print DistrPar, ",
                  "use `XX.funcYY` to convert to sequential for-loops"]
-- ppStmt (ForAll _ _ _ _) =
--   error $ concat ["Cannot pretty print ForAll, ",
--                   "use `XX.funcYY` to convert to sequential for-loops"]
ppStmt (Allocate _ _ _) = text "// TODO: allocate array" -- TODO


ppLevel :: Level -> Doc
ppLevel Thread = text "thread"
ppLevel Warp   = text "warp"
ppLevel Block  = text "block"
ppLevel Grid   = text "grid"



ppStmts :: Statements a ty -> Doc
ppStmts [] = text ""
ppStmts ((s, _) : ss) =  Newline :+: ppStmt s :+: ppStmts ss

ppDecl :: VarName -> Doc
ppDecl (n@(_,t)) = ppType t :+: text " " :+: ppVar n

ppParamList :: [VarName] -> Doc
ppParamList = sep (char ',') . map ppDecl

ppKernel :: Kernel ty -> Doc
ppKernel k =
  text "#define _WARPSIZE 32" :+: Newline :+:
  text "_kernel void " :+: text (kernelName k) :+: parens (ppParamList (kernelParams k))
  :+: text " {" :+:
    indent (ppStmts (kernelBody k))
  :+: Newline
  :+: text "}"
