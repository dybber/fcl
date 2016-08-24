-- | Pretty print OpenCL kernels
module Language.GPUIL.PrettyOpenCL where

-- A lot of code duplication here due to the lack of ML-style functors

import Language.GPUIL.PrettyLib
import Language.GPUIL.PrettyC hiding (ppExp, ppAttr, ppType, ppStmt, ppStmts, ppFunction, ppProgram)
import Language.GPUIL.Syntax

import Data.List (sort)


ppAttr :: Attribute -> Doc
ppAttr Local = text "__local"
ppAttr Global = text "__global"
ppAttr Volatile = text "volatile"

ppType :: CType -> Doc
ppType CInt32        = text "int"
ppType CDouble       = text "double"
ppType CBool         = text "bool"
ppType CWord8        = text "uchar"
ppType CWord32       = text "uint"
ppType CWord64       = text "ulong"
ppType (CPtr [] t)   = ppType t :+: char '*'
ppType (CPtr attr t) =
  hsep (map ppAttr (sort attr)) :<>: ppType t :+: char '*'

ppExp :: IExp -> Doc
ppExp (IntE c) = int c
ppExp (DoubleE c) = double c
ppExp (BoolE True) = text "true"
ppExp (BoolE False) = text "false"
ppExp (Word8E c) = int c
ppExp (Word32E c) = int c
ppExp (Word64E c) = int c
ppExp (VarE n) = ppVar n
ppExp (UnaryOpE op e) = ppUnaryOp op (ppExp e)
ppExp (BinOpE op e0 e1) = parens $ ppBinOp op (ppExp e0) (ppExp e1)
ppExp (IfE e0 e1 e2) = parens (ppExp e0 :<>: char '?' :<>:
                               ppExp e1 :<>: char ':' :<>:
                               ppExp e2)
ppExp (IndexE n e) = ppVar n :<>: brackets (ppExp e)
ppExp (CastE t e) = parens (parens (ppType t) :<>: ppExp e)
ppExp GlobalID = text "get_global_id(0)"
ppExp LocalID = text "get_local_id(0)"
ppExp GroupID = text "get_group_id(0)"
ppExp LocalSize = text "get_local_size(0)"
ppExp WarpSize = text "_WARPSIZE" -- TODO fetch this from device info-query
ppExp NumGroups = text "get_num_groups(0)"

--ppExp (CallFunE n e) = text n :<>: parens (sep (char ',') $ map ppExp e)

ppStmt :: Statement a -> Doc
ppStmt (For n e body _) =
  let var = ppVar n
  in (text "for (int " :+: var :+: text " = 0; "
        :+: var :+: text " < " :+: ppExp e :+: text "; "
        :+: var :+: text "++) {")
     :+:
       indent (ppStmts body)
     :+:
     Newline
     :+:
     text "}"
ppStmt (While _ e body _) =
     (text "while (" :+: ppExp e :+: text ") {")
     :+:
       indent (ppStmts body)
     :+:
     Newline
     :+:
     text "}"
ppStmt (If e ss_true [] _) =
  text "if " :+: parens (ppExp e) :+: text " {"
  :+:
    indent (ppStmts ss_true)
    :+: Newline :+:
  text "}"
ppStmt (If e ss_true ss_false _) =
  text "if " :+: parens (ppExp e) :+: text " {" :+:
    indent (ppStmts ss_true)
    :+: Newline :+: 
  text "} else {" :+: 
    indent (ppStmts ss_false)
    :+: Newline :+:
  text "}"
ppStmt (Assign n e _) =
  ppVar n :+: text " = " :+: unpar(ppExp e) :+: char ';'
ppStmt (AssignSub n e_idx e _) =
  ppVar n :+: brackets (ppExp e_idx) :+: text " = " :+: unpar(ppExp e) :+: char ';'
ppStmt (Decl n e _) =
  ppDecl n :+: text " = " :+: unpar(ppExp e) :+: char ';'
ppStmt (SyncLocalMem _) = text "barrier(CLK_LOCAL_MEM_FENCE);"
ppStmt (SyncGlobalMem _) = text "barrier(CLK_GLOBAL_MEM_FENCE);"
ppStmt (Allocate (name,_) _ _) = text ("// allocate " ++ name)
ppStmt (Comment msg _) = text ("// " ++ msg)

ppStmts :: [Statement a] -> Doc
ppStmts [] = text ""
ppStmts (s : ss) =  Newline :+: ppStmt s :+: ppStmts ss

ppDecl :: VarName -> Doc
ppDecl (n@(_,t)) = ppType t :+: text " " :+: ppVar n

ppParamList :: [VarName] -> Doc
ppParamList = sep (char ',') . map ppDecl

ppFunction :: Function -> Doc
ppFunction f =
  let returnSig = if isKernel f
                  then text "__kernel void"
                  else case funReturnType f of
                         Nothing -> text "void"
                         Just ty -> text (show ty)
  in 
    returnSig :+: text (funName f) :+: parens (ppParamList (funParams f))
    :+: text " {" :+:
      indent (ppStmts (funBody f))
      :+: Newline
    :+: text "}"

ppProgram :: [Function] -> Doc
ppProgram fs =
  text "#define _WARPSIZE 32" :+: Newline :+:
  sep (Newline :+: Newline) (map ppFunction fs)
