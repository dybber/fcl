-- | Pretty print CUDA kernels
module Language.CGen.PrettyCUDA where

import Language.CGen.PrettyLib
import Language.CGen.PrettyC hiding (ppExp, ppAttr, ppType, ppStmt, ppStmts, ppFunction, ppProgram)
import Language.CGen.Syntax
import Data.List (sort)

ppAttr :: Attribute -> Doc
ppAttr Local = text "__shared__"
ppAttr Global = text "__device__"
ppAttr Volatile = text "volatile"

ppType :: CType -> Doc
ppType CInt32        = text "int32_t"
ppType CDouble       = text "double"
ppType CBool         = text "bool" -- maybe this should just be uint32?
ppType CWord8        = text "uint8_t"
ppType CWord32       = text "uint32_t"
ppType CWord64       = text "uint64_t"
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
ppExp GlobalID = parens (text "threadIdx.x + (blockDim.x * blockIdx.x)")
ppExp LocalID = text "threadIdx.x"
ppExp GroupID = text "blockIdx.x"
ppExp LocalSize = text "blockDim.x"
ppExp WarpSize = text "_WARPSIZE" -- TODO fetch this from device info-query
ppExp NumGroups = text "gridDim.x"

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
ppStmt (SyncLocalMem _) = text "__syncthreads();"
ppStmt (SyncGlobalMem _) = error "SyncGlobalMem in kernels not supported in CUDA"
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
                  then text "extern \"C\" __global__ void "
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
