module Language.GPUIL.PrettyC where

import Language.GPUIL.PrettyLib
import Language.GPUIL.Syntax


ppUnaryOp :: UnaryOp -> Doc -> Doc
ppUnaryOp op d0 = name :+: parens d0
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

ppBinOp :: BinOp -> Doc -> Doc -> Doc
ppBinOp AddI d0 d1 = d0 :<>: char '+' :<>: d1
ppBinOp AddD d0 d1 = d0 :<>: char '+' :<>: d1
ppBinOp AddPtr d0 d1 = d0 :<>: char '+' :<>: d1
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
