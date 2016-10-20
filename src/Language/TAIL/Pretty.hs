module Language.TAIL.Pretty where

import Language.TAIL.Syntax

import Text.PrettyPrint

angles :: Doc -> Doc
angles p = char '<' <> p <> char '>'

indent :: Doc -> Doc
indent = nest 4


pprScalar :: ScalarValue -> Doc
pprScalar (IntV v) = int v
pprScalar (BoolV True) = text "tt"
pprScalar (BoolV False) = text "ff"
pprScalar (DoubleV v) = double v
pprScalar (CharV v) = char v

pprBaseType :: BaseType -> Doc
pprBaseType IntT = text "int"
pprBaseType DoubleT = text "double"
pprBaseType BoolT = text "bool"
pprBaseType CharT = text "char"
pprBaseType (BaseVar v) = char 'b' <> int v

pprRank :: Rank -> Doc
pprRank (Rank i) = int i
pprRank (RankVar v) = char 'r' <> int v

pprType :: Type -> Doc
pprType (ArrayT bt r) = brackets (pprBaseType bt) <> pprRank r
pprType (VectorT bt r) = angles (pprBaseType bt) <> pprRank r
pprType (SingleT bt r) = text "S" <> parens (pprBaseType bt <> char ',' <> pprRank r)
pprType (SingleVecT bt r) = text "SV" <> parens (pprBaseType bt <> char ',' <> pprRank r)
pprType (t1 :> t2) = pprType t1 <+> text "->" <+> pprType t2
pprType (TyVarT m) = int m

fcall :: Doc -> [Exp ty] -> Doc
fcall n es = 
 let args = hsep (punctuate (char ',') (map pprExp es))
 in n <> (parens args)

pprExp :: Exp ty -> Doc
pprExp (Scalar v) = pprScalar v
pprExp (Var x _)  = text x
pprExp (Lam x ty e _) = text "fn" <+> text x <+> char ':' <+> pprType ty <+> text "=>" <+> pprExp e
pprExp (Vector es _)  = brackets (hsep (punctuate (char ',') (map pprExp es)))
pprExp (UnaryOp op e)      = fcall (text (show op)) [e]
pprExp (BinOp op e1 e2)    = fcall (pprBinOp op) [e1,e2]
pprExp (Each e1 e2 _)        = fcall (text "each") [e1,e2]
pprExp (ZipWith e1 e2 e3 _)  = fcall (text "zipWith") [e1,e2,e3]
pprExp (Catenate e1 e2 _) = fcall (text "catenate") [e1,e2]
pprExp (Let v e1 e2 _) = text "let" <+> text v <+> text "=" <+> pprExp e1
                       <+> text "in" $+$ indent (pprExp e2)
pprExp (Reduce e1 e2 e3 _)  = fcall (text "reduce") [e1,e2,e3]
pprExp (Infinity _) = text "inf"

pprBinOp :: BinOp -> Doc
pprBinOp AddI = text "addi"
pprBinOp SubI = text "subi"
pprBinOp MulI = text "muli"
pprBinOp DivI = text "divi"
pprBinOp LtI  = text "lti"
pprBinOp LteI = text "ltei"
pprBinOp GtI  = text "gti"
pprBinOp GteI = text "gtei"
pprBinOp EqI  = text "eqi"
pprBinOp NeqI = text "neqi"
pprBinOp MaxI = text "maxi"
pprBinOp MinI = text "mini"

pprBinOp AddD = text "addd"
pprBinOp SubD = text "subd"
pprBinOp MulD = text "muld"
pprBinOp DivD = text "divd"
pprBinOp LtD  = text "ltd"
pprBinOp LteD = text "lted"
pprBinOp GtD  = text "gtd"
pprBinOp GteD = text "gted"
pprBinOp EqD  = text "eqd"
pprBinOp NeqD = text "neqd"
pprBinOp MaxD = text "maxd"
pprBinOp MinD = text "mind"

pprBinOp LtC  = text "ltc"
pprBinOp LteC = text "ltec"
pprBinOp GtC  = text "gtc"
pprBinOp GteC = text "gtec"
pprBinOp EqC  = text "eqc"
pprBinOp NeqC = text "neqc"

pprBinOp PowD = text "powd"

pprBinOp And  = text "and"
pprBinOp Or   = text "or"
pprBinOp Xor  = text "xor"
pprBinOp Nor  = text "nor"
pprBinOp Nand = text "nand"

