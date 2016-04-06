module Language.FCL.PrettyPrint where

import Language.GPUIL.PrettyLib
import Language.FCL.Syntax


ppType :: Type -> Doc
ppType IntT = text "int"
ppType BoolT = text "bool"
ppType DoubleT = text "double"
ppType (VarT (TyVar i Nothing)) = text "tv" :+: int i
ppType (VarT (TyVar _ (Just v))) = text v
ppType (ty :> ty')  = parens (ppType ty :<>: text "->" :<>: ppType ty')
ppType (ty :*: ty') = parens (ppType ty :+: char ',' :<>: ppType ty')
ppType (ArrayT _ ty) = brackets (ppType ty)

prettyPrintType :: Type -> String
prettyPrintType = render 0 4 . ppType

showType :: Definition Type -> String
showType (Definition v _ _ _ e) = v ++ " : " ++ prettyPrintType (typeOf e)
