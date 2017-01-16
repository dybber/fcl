module FCL.Desugaring (desugarDefinition, desugarExp) where

import FCL.Core.Identifier
import FCL.Core.SourceRegion

import FCL.Type.Polymorphic as E
import qualified FCL.External.Syntax as E
import qualified FCL.Core.Syntax as C

desugarDefinition :: E.Definition E.Type -> C.Definition C.Type
desugarDefinition (E.Definition ident signature tysc ebody) =
  C.Definition
    ident
    (desugarType <$> signature)
    (desugarTypeScheme tysc)
    (desugarExp ebody)

desugarExp :: E.Exp E.Type -> C.Exp C.Type
desugarExp e =
  case e of
    E.Literal l reg                     -> C.Literal (desugarLiteral l) reg
    E.Symbol ident ty reg               -> desugarSymbol ident ty reg
    E.Vec es ty reg                     -> C.Vec (map desugarExp es) (desugarType ty) reg
    E.Lamb ident tyarg ebody tybody reg -> C.Lamb ident (desugarType tyarg) (desugarExp ebody) (desugarType tybody) reg
    E.App e1 e2 _                       -> C.App (desugarExp e1) (desugarExp e2) -- TODO add region info
    E.LambLvl lvlVar e1 ty reg          -> C.LambLvl (desugarLvlVar lvlVar) (desugarExp e1) (desugarType ty) reg
    E.AppLvl e1 lvl _                   -> C.AppLvl (desugarExp e1) (desugarLvl lvl) -- TODO add region info
    E.Let ident ebinding ebody ty reg   -> C.Let ident (desugarExp ebinding) (desugarExp ebody) (desugarType ty) reg
    E.Cond e1 e2 e3 ty reg              -> C.Cond (desugarExp e1) (desugarExp e2) (desugarExp e3) (desugarType ty) reg
    E.Pair e1 e2 reg                    -> C.Pair (desugarExp e1) (desugarExp e2) reg
    E.UnaryOp op e1 reg                 -> C.UnaryOp (desugarUnaryOp op) (desugarExp e1) reg
    E.BinaryOp op e1 e2 reg             -> C.BinaryOp (desugarBinaryOp op) (desugarExp e1) (desugarExp e2) reg

desugarLiteral :: E.Literal -> C.Literal
desugarLiteral (E.LiteralInt i) = C.LiteralInt i
desugarLiteral (E.LiteralDouble d) = C.LiteralDouble d
desugarLiteral (E.LiteralBool b) = C.LiteralBool b
desugarLiteral (E.LiteralString s) = C.LiteralString s
desugarLiteral (E.Unit) = C.Unit

desugarLvlVar :: E.LvlVar -> C.LvlVar
desugarLvlVar (E.LvlVar i n) = C.LvlVar i n

desugarLvl :: E.Level -> C.Level
desugarLvl E.Zero = C.Zero
desugarLvl (E.Step lvl) = C.Step (desugarLvl lvl)
desugarLvl (E.VarL lvlVar) = C.VarL (desugarLvlVar lvlVar)

desugarTyVar :: E.TyVar -> C.TyVar
desugarTyVar (E.TyVar i n) = C.TyVar i n

desugarType :: E.Type -> C.Type
desugarType E.IntT = C.IntT
desugarType E.DoubleT = C.DoubleT
desugarType E.BoolT = C.BoolT
desugarType E.StringT = C.StringT
desugarType E.UnitT = C.UnitT
desugarType (E.VarT tv) = C.VarT (desugarTyVar tv)
desugarType (lvlVar E.:-> t0) = (desugarLvlVar lvlVar) C.:-> (desugarType t0)
desugarType (t0 E.:> t1) = (desugarType t0) C.:> (desugarType t1)
desugarType (t0 E.:*: t1) = (desugarType t0) C.:*: (desugarType t1)
desugarType (E.PullArrayT t0) = C.PullArrayT (desugarType t0)
desugarType (E.PushArrayT lvl t0) = C.PushArrayT (desugarLvl lvl) (desugarType t0)
desugarType (E.ProgramT lvl t0) = C.ProgramT (desugarLvl lvl) (desugarType t0)

desugarTypeScheme :: E.TypeScheme E.Type -> C.TypeScheme C.Type
desugarTypeScheme (E.TypeScheme tyvars _ ty) = C.TypeScheme (map desugarTyVar tyvars) (desugarType ty)

desugarUnaryOp :: E.UnaryOperator -> C.UnaryOperator
desugarUnaryOp E.AbsI = C.AbsI
desugarUnaryOp E.SignI = C.AbsI
desugarUnaryOp E.NegateI = C.NegateI
desugarUnaryOp E.Not = C.Not
desugarUnaryOp E.B2I = C.B2I
desugarUnaryOp E.CLZ = C.CLZ

desugarBinaryOp :: E.BinaryOperator -> C.BinaryOperator
desugarBinaryOp E.AddI = C.AddI
desugarBinaryOp E.SubI = C.SubI
desugarBinaryOp E.MulI = C.MulI
desugarBinaryOp E.DivI = C.DivI
desugarBinaryOp E.ModI = C.ModI
desugarBinaryOp E.EqI = C.EqI
desugarBinaryOp E.NeqI = C.NeqI
desugarBinaryOp E.AndI = C.AndI
desugarBinaryOp E.OrI = C.OrI
desugarBinaryOp E.XorI = C.XorI
desugarBinaryOp E.ShiftLI = C.ShiftLI
desugarBinaryOp E.ShiftRI = C.ShiftRI
desugarBinaryOp E.PowI = C.PowI
desugarBinaryOp E.DivR = C.DivR
desugarBinaryOp E.AddR = C.AddR
desugarBinaryOp E.PowR = C.PowR

builtins :: [String]
builtins = ["fst", "snd", "index", "lengthPull", "lengthPush", "generate",
            "mapPull", "mapPush", "force", "push", "return", "bind",
            "interleave", "seqfor", "power", "while", "#BlockSize",
            "readIntCSV", "forceAndPrint", "benchmark",
            "mini", "maxi", "i2d","divi","subi","addr", "lti"]

desugarBuiltin :: Identifier -> C.Type -> SourceRegion -> C.Exp C.Type
desugarBuiltin (Identifier ident) ty reg =
 case ident of
   "#BlockSize"  -> C.BlockSize reg
   "fst"        -> unop ident ty C.Proj1E reg
   "snd"        -> unop ident ty C.Proj2E reg
   "lengthPull" -> unop ident ty C.LengthPull reg
   "lengthPush" -> unop ident ty C.LengthPush reg
   "force"      -> unop ident ty C.Force reg

   "i2d"        -> unop ident ty (C.UnaryOp C.I2D) reg

   "addr"       -> binop ty (C.BinaryOp C.AddR) reg
   "lti"        -> binop ty (C.BinaryOp C.LtI) reg
   "maxi"       -> binop ty (C.BinaryOp C.MaxI) reg
   "mini"       -> binop ty (C.BinaryOp C.MinI) reg
   "subi"       -> binop ty (C.BinaryOp C.SubI) reg
   "divi"       -> binop ty (C.BinaryOp C.DivI) reg
   "index"      -> binop ty C.Index reg
   "generate"   -> binop ty C.GeneratePull reg
   "mapPull"    -> binop ty C.MapPull reg
   "mapPush"    -> binop ty C.MapPush reg
   "bind"       -> binop ty C.Bind reg
   "forceAndPrint" -> binop ty C.ForceAndPrint reg
   "benchmark" -> binop ty C.Benchmark reg

   "seqfor"     -> triop ty C.For reg
   "power"      -> triop ty C.Power reg
   "while"      -> triop ty C.While reg
   "interleave" -> triop ty C.Interleave reg

   "push"       ->
     case ty of
       (lvlVar C.:-> ty') -> C.LambLvl lvlVar (unop ident ty' (C.Push (C.VarL lvlVar)) reg) ty reg
       
   "return"       ->
     case ty of
       (lvlVar C.:-> ty') -> C.LambLvl lvlVar (unop ident ty' (C.Return (C.VarL lvlVar)) reg) ty reg
   opName -> error ("Desugaring of builtin " ++ opName ++ " failed.")
   
desugarSymbol :: Identifier -> E.Type -> SourceRegion -> C.Exp C.Type
desugarSymbol ident ty reg =
  if (identToString ident) `elem` builtins
  then desugarBuiltin ident (desugarType ty) reg
  else C.Var ident (desugarType ty) reg

missing :: SourceRegion
missing = newRegion Missing Missing

-- Wrap unary function, to allow partial application
unop :: String -> C.Type -> (C.Exp C.Type -> SourceRegion -> C.Exp C.Type) -> (SourceRegion -> C.Exp C.Type)
unop _ (ty0 C.:> ty1) opr r =
      C.Lamb (Identifier "x") ty0
           (opr (C.Var (Identifier "x") ty0 missing) r)
           ty1 r
unop ident _ _ _ = error ("Desugar: found unary operator " ++ ident ++ " with wrong type inferred.")

-- Wrap binary function, to allow partial application
binop :: C.Type -> (C.Exp C.Type -> C.Exp C.Type -> SourceRegion -> C.Exp C.Type) ->  (SourceRegion -> C.Exp C.Type)
binop (ty0 C.:> (ty1 C.:> ty2)) opr r =
      C.Lamb (Identifier "x") ty0
          (C.Lamb (Identifier "y") ty1
              (opr (C.Var (Identifier "x") ty0 missing)
                   (C.Var (Identifier "y") ty1 missing) r)
              ty2 r)
          ty2 r
binop _ _ _ = error "Desugar: found binary operator with wrong type inferred"

 -- Wrap ternary function, to allow partial application
triop :: C.Type -> (C.Exp C.Type -> C.Exp C.Type -> C.Exp C.Type -> SourceRegion -> C.Exp C.Type) -> (SourceRegion -> C.Exp C.Type)
triop (ty0 C.:> (ty1 C.:> (ty2 C.:> ty3))) opr r =
      C.Lamb (Identifier "x") ty0
          (C.Lamb (Identifier "y") ty1
              (C.Lamb (Identifier "z") ty2
                  (opr (C.Var (Identifier "x") ty0 missing)
                       (C.Var (Identifier "y") ty1 missing)
                       (C.Var (Identifier "z") ty2 missing) r)
                  ty3 r)
              ty3 r)
          ty3 r
triop _ _ _ = error "Desugar: found ternary operator with wrong type inferred"
