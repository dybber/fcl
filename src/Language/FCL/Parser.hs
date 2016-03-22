{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Language.FCL.Parser
  (parseFile,
   parseString)
where

import Control.Applicative hiding ((<|>), many)
import Text.Parsec hiding (Empty)
import Text.Parsec.String

import Language.FCL.SourceRegion
import Language.FCL.Lexer
import Language.FCL.Syntax

parseString :: String -> String -> (Program Untyped)
parseString programText filename =
  case parse program filename programText of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO (Program Untyped)
parseFile filename =
  do contents <- readFile filename
     case parse program filename contents of
       Left e  -> error $ show e
       Right r -> return r

program :: Parser (Program Untyped)
program =
  do whitespace
     prog <- many1 definition
     eof
     return prog

-------------------------------------
-- Definitions and type signatures --
-------------------------------------
definition :: Parser (Definition Untyped)
definition = try (typesig >>= fundef)  -- fun.def. w. signature
          <|> fundef Nothing           -- fun.def.

typesig :: Parser (Maybe (String,Type))
typesig =
  do reserved "sig"
     ident <- identifier
     colon
     ty <- typeExpr
     return (Just (ident,ty))

fundef :: Maybe (String, Type) -> Parser (Definition Untyped)
fundef tyanno =
  let
    addArgs :: [Variable] -> Exp Untyped -> Exp Untyped
    addArgs [] rhs = rhs
    addArgs (x:xs) rhs = addArgs xs (Lamb x Untyped rhs Untyped Missing)
  in
    do make_kernel <- (reserved "fun" >> return False) <|> (reserved "kernel" >> return True)
       name <- identifier
       args <- many identifier
       symbol "="
       rhs <- expr
       let function = addArgs (reverse args) rhs
       return (Definition
                 { defVar = name
                 , defSignature = fmap snd tyanno
                 , defTypeScheme = TypeScheme [] Untyped
                 , defEmitKernel = make_kernel
                 , defBody = function
                 })
       
----------------------
--    Expressions    --
----------------------
expr :: Parser (Exp Untyped)
expr = chainl1 nonAppExpr (return App)

nonAppExpr :: Parser (Exp Untyped)
nonAppExpr = fnExpr
   <|> pairOrParens
   <|> configVariable
   <|> opExpr
   <|> arrayExpr
   <|> letExpr
   <|> ifThenElseExpr
   <|> valueExpr
 <?> "expression"

intExpr :: Parser (Region -> Exp Untyped)
intExpr = IntScalar . fromInteger <$> lexeme decimal

doubleExpr :: Parser (Region -> Exp Untyped)
doubleExpr = try (DoubleScalar <$> lexeme float)


boolExpr :: Parser (Region -> Exp Untyped)
boolExpr =
      try (reserved "true" >> return (BoolScalar True))
  <|> try (reserved "false" >> return (BoolScalar False))
  <?> "boolean value"

-- TODO: negation of doubles
negation :: Parser (Region -> Exp Untyped)
negation = do
  oneOf "~-"
  UnOp NegateI <$> valueExpr

valueExpr :: Parser (Exp Untyped)
valueExpr = withRegion (doubleExpr <|> intExpr <|> boolExpr <|> negation)

ifThenElseExpr :: Parser (Exp Untyped)
ifThenElseExpr =
  withRegion $ do
    reserved "if"
    e1 <- expr
    reserved "then"
    e2 <- expr
    reserved "else"
    e3 <- expr
    return (Cond e1 e2 e3 Untyped)

arrayExpr :: Parser (Exp Untyped)
arrayExpr = withRegion (Vec <$> (brackets (sepBy expr comma)) <*> pure Untyped)

configVariable :: Parser (Exp Untyped)
configVariable =
  do char '#'
     v <- identifier
     case v of
       "localSize" -> withRegion (return LocalSize)
       _ -> error ("Unknown configuration variable #" ++ v)

pairOrParens :: Parser (Exp Untyped)
pairOrParens =
  do p1 <- getPosition
     symbol "("
     t1 <- expr
     try (do comma
             t2 <- expr
             symbol ")"
             p2 <- getPosition
             return (Pair t1 t2 (newRegion p1 p2)))
      <|> do symbol ")"
             return t1

letExpr :: Parser (Exp Untyped)
letExpr =
  withRegion $ do
    reserved "let"
    ident <- identifier
    symbol "="
    e1 <- expr
    reserved "in"
    e2 <- expr
    return (Let ident e1 e2 Untyped)

opExpr :: Parser (Exp Untyped)
opExpr = withRegion (identifier >>= switch)
  where
    switch "i2d"      = UnOp I2D       <$> nonAppExpr
    switch "addi"     = BinOp AddI     <$> nonAppExpr <*> nonAppExpr
    switch "subi"     = BinOp SubI     <$> nonAppExpr <*> nonAppExpr
    switch "muli"     = BinOp MulI     <$> nonAppExpr <*> nonAppExpr
    switch "divi"     = BinOp DivI     <$> nonAppExpr <*> nonAppExpr
    switch "modi"     = BinOp ModI     <$> nonAppExpr <*> nonAppExpr
    switch "mini"     = BinOp MinI     <$> nonAppExpr <*> nonAppExpr
    switch "eqi"      = BinOp EqI      <$> nonAppExpr <*> nonAppExpr
    switch "neqi"     = BinOp NeqI     <$> nonAppExpr <*> nonAppExpr
    switch "powi"     = BinOp PowI     <$> nonAppExpr <*> nonAppExpr
    switch "shiftLi"  = BinOp ShiftLI  <$> nonAppExpr <*> nonAppExpr
    switch "shiftRi"  = BinOp ShiftRI  <$> nonAppExpr <*> nonAppExpr
    switch "andi"     = BinOp AndI     <$> nonAppExpr <*> nonAppExpr
    switch "xori"     = BinOp XorI     <$> nonAppExpr <*> nonAppExpr
    switch "powr"     = BinOp PowR     <$> nonAppExpr <*> nonAppExpr
    switch "divr"     = BinOp DivR     <$> nonAppExpr <*> nonAppExpr
    switch "fst"      = Proj1E         <$> nonAppExpr <?> "fst"
    switch "snd"      = Proj2E         <$> nonAppExpr <?> "snd"
    switch "index"    = Index          <$> nonAppExpr <*> nonAppExpr <?> "index"
    switch "length"   = Length         <$> nonAppExpr <?> "length"
    switch "generate" = Generate Block <$> nonAppExpr <*> nonAppExpr <?> "generate"
    switch "while"    = While          <$> nonAppExpr <*> nonAppExpr <*> nonAppExpr <?> "while"
    switch "map"      = Map            <$> nonAppExpr <*> nonAppExpr <?> "map"
    switch "force"    = ForceLocal     <$> nonAppExpr <?> "force"
    switch "concat"   = Concat         <$> nonAppExpr <*> nonAppExpr <?> "concat"
    switch n          = return (Var n Untyped)

fnExpr :: Parser (Exp Untyped)
fnExpr =
  withRegion $ do
    reserved "fn"
    ident <- identifier
    symbol "=>"
    e <- expr
    return (Lamb ident Untyped e Untyped)

------------------
--    Types
------------------
typeExpr :: Parser Type
typeExpr = chainr1 tyterm funType

tyterm :: Parser Type
tyterm = baseType
    <|> tyVar
    <|> productOrParensType
    <|> arrayType

productOrParensType :: Parser Type
productOrParensType =
  do symbol "("
     t1 <- typeExpr
     try (do comma
             t2 <- typeExpr
             symbol ")"
             return (t1 :*: t2))
      <|> do symbol ")"
             return t1

funType :: Parser (Type -> Type -> Type)
funType =
  do symbol "->"
     return (:>)

arrayType :: Parser Type
arrayType = ArrayT Block <$> brackets typeExpr

baseType :: Parser Type
baseType = (reserved "int" >> return IntT)
       <|> (reserved "double" >> return DoubleT)
       <|> (reserved "bool" >> return BoolT)
       <?> "base type"

newTyVar :: String -> Type
newTyVar name = VarT (TyVar 0 (Just name))

tyVar :: Parser Type
tyVar = newTyVar <$> identifier
       <?> "base type"
