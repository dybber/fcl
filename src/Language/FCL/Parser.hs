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

----------------------
-- Exported parsers --
----------------------
parseString :: String -> String -> (Program Untyped)
parseString programText filename =
  case parse topLevel filename programText of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO (Program Untyped)
parseFile filename =
  do contents <- readFile filename
     case parse topLevel filename contents of
       Left e  -> error $ show e
       Right r -> return r

---------------------------
-- Top-level definitions --
---------------------------
topLevel :: Parser (Program Untyped)
topLevel =
  do whitespace
     prog <- many1 definition
     eof
     return prog

definition :: Parser (Definition Untyped)
definition = try (typesig >>= fundef)  -- fun.def. w. signature
          <|> fundef Nothing           -- fun.def.

typesig :: Parser (Maybe (String,Type))
typesig =
  do reserved "sig"
     ident <- identifier
     colon
     ty <- type'
     return (Just (ident,ty))

fundef :: Maybe (String, Type) -> Parser (Definition Untyped)
fundef tyanno =
  let
    addArgs :: [Variable] -> Exp Untyped -> Exp Untyped
    addArgs [] rhs = rhs
    addArgs (x:xs) rhs = addArgs xs (Lamb x Untyped rhs Untyped Missing)
  in
    do make_kernel <-     (reserved "fun" >> return False)
                      <|> (reserved "kernel" >> return True)
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
       
---------------------
--   Expressions   --
---------------------
expr :: Parser (Exp Untyped)
expr = chainl1 nonAppExpr (return App)

nonAppExpr :: Parser (Exp Untyped)
nonAppExpr =
   try fn
   <|> let'
   <|> if'
   <|> configVariable
   <|> try bool
   <|> try floating
   <|> integer
   <|> op
   <|> array
   <|> tupleOrParens

sign :: Num a => Parser (a -> a)
sign = (oneOf "-~" >> return negate)
       <|> (char '+' >> return id)
       <|> return id

integer :: Parser (Exp Untyped)
integer =
  withRegion $ do
    f <- sign
    n <- decimal
    return (IntScalar (fromInteger (f n)))

floating :: Parser (Exp Untyped)
floating =
  withRegion $ do
    f <- sign
    n <- float
    return (DoubleScalar (f n))

bool :: Parser (Exp Untyped)
bool =
  withRegion $
    (reserved "true" >> return (BoolScalar True))
    <|> (reserved "false" >> return (BoolScalar False))

if' :: Parser (Exp Untyped)
if' =
  withRegion $ do
    reserved "if"
    e1 <- expr
    reserved "then"
    e2 <- expr
    reserved "else"
    e3 <- expr
    return (Cond e1 e2 e3 Untyped)

array :: Parser (Exp Untyped)
array = withRegion $ do
  elems <- brackets (sepBy expr comma)
  return (Vec elems Untyped)

configVariable :: Parser (Exp Untyped)
configVariable =
  do char '#'
     v <- identifier
     case v of
       "localSize" -> withRegion (return LocalSize)
       _ -> error ("Unknown configuration variable #" ++ v)

tupleOrParens :: Parser (Exp Untyped)
tupleOrParens =
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

let' :: Parser (Exp Untyped)
let' =
  withRegion $ do
    reserved "let"
    ident <- identifier
    symbol "="
    e1 <- expr
    reserved "in"
    e2 <- expr
    return (Let ident e1 e2 Untyped)

op :: Parser (Exp Untyped)
op = withRegion (identifier >>= switch)
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

fn :: Parser (Exp Untyped)
fn =
  withRegion $ do
    reserved "fn"
    ident <- identifier
    symbol "=>"
    e <- expr
    return (Lamb ident Untyped e Untyped)

------------------
--    Types
------------------
type' :: Parser Type
type' = chainr1 simpleType funType

simpleType :: Parser Type
simpleType =
  baseType
  <|> tyVar
  <|> tupleType
  <|> arrayType


baseType :: Parser Type
baseType = (reserved "int" >> return IntT)
       <|> (reserved "double" >> return DoubleT)
       <|> (reserved "bool" >> return BoolT)

arrayType :: Parser Type
arrayType = ArrayT Block <$> brackets type'

tupleType :: Parser Type
tupleType =
  do symbol "("
     t1 <- type'
     try (do comma
             t2 <- type'
             symbol ")"
             return (t1 :*: t2))
      <|> do symbol ")"
             return t1

funType :: Parser (Type -> Type -> Type)
funType =
  do symbol "->"
     return (:>)

tyVar :: Parser Type
tyVar = do
  name <- identifier
  return (VarT (TyVar 0 (Just name)))
