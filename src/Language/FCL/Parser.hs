{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Language.FCL.Parser
  (parseTopLevel, ParseError)
where

import Control.Applicative hiding ((<|>), many)
import Text.Parsec hiding (Empty)
import Text.Parsec.String
import Text.Parsec.Expr

import Language.FCL.SourceRegion
import Language.FCL.Lexer
import Language.FCL.Syntax

----------------------
-- Exported parsers --
----------------------
parseTopLevel :: String -> String -> Either ParseError (Program Untyped)
parseTopLevel filename programText = parse topLevel filename programText

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
    addArgs :: [Name] -> Exp Untyped -> Exp Untyped
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
-- term :: Parser (Exp Untyped)
-- term =
--   do t <- term_no_anno
--      anno <- (type_annotation <|> return Nothing)
--      case anno of
--        Just ty -> withRegion (return (Annotation t ty))
--        Nothing -> return t

-- type_annotation :: Parser (Maybe Type)
-- type_annotation =
--   do colon
--      ty <- type'
--      return (Just ty)

term :: Parser (Exp Untyped)
term =
   try fn
   <|> tupleOrParens
   <|> if'
   <|> try bool
   <|> try floating
   <|> integer
   <|> try op
   <|> array
   <|> let'

sign :: Num a => Parser (a -> a)
sign = (oneOf "-~" >> return negate)
       <|> (char '+' >> return id)
       <|> return id

integer :: Parser (Exp Untyped)
integer =
  withRegion $ do
    n <- natural
    return (IntScalar (fromInteger n))

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
    switch "#localSize" = return LocalSize
    switch "#i2d"      = UnOp I2D      <$> term
    switch "#addi"     = BinOp AddI    <$> term <*> term
    switch "#subi"     = BinOp SubI    <$> term <*> term
    switch "#muli"     = BinOp MulI    <$> term <*> term
    switch "#divi"     = BinOp DivI    <$> term <*> term
    switch "#modi"     = BinOp ModI    <$> term <*> term
    switch "#mini"     = BinOp MinI    <$> term <*> term
    switch "#eqi"      = BinOp EqI     <$> term <*> term
    switch "#neqi"     = BinOp NeqI    <$> term <*> term
    switch "#powi"     = BinOp PowI    <$> term <*> term
    switch "#shiftLi"  = BinOp ShiftLI <$> term <*> term
    switch "#shiftRi"  = BinOp ShiftRI <$> term <*> term
    switch "#andi"     = BinOp AndI    <$> term <*> term
    switch "#xori"     = BinOp XorI    <$> term <*> term
    switch "#powr"     = BinOp PowR    <$> term <*> term
    switch "#divr"     = BinOp DivR    <$> term <*> term
    switch "#fst"      = Proj1E        <$> term <?> "fst"
    switch "#snd"      = Proj2E        <$> term <?> "snd"
    switch "#index"    = Index         <$> term <*> term <?> "index"
    switch "#lengthPull"   = LengthPull    <$> term <?> "lengthPull"
    switch "#lengthPush"   = LengthPush    <$> term <?> "lengthPush"
    switch "#generatePull" = GeneratePull <$> term <*> term <?> "generatePull"
    switch "#while"    = While         <$> term <*> term <*> term <?> "while"
    switch "#whileSeq" = WhileSeq      <$> term <*> term <*> term <?> "whileSeq"
    switch "#push"     = Push          <$> angles level <*> term <*> (return Untyped) <?> "push"
    switch "#mapPull"  = MapPull       <$> term <*> term <?> "mapPull"
    switch "#mapPush"  = MapPush       <$> term <*> term <?> "mapPush"
    switch "#force"    = Force         <$> term <?> "force"
    switch "#concat"   = Concat        <$> term <*> term <?> "concat"
    switch "#assemble" = Assemble      <$> term <*> term <*> term <?> "assemble"
    switch n          = return (Var n Untyped)

-- push =
--   do angles level

fn :: Parser (Exp Untyped)
fn =
  withRegion $ do
    reserved "fn"
    ident <- identifier
    symbol "=>"
    e <- expr
    return (Lamb ident Untyped e Untyped)


expr :: Parser (Exp Untyped)
expr =
  let table = [ [Infix (return App) AssocLeft],
                [Infix (binOp "+" AddI) AssocLeft],
                [Infix pipeForward AssocLeft]
              ]
  in buildExpressionParser table term

pipeForward :: Parser (Exp Untyped -> Exp Untyped -> Exp Untyped)
pipeForward = do
  reservedOp "|>"
  return (\e1 e2 -> App e2 e1)

binOp :: String -> BinOp -> Parser (Exp Untyped -> Exp Untyped -> Exp Untyped)
binOp opName operator = do
  reservedOp opName
  return (\e1 e2 -> BinOp operator e1 e2 Missing)

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
  <|> pullArrayType
  <|> pushArrayType


baseType :: Parser Type
baseType = (reserved "int" >> return IntT)
       <|> (reserved "double" >> return DoubleT)
       <|> (reserved "bool" >> return BoolT)

level :: Parser Level
level = (reserved "thread" >> return threadLevel)
    <|> (reserved "warp" >> return warpLevel)
    <|> (reserved "block" >> return blockLevel)
    <|> (reserved "grid" >> return gridLevel)
    <|> lvlVar
    <|> parens
          (do char '1'
              reservedOp "+"
              lvl <- level
              return (Step lvl))

pullArrayType :: Parser Type
pullArrayType = PullArrayT <$> brackets type'

pushArrayType :: Parser Type
pushArrayType = do
 ty <- angles type'
 lvl <- level
 return (PushArrayT lvl ty)

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

lvlVar :: Parser Level
lvlVar = do
  name <- identifier
  return (VarL (LvlVar 0 (Just name)))
