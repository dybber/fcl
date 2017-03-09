{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module FCL.External.Parser (parseProgram, parseExpr, ParseError) where

import Data.Functor.Identity (Identity)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Text.Parsec hiding (Empty)
import Text.Parsec.Expr
import Text.Parsec.String (Parser)

import FCL.External.Syntax
import FCL.External.Lexer

type Op a = Operator String () Identity a

parseProgram :: String -> String -> Either ParseError Program
parseProgram filename programText = parse program filename programText

parseExpr :: String -> String -> Either ParseError Exp
parseExpr filename programText = parse (whitespace >> expr) filename programText

--------------
-- Programs --
--------------
program :: Parser Program
program =
  do whitespace
     prog <- many1 declaration
     eof
     return (Program prog)

declaration :: Parser FunctionDefinition
declaration = do
  sig <- try (Just <$> typesignature) <|> return Nothing
  fun sig <|> val sig

typesignature :: Parser TypeScheme
typesignature =
  do reserved "sig"
     _ <- identifier -- TODO check that the name matches the following definition
     colon
     tysc <- typescheme
     return tysc

fun :: Maybe TypeScheme -> Parser FunctionDefinition
fun sig =
  do reserved "fun"
     ident <- identifier
     lvls <- lvlVars
     params <- many1 identifier
     reservedOp "="
     e <- expr
     return (FunctionDefinition
               { funName = ident
               , funSignature = sig
               , funQuantifiedLevelVariables = lvls
               , funParameters = params
               , funBody = e
               })

val :: Maybe TypeScheme -> Parser FunctionDefinition
val sig =
  do reserved "val"
     ident <- identifier
     lvls <- lvlVars
     reservedOp "="
     e <- expr
     return (FunctionDefinition
               { funName = ident
               , funSignature = sig
               , funQuantifiedLevelVariables = lvls
               , funParameters = []
               , funBody = e
               })

---------------------
--   Expressions   --
---------------------
term :: Parser Exp
term =
   try fn
   <|> unitTupleOrParens
   <|> bool
   <|> try floating
   <|> try integer
   <|> try stringLiteral
   <|> try var
   <|> if'
   <|> let'
   <|> do'

var :: Parser Exp
var =
  do ident <- identifier
     lvls <- try (angles (level `sepBy` comma))
               <|> return []
     return (Symbol ident lvls)

sign :: Num a => Parser (a -> a)
sign = (oneOf "-" >> return negate)
       <|> return id

integer :: Parser Exp
integer =
  do f <- sign
     n <- natural
     return (Literal (LiteralInt (fromInteger (f n))))

floating :: Parser Exp
floating =
  do f <- sign
     n <- float
     return (Literal (LiteralDouble (f n)))

bool :: Parser Exp
bool =
    (reserved "true" >> return (Literal (LiteralBool True)))
    <|> (reserved "false" >> return (Literal (LiteralBool False)))

stringLiteral :: Parser Exp
stringLiteral =
  do str <- stringlit
     return (Literal (LiteralString str))

unitTupleOrParens :: Parser Exp
unitTupleOrParens =
  do --p1 <- getPosition
     symbol "("
     try (symbol ")" >> return Unit)
       <|>
        (do t1 <- expr
            try (do comma
                    t2 <- expr
                    symbol ")"
                    --p2 <- getPosition
                    return (Pair t1 t2 -- (regionFromPos p1 p2)
                           ))
             <|> do symbol ")"
                    return t1)

if' :: Parser Exp
if' =
 do reserved "if"
    e1 <- expr
    reserved "then"
    e2 <- expr
    reserved "else"
    e3 <- expr
    return (Cond e1 e2 e3)

let' :: Parser Exp
let' =
 do reserved "let"
    ident <- identifier
    lvls <- lvlVars
    annotation <- try (colon >> (Just <$> typescheme)) <|> return Nothing
    symbol "="
    e1 <- expr
    reserved "in"
    e2 <- expr
    return (Let ident annotation lvls e1 e2)

do' :: Parser Exp
do' =
  let 
   dobody :: Parser DoStmt
   dobody =
     try dobind <|> doexp

   doexp :: Parser DoStmt
   doexp = DoExp <$> expr

   dobind :: Parser DoStmt
   dobind =
       do v <- identifier
          reservedOp "<-"
          e <- expr
          return (DoBind v e)
  in do reserved "do"
        lvl <- angles level
        stmts <- braces (dobody `sepBy` (reservedOp ";"))
        return (Do lvl stmts)

-- Function argument
argument :: Parser (Exp -> Exp)
argument =
  do ident <- identifier
     return (\e -> Lamb ident e)

-- Zero or more function arguments
arguments :: Parser (Exp -> Exp)
arguments = 
    try (do a <- argument
            more <- arguments
            return (\e -> a (more e)))
    <|> (return (\e -> e))

-- One or more function arguments
arguments1 :: Parser (Exp -> Exp)
arguments1 =
  do a <- argument
     as <- arguments
     return (\e -> a (as e))

fn :: Parser Exp
fn =
  do reserved "fn"
     f <- arguments1
     symbol "=>"
     e <- expr
     return (f e)

expr :: Parser Exp
expr =
  let
    application :: Op Exp
    application =
      Infix (return (\e1 e2 -> App e1 e2))
            AssocLeft

    pipeForward :: Op Exp
    pipeForward =
      Infix (do reservedOp "|>"
                return (\e1 e2 -> App e2 e1))
                        -- let start = beginPosition (getSourceRegion e1)
                        --     end = endPosition (getSourceRegion e2)
                        -- in App e2 e1 (newRegion start end)))
            AssocLeft

    binOp :: String -> BinaryOperator -> Parser (Exp -> Exp -> Exp)
    binOp opName operator =
      do
--         pos1 <- getPosition
         reservedOp opName
--         pos2 <- getPosition
--         let reg = regionFromPos pos1 pos2
         return (\e1 e2 -> BinaryOp operator e1 e2)

    table = [ [application],
              [Infix (binOp "*" MulI) AssocLeft, Infix (binOp "/" DivI) AssocLeft, Infix (binOp "%" ModI) AssocLeft],
              [Infix (binOp "+" AddI) AssocLeft, Infix (binOp "-" SubI) AssocLeft],
              [Infix (binOp "<<" ShiftLI) AssocLeft,
               Infix (binOp ">>" ShiftRI) AssocLeft,
               Infix (binOp "&" AndI) AssocLeft-- ,
               -- Infix (binOp "|" OrI) AssocLeft
              ],
              [Infix (binOp "==" EqI) AssocLeft, Infix (binOp "!=" NeqI) AssocLeft],
              [pipeForward]
            ]
  in buildExpressionParser table term

------------
-- Levels --
------------
level :: Parser Level
level = (do char 'Z'
            return Zero)
    <|> (reserved "thread" >> return Zero)
    <|> (reserved "block" >> return (Step Zero))
    <|> (reserved "grid" >> return (Step (Step Zero)))
    <|> (do char '1'
            reservedOp "+"
            lvl <- level
            return (Step lvl))
    <|> (VarL <$> lvlVar)

lvlVar :: Parser LvlVar
lvlVar = LvlVar 0 . Just <$> identifier

lvlVars :: Parser [LvlVar]
lvlVars =
  try (angles (lvlVar `sepBy` comma))
  <|> return []

-----------
-- Types --
-----------
typescheme :: Parser TypeScheme
typescheme =
  try (do reservedOp "forall"
          lvlvars <- lvlVars
          tyvars <- many tyVar
          char '.'
          whitespace
          t <- type'
          return (TypeScheme lvlvars tyvars t))
   <|> (TypeScheme [] [] <$> type')

type' :: Parser Type
type' =
 let scan =
       do x <- simpleType
          rest x
     rest x =
       (do symbol "->"
           y <- scan
           return (x :> y))
       <|> return x
 in scan

simpleType :: Parser Type
simpleType =
  baseType
  <|> programType
  <|> (VarT <$> tyVar)
  <|> tupleType
  <|> arrayType

baseType :: Parser Type
baseType = (reserved "int" >> return IntT)
       <|> (reserved "double" >> return DoubleT)
       <|> (reserved "bool" >> return BoolT)
       <|> (reserved "string" >> return StringT)
       <|> (reserved "unit" >> return UnitT)

programType :: Parser Type
programType =
 do reserved "Program"
    lvl <- angles level
    ty <- simpleType
    return (ProgramT lvl ty)

arrayType :: Parser Type
arrayType =
  do ty <- brackets type'
     (try (do lvl <- angles level
              return (PushArrayT lvl ty)))
       <|> return (PullArrayT ty)

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

tyVar :: Parser TyVar
tyVar =
  char '\'' >> identifier >>= return . TyVar 0 . Just
