{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module FCL.IL.Parser
  (parseProgram, parseExpr, ParseError)
where

import Data.Functor.Identity (Identity)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Text.Parsec hiding (Empty)
import Text.Parsec.Expr
import Text.Parsec.String (Parser)

import FCL.IL.Syntax
import FCL.IL.Lexer

type Op a = Operator String () Identity a

parseProgram :: String -> String -> Either ParseError (ILProgram ())
parseProgram filename programText = parse (whitespace >> many stmt) filename programText

parseExpr :: String -> String -> Either ParseError ILExp
parseExpr filename programText = parse (whitespace >> expr) filename programText

variable :: Parser ILName
variable =
  do i <- identifier
     return (ILName i 0)

typ :: Parser ILType
typ =
      (reserved "int" >> return ILInt)
  <|> (reserved "double" >> return ILDouble)
  <|> (reserved "bool" >> return ILBool)
  <|> (reserved "string" >> return ILString)
  <|> (ILArray <$> brackets typ)
           
level :: Parser ILLevel
level =
      (reserved "thread" >> return Thread)
  <|> (reserved "block" >> return Block)
  <|> (reserved "grid" >> return Grid)

sign :: Num a => Parser (a -> a)
sign = (oneOf "-" >> return negate)
       <|> return id

integer :: Parser ILExp
integer =
  do f <- sign
     n <- natural
     return (EInt (fromInteger (f n)))

floating :: Parser ILExp
floating =
  do f <- sign
     n <- float
     return (EDouble (f n))

bool :: Parser ILExp
bool =
    (reserved "true" >> return (EBool True))
    <|> (reserved "false" >> return (EBool False))

stringLiteral :: Parser ILExp
stringLiteral =
  do str <- stringlit
     return (EString str)

indexExp :: Parser ILExp
indexExp =
  do arr <- variable
     idx <- brackets expr
     return (EIndex arr idx)

ifExp :: Parser ILExp
ifExp =
  do reserved "if"
     e1 <- expr
     reserved "then"
     e2 <- expr
     reserved "else"
     e3 <- expr
     return (EIf e1 e2 e3)

unop :: String -> UnaryOp -> Parser ILExp
unop name op =
  do symbol name
     e1 <- parens expr
     return (EUnaryOp op e1)

binop :: String -> BinOp -> Parser ILExp
binop name op =
  do reserved name
     (e1, e2) <- tuple expr expr
     return (EBinOp op e1 e2)
  
operator :: Parser ILExp
operator =
  choice [ binop "addi" AddI
         , binop "subi" SubI
         , binop "muli" MulI
         , binop "divi" DivI
         , binop "modi" ModI
         , binop "mini" MinI
         , binop "lti" LtI
         , binop "eqi" EqI]

term :: Parser ILExp
term =
  parens expr
  <|> try operator
  <|> try indexExp
  <|> try (EVar <$> variable)
  <|> try integer
  <|> floating
  <|> bool
  <|> stringLiteral
  <|> ifExp

  -- <|> operator


expr :: Parser ILExp
expr =
  let
    binOp :: String -> BinOp -> Parser (ILExp -> ILExp -> ILExp)
    binOp opName op =
      do reservedOp opName
         return (\e1 e2 -> EBinOp op e1 e2)

    table = [ [Infix (binOp "*" MulI) AssocLeft, Infix (binOp "/" DivI) AssocLeft, Infix (binOp "%" ModI) AssocLeft],
              [Infix (binOp "+" AddI) AssocLeft, Infix (binOp "-" SubI) AssocLeft],
              [Infix (binOp "<<" Sll) AssocLeft,
               Infix (binOp ">>" Srl) AssocLeft,
               Infix (binOp "&" Land) AssocLeft,
               Infix (binOp "|" Lor) AssocLeft
              ],
              [Infix (binOp "==" EqI) AssocLeft, Infix (binOp "!=" NeqI) AssocLeft]
            ]
  in buildExpressionParser table term

declare :: Parser (Stmt ())
declare =
  do ty <- typ
     x <- variable
     reservedOp "="
     e <- expr
     reservedOp ";"
     return (Declare x ty e ())

tuple :: Parser a -> Parser b -> Parser (a,b)
tuple p1 p2 =
  parens (do v1 <- p1
             comma
             v2 <- p2
             return (v1, v2))

allocate :: Parser (Stmt ())
allocate =
  do x <- variable
     reservedOp "="
     reserved "allocate"
     (ty, size) <- tuple typ expr
     reservedOp ";"
     return (Alloc x ty size ())

synchronize :: Parser (Stmt ())
synchronize =
  do reserved "synchronize"
     reservedOp ";"
     return (Synchronize ())

assign :: Parser (Stmt ())
assign =
  do x <- variable
     reservedOp "="
     e <- expr
     reservedOp ";"
     return (Assign x e ())

assignSub :: Parser (Stmt ())
assignSub =
  do x <- variable
     ix <- brackets expr
     reservedOp "="
     e <- expr
     reservedOp ";"
     return (AssignSub x ix e ())

readIntCSV :: Parser (Stmt ())
readIntCSV =
  do (x,xlen) <- tuple variable variable
     reservedOp "="
     reserved "readIntCSV"
     e <- parens expr
     reservedOp ";"
     return (ReadIntCSV x xlen e ())

printIntArray :: Parser (Stmt ())
printIntArray =
  do reserved "printIntArray"
     (e1, e2) <- tuple expr expr
     reservedOp ";"
     return (PrintIntArray e1 e2 ())

distribute :: Parser (Stmt ())
distribute =
  do reserved "distribute"
     lvl <- angles level
     (x, bound) <- parens (do x <- variable
                              reserved "<"
                              bound <- expr
                              return (x, bound))
     body <- stmts
     return (Distribute lvl x bound body ())

parfor :: Parser (Stmt ())
parfor =
  do reserved "parfor"
     lvl <- angles level
     (x, bound) <- parens (do x <- variable
                              reserved "<"
                              bound <- expr
                              return (x, bound))
     body <- stmts
     return (ParFor lvl x bound body ())

while :: Parser (Stmt ())
while =
  do reserved "while"
     cond <- parens expr
     body <- stmts
     return (While cond body ())

ifStmt :: Parser (Stmt ())
ifStmt =
  do reserved "if"
     cond <- parens expr
     body1 <- stmts
     body2 <- (try (reserved "else" >> stmts)) <|> return []
     return (If cond body1 body2 ())

seqfor :: Parser (Stmt ())
seqfor =
  do reserved "seqfor"
     (x, bound) <- parens (do x <- variable
                              reserved "<"
                              bound <- expr
                              return (x, bound))
     body <- stmts
     return (SeqFor x bound body ())

benchmark :: Parser (Stmt ())
benchmark =
  do reserved "benchmark"
     iterations <- parens expr
     body <- stmts
     return (Benchmark iterations body ())

stmts :: Parser [Stmt ()]
stmts =
  braces (many stmt)
   <|> ((:[]) <$> stmt )
  
stmt :: Parser (Stmt ())
stmt =
      try allocate
  <|> declare
  <|> try assign
  <|> try assignSub
  <|> synchronize
  <|> readIntCSV
  <|> printIntArray
  <|> distribute
  <|> parfor
  <|> ifStmt
  <|> while
  <|> seqfor
  <|> benchmark
