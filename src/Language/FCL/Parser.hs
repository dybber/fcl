{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- | Parse FCL programs. After parsing, all type variables and level
-- variables needs to be numbered. This is not done here to seperate
-- concerns.
module Language.FCL.Parser
  (parseTopLevel, parseType, ParseError)
where

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

parseType :: String -> String -> Either ParseError Type
parseType filename input = parse type' filename input

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

parseConfig :: KernelConfig -> String -> Parser KernelConfig
parseConfig cfg "#BlockSize" =
  do i <- natural
     return (cfg { configBlockSize = Just (fromInteger i) })
parseConfig cfg "#WarpSize" =
  do i <- natural
     return (cfg { configWarpSize = fromInteger i})
parseConfig _ str = error ("Unsupported kernel configuration option: " ++ str)

kernelConfig :: KernelConfig -> Parser KernelConfig
kernelConfig cfg =
  do reserved "config"
     ident <- identifier
     reservedOp "="
     cfg' <- parseConfig cfg ident
     kernelConfig cfg'
  <|> return cfg

fundef :: Maybe (String, Type) -> Parser (Definition Untyped)
fundef tyanno =
    do make_kernel <-     (reserved "fun" >> return False)
                      <|> (reserved "kernel" >> return True)
       name <- identifier
       args <- arguments
       reservedOp "="
       rhs <- expr
       conf <- kernelConfig defaultKernelConfig
       let function = args rhs Missing
       return (Definition
                 { defVar = name
                 , defSignature = fmap snd tyanno
                 , defTypeScheme = TypeScheme [] Untyped
                 , defEmitKernel = make_kernel
                 , defKernelConfig = conf
                 , defBody = function
                 })

-- Function argument
argument :: Parser (Exp Untyped -> Region -> Exp Untyped)
argument =
  (do ident <- angles (lvlVar)
      return (\e r -> LambLvl ident e Untyped r))
    <|>
  (do ident <- identifier
      return (\e r -> Lamb ident Untyped e Untyped r))

-- Zero or more function arguments
arguments :: Parser (Exp Untyped -> Region -> Exp Untyped)
arguments = 
    try (do a <- argument
            more <- arguments
            return (\e r -> a (more e r) r))
    <|> (return (\e _ -> e))

-- One or more function arguments
arguments1 :: Parser (Exp Untyped -> Region -> Exp Untyped)
arguments1 =
  do a <- argument
     as <- arguments
     return (\e r -> a (as e r) r)

---------------------
--   Expressions   --
---------------------
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

-- Wrap unary function, to allow partial application
unop :: (Exp Untyped -> Region -> Exp Untyped) ->  Parser (Region -> Exp Untyped)
unop opr =
  return (\r ->
      Lamb "x" Untyped
           (opr (Var "x" Untyped Missing) r)
           Untyped r)

-- Wrap binary function, to allow partial application
binop :: (Exp Untyped -> Exp Untyped -> Region -> Exp Untyped) ->  Parser (Region -> Exp Untyped)
binop opr =
  return (\r ->
      Lamb "x" Untyped
          (Lamb "y" Untyped
              (opr (Var "x" Untyped Missing)
                   (Var "y" Untyped Missing) r)
              Untyped r)
          Untyped r)

-- Wrap ternary function, to allow partial application
triop :: (Exp Untyped -> Exp Untyped -> Exp Untyped -> Region -> Exp Untyped) ->  Parser (Region -> Exp Untyped)
triop opr =
  return (\r ->
      Lamb "x" Untyped
          (Lamb "y" Untyped
              (Lamb "z" Untyped
                  (opr (Var "x" Untyped Missing)
                       (Var "y" Untyped Missing)
                       (Var "z" Untyped Missing) r)
                  Untyped r)
              Untyped r)
          Untyped r)

op :: Parser (Exp Untyped)
op = withRegion (identifier >>= switch)
  where
    switch "#BlockSize"  = return BlockSize
    switch "i2d"        = unop (UnOp I2D)
    switch "b2i"        = unop (UnOp B2I)
    switch "clz"        = unop (UnOp CLZ)
    switch "negatei"    = unop (UnOp NegateI)
    switch "addi"       = binop (BinOp AddI)
    switch "subi"       = binop (BinOp SubI)
    switch "muli"       = binop (BinOp MulI)
    switch "divi"       = binop (BinOp DivI)
    switch "modi"       = binop (BinOp ModI)
    switch "mini"       = binop (BinOp MinI)
    switch "eqi"        = binop (BinOp EqI)
    switch "neqi"       = binop (BinOp NeqI)
    switch "powi"       = binop (BinOp PowI)
    switch "shiftLi"    = binop (BinOp ShiftLI)
    switch "shiftRi"    = binop (BinOp ShiftRI)
    switch "andi"       = binop (BinOp AndI)
    switch "ori"        = binop (BinOp OrI)
    switch "xori"       = binop (BinOp XorI)
    switch "powr"       = binop (BinOp PowR)
    switch "divr"       = binop (BinOp DivR)
    switch "fst"        = unop Proj1E
    switch "snd"        = unop Proj2E
    switch "lengthPull" = unop LengthPull
    switch "lengthPush" = unop LengthPush
    switch "force"      = unop Force
    switch "push"       =
      let var = LvlVar 0 (Just "thisisahack") -- TODO fix this hack
      in return (\r -> LambLvl var (Lamb "x" Untyped (Push (VarL var) (Var "x" Untyped Missing) r) Untyped r) Untyped r)
    switch "index"      = binop Index
    switch "generatePull" = binop GeneratePull
    switch "mapPull"    = binop MapPull
    switch "mapPush"    = binop MapPush
    switch "while"      = triop While
    switch "whileSeq"   = triop WhileSeq
    switch "interleave" = triop Interleave
    switch "scanl"      = triop Scanl
    switch n            = return (Var n Untyped)

fn :: Parser (Exp Untyped)
fn =
  withRegion $
    do reserved "fn"
       f <- arguments1
       symbol "=>"
       e <- expr
       return (f e)

expr :: Parser (Exp Untyped)
expr =
  let
    lvlapp :: Parser (Exp Untyped -> Exp Untyped)
    lvlapp =
      do e2 <- angles level
         return (\e1 -> AppLvl e1 e2)

    pipeForward :: Parser (Exp Untyped -> Exp Untyped -> Exp Untyped)
    pipeForward =
      do reservedOp "|>"
         return (\e1 e2 -> App e2 e1)

    binOp :: String -> BinOp -> Parser (Exp Untyped -> Exp Untyped -> Exp Untyped)
    binOp opName operator =
      do reservedOp opName
         return (\e1 e2 -> BinOp operator e1 e2 Missing)

    table = [ [Infix (return App) AssocLeft, Postfix lvlapp],
              [Infix (binOp "*" MulI) AssocLeft, Infix (binOp "/" DivI) AssocLeft, Infix (binOp "%" ModI) AssocLeft],
              [Infix (binOp "+" AddI) AssocLeft, Infix (binOp "-" SubI) AssocLeft],
              [Infix (binOp "<<" ShiftLI) AssocLeft, Infix (binOp ">>" ShiftRI) AssocLeft],
              [Infix (binOp "==" EqI) AssocLeft, Infix (binOp "!=" NeqI) AssocLeft],
              [Infix pipeForward AssocLeft]
            ]
  in buildExpressionParser table term

------------------
--    Types
------------------
type' :: Parser Type
type' =
 let scan =
       do x <- (Left <$> simpleType) <|> (Right <$> (angles lvlVar))
          rest x

     rest (Left x) =
       (do symbol "->"
           y <- scan
           return (x :> y))
       <|> return x
     rest (Right lvl) =
       do symbol "->"
          y <- scan
          return (lvl :-> y)
 in scan

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

level :: Parser Level
level = (reserved "thread" >> return threadLevel)
    <|> (reserved "warp" >> return warpLevel)
    <|> (reserved "block" >> return blockLevel)
    <|> (reserved "grid" >> return gridLevel)
    <|> (VarL <$> lvlVar)
    <|> do char '1'
           reservedOp "+"
           lvl <- level
           return (Step lvl)

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

-- All type variables are numbered identical (0), but needs to be
-- renumbered properly before type checking (see Language.FCL.InitTyVars)
tyVar :: Parser Type
tyVar =
  do name <- identifier
     return (VarT (TyVar 0 (Just name)))

lvlVar :: Parser LvlVar
lvlVar =
  do name <- identifier
     return (LvlVar 0 (Just name))
