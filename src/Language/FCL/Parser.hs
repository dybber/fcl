{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- | Parse FCL programs. After parsing, all type variables and level
-- variables needs to be numbered. This is not done here to seperate
-- concerns.
module Language.FCL.Parser
  (parseTopLevel, ParseError)
where

import qualified Data.Map as Map

import Text.Parsec hiding (Empty)
import Text.Parsec.Expr

import Language.FCL.Identifier
import Language.FCL.SourceRegion
import Language.FCL.Syntax
import Language.FCL.Lexer

-----------
-- Monad --
-----------
data ParserState =
  ParserState
    { varCount :: Int
    , tyEnv :: Map.Map Identifier Type
    , lvlEnv :: Map.Map Identifier LvlVar
    }

type ParserFCL = Parsec String ParserState

initState :: ParserState
initState =
  ParserState
    { varCount = 0
    , tyEnv = Map.empty
    , lvlEnv = Map.empty
    }

clearEnv :: ParserFCL ParserState
clearEnv =
  do oldState <- getState
     modifyState (\s -> s { tyEnv = Map.empty
                          , lvlEnv = Map.empty })
     return oldState

resetEnv :: ParserState -> ParserFCL ()
resetEnv oldState =
  modifyState (\s -> s { tyEnv = tyEnv oldState
                       , lvlEnv = lvlEnv oldState })

gets :: (ParserState -> a) -> ParserFCL a
gets f =
  do s <- getState
     return (f s)

incVarCount :: ParserFCL Int
incVarCount =
  do i <- gets varCount
     modifyState (\s -> s {varCount = i+1})
     return i

newLvlVar :: ParserFCL LvlVar
newLvlVar = do
 i <- incVarCount
 return (LvlVar i Nothing)

newNamedTV :: Identifier -> ParserFCL Type
newNamedTV name =
  do env <- gets tyEnv
     case Map.lookup name env of
       Just tv -> return tv
       Nothing ->
         do i <- incVarCount
            let tv = (VarT (TyVar i (Just name)))
            let env' = Map.insert name tv env
            modifyState (\s -> s { tyEnv = env' })
            return tv

newNamedLvlVar :: Identifier -> ParserFCL LvlVar
newNamedLvlVar name =
  do env <- gets lvlEnv
     case Map.lookup name env of
       Just lvlvar -> return lvlvar
       Nothing ->
         do i <- incVarCount
            let lvlvar = (LvlVar i (Just name))
            let env' = Map.insert name lvlvar env
            modifyState (\s -> s { lvlEnv = env' })
            return lvlvar

----------------------
-- Exported parsers --
----------------------
parseTopLevel :: String -> String -> Either ParseError [Definition Untyped]
parseTopLevel filename programText = runParser topLevel initState filename programText

-- parseType :: String -> String -> Either ParseError Type
-- parseType filename input = runParser type' initState filename input

---------------------------
-- Top-level definitions --
---------------------------
topLevel :: ParserFCL [Definition Untyped]
topLevel =
  do whitespace
     prog <- many1 definition
     eof
     return prog

definition :: ParserFCL (Definition Untyped)
definition = try (typesig >>= fundef)  -- fun.def. w. signature
          <|> fundef Nothing           -- fun.def.

typesig :: ParserFCL (Maybe (String,Type))
typesig =
  do reserved "sig"
     ident <- identifier
     colon
     oldEnv <- clearEnv
     ty <- type'
     resetEnv oldEnv
     return (Just (ident,ty))

fundef :: Maybe (String, Type) -> ParserFCL (Definition Untyped)
fundef tyanno =
    do reserved "fun"
       name <- identifier
       case tyanno of
         Just (signame, _) ->
           if signame == name
           then return ()
           else fail "Name in signature and function-definition does not match."
         Nothing -> return ()
       args <- arguments
       reservedOp "="
       rhs <- expr
       let function = args rhs Missing
       return (Definition
                 { defVar = Identifier name
                 , defSignature = fmap snd tyanno
                 , defTypeScheme = TypeScheme [] Untyped
                 , defBody = function
                 })

-- Function argument
argument :: ParserFCL (Exp Untyped -> SourceRegion -> Exp Untyped)
argument =
  (do ident <- angles (lvlVar)
      return (\e r -> LambLvl ident e Untyped r))
    <|>
  (do ident <- identifier
      return (\e r -> Lamb (Identifier ident) Untyped e Untyped r))

-- Zero or more function arguments
arguments :: ParserFCL (Exp Untyped -> SourceRegion -> Exp Untyped)
arguments = 
    try (do a <- argument
            more <- arguments
            return (\e r -> a (more e r) r))
    <|> (return (\e _ -> e))

-- One or more function arguments
arguments1 :: ParserFCL (Exp Untyped -> SourceRegion -> Exp Untyped)
arguments1 =
  do a <- argument
     as <- arguments
     return (\e r -> a (as e r) r)

---------------------
--   Expressions   --
---------------------
term :: ParserFCL (Exp Untyped)
term =
   try fn
   <|> tupleOrParens
   <|> if'
   <|> do'
   <|> try bool
   <|> try floating
   <|> try integer
   <|> try stringLiteral
   <|> try op
   <|> array
   <|> let'

sign :: Num a => ParserFCL (a -> a)
sign = (oneOf "-~" >> return negate)
       <|> (char '+' >> return id)
       <|> return id

integer :: ParserFCL (Exp Untyped)
integer =
  withSourceRegion $ do
    f <- sign
    n <- natural
    return (Literal (LiteralInt (fromInteger (f n))))

floating :: ParserFCL (Exp Untyped)
floating =
  withSourceRegion $ do
    f <- sign
    n <- float
    return (Literal (LiteralDouble (f n)))

bool :: ParserFCL (Exp Untyped)
bool =
  withSourceRegion $
    (reserved "true" >> return (Literal (LiteralBool True)))
    <|> (reserved "false" >> return (Literal (LiteralBool False)))

stringLiteral :: ParserFCL (Exp Untyped)
stringLiteral =
  withSourceRegion $ do
    str <- stringlit
    return (Literal (LiteralString str))

if' :: ParserFCL (Exp Untyped)
if' =
  withSourceRegion $ do
    reserved "if"
    e1 <- expr
    reserved "then"
    e2 <- expr
    reserved "else"
    e3 <- expr
    return (Cond e1 e2 e3 Untyped)

do' :: ParserFCL (Exp Untyped)
do' = reserved "do" >> braces dobody
 where
   dobody :: ParserFCL (Exp Untyped)
   dobody =
     try bind <|> try bindignore <|> expr

   bindignore :: ParserFCL (Exp Untyped)
   bindignore =
     withSourceRegion $
       do e <- expr
          reservedOp ";"
          rest <- dobody
          return (Bind e (Lamb (Identifier "$ignored") Untyped rest Untyped Missing))

   bind :: ParserFCL (Exp Untyped)
   bind =
     withSourceRegion $
       do v <- identifier
          reservedOp "<-"
          e <- expr
          reservedOp ";"
          rest <- dobody
          return (Bind e (Lamb (Identifier v) Untyped rest Untyped Missing))


array :: ParserFCL (Exp Untyped)
array = withSourceRegion $ do
  elems <- brackets (sepBy expr comma)
  return (Vec elems Untyped)

tupleOrParens :: ParserFCL (Exp Untyped)
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

let' :: ParserFCL (Exp Untyped)
let' =
  withSourceRegion $ do
    reserved "let"
    ident <- identifier
    symbol "="
    e1 <- expr
    reserved "in"
    e2 <- expr
    return (Let (Identifier ident) e1 e2 Untyped)


unop :: Bool -> (Exp Untyped -> SourceRegion -> Exp Untyped) -> ParserFCL (SourceRegion -> Exp Untyped)
unop False opr =
  do char '('
     e1 <- expr
     char ')'
     return (opr e1)
-- Wrap unary function, to allow partial application
unop True opr =
  return (\r ->
      Lamb (Identifier "x") Untyped
           (opr (Var (Identifier "x") Untyped Missing) r)
           Untyped r)

binop :: Bool -> (Exp Untyped -> Exp Untyped -> SourceRegion -> Exp Untyped) ->  ParserFCL (SourceRegion -> Exp Untyped)
binop False opr =
  do char '('
     e1 <- expr
     char ','
     e2 <- expr
     char ')'
     return (opr e1 e2)
-- Wrap binary function, to allow partial application
binop True opr =
    return (\r ->
      Lamb (Identifier "x") Untyped
          (Lamb (Identifier "y") Untyped
              (opr (Var (Identifier "x") Untyped Missing)
                   (Var (Identifier "y") Untyped Missing) r)
              Untyped r)
          Untyped r)

triop :: Bool -> (Exp Untyped -> Exp Untyped -> Exp Untyped -> SourceRegion -> Exp Untyped) -> ParserFCL (SourceRegion -> Exp Untyped)
triop False opr =
  do char '('
     e1 <- expr
     char ','
     e2 <- expr
     char ','
     e3 <- expr
     char ')'
     return (opr e1 e2 e3)
triop True opr = -- Wrap ternary function, to allow partial application
  return (\r ->
      Lamb (Identifier "x") Untyped
          (Lamb (Identifier "y") Untyped
              (Lamb (Identifier "z") Untyped
                  (opr (Var (Identifier "x") Untyped Missing)
                       (Var (Identifier "y") Untyped Missing)
                       (Var (Identifier "z") Untyped Missing) r)
                  Untyped r)
              Untyped r)
          Untyped r)

builtin_ops :: Bool -> String -> (ParserFCL (SourceRegion -> Exp Untyped))
builtin_ops _ "BlockSize"  = return BlockSize
builtin_ops curried "i2d"        = unop curried (UnaryOp I2D)
builtin_ops curried "b2i"        = unop curried (UnaryOp B2I)
builtin_ops curried "clz"        = unop curried (UnaryOp CLZ)
builtin_ops curried "negatei"    = unop curried (UnaryOp NegateI)
builtin_ops curried "addi"       = binop curried (BinaryOp AddI)
builtin_ops curried "addr"       = binop curried (BinaryOp AddR)
builtin_ops curried "subi"       = binop curried (BinaryOp SubI)
builtin_ops curried "muli"       = binop curried (BinaryOp MulI)
builtin_ops curried "divi"       = binop curried (BinaryOp DivI)
builtin_ops curried "modi"       = binop curried (BinaryOp ModI)
builtin_ops curried "mini"       = binop curried (BinaryOp MinI)
builtin_ops curried "maxi"       = binop curried (BinaryOp MaxI)
builtin_ops curried "eqi"        = binop curried (BinaryOp EqI)
builtin_ops curried "neqi"       = binop curried (BinaryOp NeqI)
builtin_ops curried "lti"        = binop curried (BinaryOp LtI)
builtin_ops curried "powi"       = binop curried (BinaryOp PowI)
builtin_ops curried "shiftLi"    = binop curried (BinaryOp ShiftLI)
builtin_ops curried "shiftRi"    = binop curried (BinaryOp ShiftRI)
builtin_ops curried "andi"       = binop curried (BinaryOp AndI)
builtin_ops curried "ori"        = binop curried (BinaryOp OrI)
builtin_ops curried "xori"       = binop curried (BinaryOp XorI)
builtin_ops curried "powr"       = binop curried (BinaryOp PowR)
builtin_ops curried "divr"       = binop curried (BinaryOp DivR)
builtin_ops curried "fst"        = unop curried Proj1E
builtin_ops curried "snd"        = unop curried Proj2E
builtin_ops curried "lengthPull" = unop curried LengthPull
builtin_ops curried "lengthPush" = unop curried LengthPush
builtin_ops curried "force"      = unop curried Force
builtin_ops False "push"       =
  do char '('
     lvl <- angles level
     char ','
     e <- expr
     char ')'
     return (Push lvl e)
builtin_ops True "push"       =
  do var <- newLvlVar
     return (\r -> LambLvl var (Lamb (Identifier "x") Untyped (Push (VarL var) (Var (Identifier "x") Untyped Missing) r) Untyped r) Untyped r)
builtin_ops False "return"       =
  do char '('
     lvl <- angles level
     char ','
     e <- expr
     char ')'
     return (Return lvl e)
builtin_ops True "return"     =
  do var <- newLvlVar
     return (\r -> LambLvl var (Lamb (Identifier "x") Untyped (Return (VarL var) (Var (Identifier "x") Untyped Missing) r) Untyped r) Untyped r)
builtin_ops curried "bind"         = binop curried Bind
builtin_ops curried "index"        = binop curried Index
builtin_ops curried "generatePull" = binop curried GeneratePull
builtin_ops curried "mapPull"    = binop curried MapPull
builtin_ops curried "mapPush"    = binop curried MapPush
builtin_ops curried "power"      = triop curried Power
builtin_ops curried "seqfor"      = triop curried For
builtin_ops curried "while"      = triop curried While
builtin_ops curried "whileSeq"   = triop curried WhileSeq
builtin_ops curried "interleave" = triop curried Interleave
builtin_ops curried "forceAndPrint" = binop curried ForceAndPrint
builtin_ops curried "benchmark" = binop curried Benchmark
builtin_ops curried "readIntCSV" = unop curried ReadIntCSV
builtin_ops False n            = return (Var (Identifier ('#':n)) Untyped)
builtin_ops True n            = return (Var (Identifier n) Untyped)

op :: ParserFCL (Exp Untyped)
op =
  withSourceRegion $
    do name <- identifier
       case name of
         ('#':name') -> builtin_ops False name'
         name'       -> builtin_ops True name'

fn :: ParserFCL (Exp Untyped)
fn =
  withSourceRegion $
    do reserved "fn"
       old <- getState
       f <- arguments1
       symbol "=>"
       e <- expr
       resetEnv old
       return (f e)

expr :: ParserFCL (Exp Untyped)
expr =
  let
    lvlapp :: ParserFCL (Exp Untyped -> Exp Untyped)
    lvlapp =
      do e2 <- angles level
         return (\e1 -> AppLvl e1 e2)

    pipeForward :: ParserFCL (Exp Untyped -> Exp Untyped -> Exp Untyped)
    pipeForward =
      do reservedOp "|>"
         return (\e1 e2 -> App e2 e1)

    binOp :: String -> BinaryOperator -> ParserFCL (Exp Untyped -> Exp Untyped -> Exp Untyped)
    binOp opName operator =
      do reservedOp opName
         return (\e1 e2 -> BinaryOp operator e1 e2 Missing)

    table = [ [Infix (return App) AssocLeft, Postfix lvlapp],
              [Infix (binOp "*" MulI) AssocLeft, Infix (binOp "/" DivI) AssocLeft, Infix (binOp "%" ModI) AssocLeft],
              [Infix (binOp "+" AddI) AssocLeft, Infix (binOp "-" SubI) AssocLeft],
              [Infix (binOp "<<" ShiftLI) AssocLeft, Infix (binOp ">>" ShiftRI) AssocLeft],
              [Infix (binOp "==" EqI) AssocLeft, Infix (binOp "!=" NeqI) AssocLeft],
              [Infix pipeForward AssocLeft]
            ]
  in buildExpressionParser table term

-------------------
-- Parsing types --
-------------------

type' :: ParserFCL Type
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

simpleType :: ParserFCL Type
simpleType =
  baseType
  <|> programType
  <|> tyVar
  <|> tupleType
  <|> arrayType

baseType :: ParserFCL Type
baseType = (reserved "int" >> return IntT)
       <|> (reserved "double" >> return DoubleT)
       <|> (reserved "bool" >> return BoolT)
       <|> (reserved "string" >> return StringT)
       <|> (reserved "unit" >> return UnitT)

programType :: ParserFCL Type
programType =
 do reserved "Program"
    lvl <- angles level
    ty <- simpleType
    return (ProgramT lvl ty)

level :: ParserFCL Level
level = (reserved "thread" >> return threadLevel)
    <|> (reserved "block" >> return blockLevel)
    <|> (reserved "grid" >> return gridLevel)
    <|> (VarL <$> lvlVar)
    <|> do char '1'
           reservedOp "+"
           lvl <- level
           return (Step lvl)

arrayType :: ParserFCL Type
arrayType =
  do ty <- brackets type'
     (try (do lvl <- angles level
              return (PushArrayT lvl ty)))
       <|> return (PullArrayT ty)

tupleType :: ParserFCL Type
tupleType =
  do symbol "("
     t1 <- type'
     try (do comma
             t2 <- type'
             symbol ")"
             return (t1 :*: t2))
      <|> do symbol ")"
             return t1

tyVar :: ParserFCL Type
tyVar =
  char '\'' >> (Identifier <$> identifier) >>= newNamedTV

lvlVar :: ParserFCL LvlVar
lvlVar = (Identifier <$> identifier) >>= newNamedLvlVar
