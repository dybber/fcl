{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- | Parse FCL programs. After parsing, all type variables and level
-- variables needs to be numbered. This is not done here to seperate
-- concerns.
module FCL.External.Parser
  (parseTopLevel, ParseError)
where

import qualified Data.Map as Map
import Data.Functor.Identity (Identity)

import Text.Parsec hiding (Empty)
import Text.Parsec.Expr

import FCL.Core.Identifier
import FCL.Core.SourceRegion
import FCL.External.Lexer
import FCL.External.Syntax
import FCL.Type.Polymorphic

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
type Op a = Operator String ParserState Identity a

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

-- newLvlVar :: ParserFCL LvlVar
-- newLvlVar = do
--  i <- incVarCount
--  return (LvlVar i Nothing)

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
       pos1 <- getPosition
       args <- arguments
       pos2 <- getPosition
       reservedOp "="
       rhs <- expr
       let function = args rhs (regionFromPos pos1 pos2)
       return (Definition
                 { defVar = Identifier name
                 , defSignature = fmap snd tyanno
                 , defTypeScheme = TypeScheme [] [] Untyped
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
   <|> try var
   <|> array
   <|> let'

sign :: Num a => ParserFCL (a -> a)
sign = (oneOf "~" >> return negate)
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
          return (bindConstructor e (Lamb (Identifier "$ignored") Untyped rest Untyped (newRegion Missing Missing)))

   bind :: ParserFCL (Exp Untyped)
   bind =
     withSourceRegion $
       do v <- identifier
          reservedOp "<-"
          e <- expr
          reservedOp ";"
          rest <- dobody
          return (bindConstructor e (Lamb (Identifier v) Untyped rest Untyped (newRegion Missing Missing)))


-- Wrap binary function, to allow partial application
bindConstructor :: Exp Untyped -> Exp Untyped -> SourceRegion -> Exp Untyped
bindConstructor e1 e2 reg =
  App (App (Symbol (Identifier "bind") Untyped reg)
           e1
           (newRegion Missing Missing))
      e2
      (newRegion Missing Missing)


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
             return (Pair t1 t2 (regionFromPos p1 p2)))
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

var :: ParserFCL (Exp Untyped)
var =
  withSourceRegion $
    do name <- identifier
       return (Symbol (Identifier name) Untyped)

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
    application :: Op (Exp Untyped)
    application =
      Infix (return (\e1 e2 ->
                        let start = beginPosition (getSourceRegion e1)
                            end = endPosition (getSourceRegion e2)
                        in App e1 e2 (newRegion start end)))
            AssocLeft

    lvlapp :: Op (Exp Untyped)
    lvlapp =
      Postfix (try $ do e2 <- angles level
                        return (\e1 ->
                                 let start = beginPosition (getSourceRegion e1)
                                 in AppLvl e1 e2 (newRegion start Missing)))

    pipeForward :: Op (Exp Untyped)
    pipeForward =
      Infix (do reservedOp "|>"
                return (\e1 e2 ->
                        let start = beginPosition (getSourceRegion e1)
                            end = endPosition (getSourceRegion e2)
                        in App e2 e1 (newRegion start end)))
            AssocLeft

    binOp :: String -> BinaryOperator -> ParserFCL (Exp Untyped -> Exp Untyped -> Exp Untyped)
    binOp opName operator =
      do pos1 <- getPosition
         reservedOp opName
         pos2 <- getPosition
         let reg = regionFromPos pos1 pos2
         return (\e1 e2 -> BinaryOp operator e1 e2 reg)

    table = [ [application, lvlapp],
              [Infix (binOp "*" MulI) AssocLeft, Infix (binOp "/" DivI) AssocLeft, Infix (binOp "%" ModI) AssocLeft],
              [Infix (binOp "+" AddI) AssocLeft, Infix (binOp "-" SubI) AssocLeft],
              [Infix (binOp "<<" ShiftLI) AssocLeft, Infix (binOp ">>" ShiftRI) AssocLeft, Infix (binOp "&" AndI) AssocLeft],
              [Infix (binOp "==" EqI) AssocLeft, Infix (binOp "!=" NeqI) AssocLeft],
              [pipeForward]
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


----------------------
-- Parsec utilities --
----------------------

-- | Convert Parsec `SourcePos` to `Position`
fromSourcePos :: SourcePos -> Position
fromSourcePos pos = Position { fileName = sourceName pos
                             , line = sourceLine pos
                             , column = sourceColumn pos }


-- | Convert two Parsec `SourcePos` positions to a `Region`
regionFromPos :: SourcePos -> SourcePos -> SourceRegion
regionFromPos pos1 pos2 = SourceRegion (fromSourcePos pos1)
                                       (fromSourcePos pos2)

withSourceRegion :: Monad m => ParsecT s u m (SourceRegion -> b) -> ParsecT s u m b
withSourceRegion p = do
  pos1 <- getPosition
  f <- p
  pos2 <- getPosition
  return (f (regionFromPos pos1 pos2))
