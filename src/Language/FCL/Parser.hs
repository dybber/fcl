{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Language.FCL.Parser
  (parseFile,
   parseString)
where

import Control.Applicative hiding ((<|>), many)
import Control.Monad (when)
import Control.Monad.Identity (Identity)
import Text.Parsec hiding (Empty)
import Text.Parsec.String
import qualified Text.Parsec.Token as Token

import Language.FCL.Syntax

parseString :: String -> String -> (Prog Untyped)
parseString programText filename =
  case parse program filename programText of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO (Prog Untyped)
parseFile filename =
  do contents <- readFile filename
     case parse program filename contents of
       Left e  -> error $ show e
       Right r -> return r

fclDef :: Token.GenLanguageDef String u Identity
fclDef = Token.LanguageDef {
                Token.commentStart     = "(*"
              , Token.commentEnd       = "*)"
              , Token.commentLine      = "--"
              , Token.nestedComments   = False
              , Token.identStart       = letter
              , Token.identLetter      = alphaNum <|> char '_'
              , Token.opStart          = oneOf ""
              , Token.opLetter         = oneOf ""
              , Token.reservedOpNames  = []
              , Token.reservedNames    = [ "sig", "fun", "let", "in", "int", "double",
                                           "bool", "char", "fn", "true", "false" ]
              , Token.caseSensitive    = True
  }

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser fclDef

identifier :: Parser String
identifier = Token.identifier lexer
reserved :: String -> Parser ()
reserved   = Token.reserved   lexer
-- reservedOp :: String -> Parser ()
-- reservedOp = Token.reservedOp lexer
-- stringlit :: Parser String
-- stringlit  = Token.stringLiteral lexer
-- charlit :: Parser Char
-- charlit    = Token.charLiteral lexer
parens :: Parser a -> Parser a
parens     = Token.parens     lexer
brackets :: Parser a -> Parser a
brackets   = Token.brackets   lexer
-- angles :: Parser a -> Parser a
-- angles     = Token.angles     lexer
-- braces :: Parser a -> Parser a
-- braces     = Token.braces     lexer
-- integer :: Parser Integer
-- integer    = Token.integer    lexer
-- semi :: Parser String
-- semi       = Token.semi       lexer
comma :: Parser String
comma      = Token.comma      lexer
colon :: Parser String
colon      = Token.colon      lexer
symbol :: String -> Parser String
symbol     = Token.symbol     lexer
whitespace :: Parser ()
whitespace = Token.whiteSpace lexer
decimal :: Parser Integer
decimal    = Token.decimal    lexer
float :: Parser Double
float      = Token.float      lexer
lexeme :: Parser a -> Parser a
lexeme     = Token.lexeme     lexer

program :: Parser (Prog Untyped)
program =
  do whitespace
     prog <- many1 definition
     eof
     return prog

-------------------------------------
-- Definitions and type signatures --
-------------------------------------
definition :: Parser (Definition Untyped)
definition = try (typesig >>= def) <|> def Nothing

typesig :: Parser (Maybe (String,Type))
typesig =
  do reserved "sig"
     ident <- identifier
     colon
     ty <- typeExpr
     return (Just (ident,ty))

def :: Maybe (String, Type) -> Parser (Definition Untyped)
def tyanno =
  let
    addArgs :: [(Variable, Type)] -> Exp Untyped -> Exp Untyped
    addArgs [] rhs = rhs
    addArgs ((v,ty):xs) rhs = addArgs xs (Lamb v ty rhs Untyped)
  in
    do reserved "fun"
       name <- identifier
       args <- many (parens typedIdent)
       symbol "="
       rhs <- expr
       let function = addArgs (reverse args) rhs
       case tyanno of
         Just (name', typ) -> do
           when (name /= name') (fail "Different identifier in signature and definition")
           return (Definition name (Just typ) function)
         Nothing -> return (Definition name Nothing function)
  
----------------------
--    Expressions    --
----------------------
expr :: Parser (Exp Untyped)
expr = chainl1 nonAppExpr (return App)

nonAppExpr :: Parser (Exp Untyped)
nonAppExpr = fnExpr
   <|> pairOrParens
   <|> opExpr
   <|> arrayExpr
   <|> letExpr
   <|> valueExpr
 <?> "expression"

valueExpr :: Parser (Exp Untyped)
valueExpr = try (DoubleScalar <$> lexeme float)
         <|> (IntScalar . fromInteger) <$> lexeme decimal
         <|> boolExpr
         <|> (oneOf "~-" >> UnOp NegateI <$> valueExpr) -- TODO: negation of doubles
         <?> "value or identifier"

boolExpr :: Parser (Exp Untyped)
boolExpr = try (reserved "true" >> return (BoolScalar True))
       <|> try (reserved "false" >> return (BoolScalar False))
       <?> "boolean value"

arrayExpr :: Parser (Exp Untyped)
arrayExpr = Vec <$> (brackets (sepBy expr comma)) <*> pure Untyped

pairOrParens :: Parser (Exp Untyped)
pairOrParens =
  do symbol "("
     t1 <- expr
     try (do comma
             t2 <- expr
             symbol ")"
             return (Pair t1 t2))
      <|> do symbol ")"
             return t1

letExpr :: Parser (Exp Untyped)
letExpr =
  do reserved "let"
     (ident, typ) <- typedIdent
     symbol "="
     e1 <- expr
     reserved "in"
     e2 <- expr
     return (Let ident e1 e2 Untyped)

opExpr :: Parser (Exp Untyped)
opExpr = identifier >>= switch
  where
    switch "addi" = BinOp AddI <$> nonAppExpr <*> nonAppExpr
    switch "subi" = BinOp SubI <$> nonAppExpr <*> nonAppExpr
    switch "muli" = BinOp MulI <$> nonAppExpr <*> nonAppExpr
    switch "divi" = BinOp DivI <$> nonAppExpr <*> nonAppExpr
    switch "modi" = BinOp ModI <$> nonAppExpr <*> nonAppExpr
    switch "mini" = BinOp MinI <$> nonAppExpr <*> nonAppExpr
    switch "eqi"  = BinOp EqI  <$> nonAppExpr <*> nonAppExpr
    switch "neqi" = BinOp NeqI <$> nonAppExpr <*> nonAppExpr
    switch "fst"      = Proj1E         <$> nonAppExpr <?> "fst"
    switch "snd"      = Proj2E         <$> nonAppExpr <?> "snd"
    switch "index"    = Index          <$> nonAppExpr <*> nonAppExpr <?> "index"
    switch "length"   = Length         <$> nonAppExpr <?> "length"
    switch "generate" = Generate Block <$> nonAppExpr <*> nonAppExpr <?> "generate"
    switch "while"    = Fixpoint       <$> nonAppExpr <*> nonAppExpr <*> nonAppExpr <?> "while"
    switch "map"      = Map            <$> nonAppExpr <*> nonAppExpr <?> "map"
    switch "force"    = ForceLocal     <$> nonAppExpr <?> "force"
    switch "assemble" = Assemble       <$> nonAppExpr <*> nonAppExpr <*> nonAppExpr <?> "assemble"
    switch n          = return (Var n Untyped)

fnExpr :: Parser (Exp Untyped)
fnExpr =
  do reserved "fn"
     (ident, typ) <- typedIdent
     symbol "=>"
     e <- expr
     return (Lamb ident typ e Untyped)

typedIdent :: Parser (Variable, Type)
typedIdent =
  do ident <- identifier
     colon
     typ <- typeExpr
     return (ident, typ)

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

tyVar :: Parser Type
tyVar = TyVar <$> identifier 
       <?> "base type"
