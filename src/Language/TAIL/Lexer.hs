module Language.TAIL.Lexer where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as Token

import Data.Functor.Identity (Identity)

reservedNames :: [String]
reservedNames =
  [ "let", "in", "int", "double",
    "bool", "char", "fn", "inf", "tt", "ff" ]

reservedOps :: [String]
reservedOps = []

tailDef :: Token.GenLanguageDef String u Identity
tailDef = Token.LanguageDef {
                Token.commentStart     = "(*"
              , Token.commentEnd       = "*)"
              , Token.commentLine      = ""
              , Token.nestedComments   = True
              , Token.identStart       = letter
              , Token.identLetter      = alphaNum <|> char '_'
              , Token.opStart          = oneOf ""
              , Token.opLetter         = oneOf ""
              , Token.reservedOpNames  = reservedOps
              , Token.reservedNames    = reservedNames
              , Token.caseSensitive    = True
  }

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser tailDef

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

stringlit :: Parser String
stringlit = Token.stringLiteral lexer

charlit :: Parser Char
charlit = Token.charLiteral lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

angles :: Parser a -> Parser a
angles = Token.angles lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

integer :: Parser Integer
integer = Token.integer lexer

semi :: Parser String
semi = Token.semi lexer

comma :: Parser String
comma = Token.comma lexer

colon :: Parser String
colon = Token.colon lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

whitespace :: Parser ()
whitespace = Token.whiteSpace lexer

natural :: Parser Integer
natural = Token.natural lexer

float :: Parser Double
float = Token.float lexer
