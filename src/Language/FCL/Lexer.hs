-- | Lexical elements of FCL
module Language.FCL.Lexer where

import Text.Parsec

import qualified Text.Parsec.Token as Token
import Data.Functor.Identity (Identity)

reservedNames :: [String]
reservedNames =
  ["sig",
   "fun",
   "let",
   "in",
   "int",
   "double",
   "bool",
   "char",
   "fn",
   "true",
   "false",
   "kernel",
   "if",
   "then",
   "else",
   "thread",
   "block",
   "grid",
   "config",
   "do"]

reservedOps :: [String]
reservedOps = ["|>",
               "+",
               "-",
               "*",
               "/",
               "%",
               "!=",
               "==",
               "<<",
               ">>"]

fclDef :: Token.GenLanguageDef String u Identity
fclDef = Token.LanguageDef {
                Token.commentStart     = "(*"
              , Token.commentEnd       = "*)"
              , Token.commentLine      = "--"
              , Token.nestedComments   = True
              , Token.identStart       = letter <|> char '#'
              , Token.identLetter      = alphaNum <|> char '_' <|> char '#'
              , Token.opStart          = oneOf ""
              , Token.opLetter         = oneOf ""
              , Token.reservedOpNames  = reservedOps
              , Token.reservedNames    = reservedNames
              , Token.caseSensitive    = True
  }

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser fclDef

lexeme  :: ParsecT String u Identity a -> ParsecT String u Identity a
lexeme = Token.lexeme lexer

whitespace :: ParsecT String u Identity ()
whitespace = Token.whiteSpace lexer

identifier :: ParsecT String u Identity String
identifier = Token.identifier lexer

reserved :: String -> ParsecT String u Identity ()
reserved = Token.reserved   lexer

reservedOp :: String -> ParsecT String u Identity ()
reservedOp = Token.reservedOp lexer

symbol :: String -> ParsecT String u Identity String
symbol = Token.symbol lexer

stringlit :: ParsecT String u Identity String
stringlit = Token.stringLiteral lexer

charlit :: ParsecT String u Identity Char
charlit = Token.charLiteral lexer

semi :: ParsecT String u Identity String
semi = Token.semi lexer

comma :: ParsecT String u Identity String
comma = Token.comma lexer

colon :: ParsecT String u Identity String
colon = Token.colon lexer

natural :: ParsecT String u Identity Integer
natural = Token.natural lexer

float :: ParsecT String u Identity Double
float = Token.float lexer

parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = Token.parens lexer

brackets :: ParsecT String u Identity a -> ParsecT String u Identity a
brackets = Token.brackets lexer

angles :: ParsecT String u Identity a -> ParsecT String u Identity a
angles = Token.angles lexer

braces :: ParsecT String u Identity a -> ParsecT String u Identity a
braces = Token.braces lexer
