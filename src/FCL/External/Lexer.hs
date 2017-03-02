module FCL.External.Lexer where

import Text.Parsec
import qualified Text.Parsec.Token as Token

reservedNames :: [String]
reservedNames =
  ["sig",
   "fun",
   "val",
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
   "do"]

reservedOps :: [String]
reservedOps =
  ["|>",
   "+",
   "-",
   "*",
   "/",
   "%",
   "!=",
   "==",
   "<<",
   ">>",
   "<-"]

fclDef :: Monad m => Token.GenLanguageDef String u m
fclDef =
  Token.LanguageDef {
      Token.commentStart     = "(*"
    , Token.commentEnd       = "*)"
    , Token.commentLine      = "--"
    , Token.nestedComments   = True
    , Token.identStart       = letter <|> char '#'
    , Token.identLetter      = alphaNum <|> char '_' <|> char '#' <|> char '\''
    , Token.opStart          = oneOf ""
    , Token.opLetter         = oneOf ""
    , Token.reservedOpNames  = reservedOps
    , Token.reservedNames    = reservedNames
    , Token.caseSensitive    = True
  }

lexer :: Monad m => Token.GenTokenParser String u m
lexer = Token.makeTokenParser fclDef

lexeme  :: Monad m => ParsecT String u m a -> ParsecT String u m a
lexeme = Token.lexeme lexer

whitespace :: Monad m => ParsecT String u m ()
whitespace = Token.whiteSpace lexer

identifier :: Monad m => ParsecT String u m String
identifier = Token.identifier lexer

reserved :: Monad m => String -> ParsecT String u m ()
reserved = Token.reserved lexer

reservedOp :: Monad m => String -> ParsecT String u m ()
reservedOp = Token.reservedOp lexer

symbol :: Monad m => String -> ParsecT String u m String
symbol = Token.symbol lexer

stringlit :: Monad m => ParsecT String u m String
stringlit = Token.stringLiteral lexer

charlit :: Monad m => ParsecT String u m Char
charlit = Token.charLiteral lexer

semi :: Monad m => ParsecT String u m String
semi = Token.semi lexer

comma :: Monad m => ParsecT String u m String
comma = Token.comma lexer

colon :: Monad m => ParsecT String u m String
colon = Token.colon lexer

natural :: Monad m => ParsecT String u m Integer
natural = Token.natural lexer

float :: Monad m => ParsecT String u m Double
float = Token.float lexer

parens :: Monad m => ParsecT String u m a -> ParsecT String u m a
parens = Token.parens lexer

brackets :: Monad m => ParsecT String u m a -> ParsecT String u m a
brackets = Token.brackets lexer

angles :: Monad m => ParsecT String u m a -> ParsecT String u m a
angles = Token.angles lexer

braces :: Monad m => ParsecT String u m a -> ParsecT String u m a
braces = Token.braces lexer
