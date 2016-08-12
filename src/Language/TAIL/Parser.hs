module Language.TAIL.Parser
  (parseFile,
   parseString)
where

import Control.Monad (liftM, liftM2)
import Text.Parsec hiding (Empty)
import Text.Parsec.String

import Language.TAIL.Lexer
import Language.TAIL.Syntax

parseString :: String -> String -> (Exp Untyped)
parseString programText filename =
  case parse program filename programText of
    Left e  -> error (show e)
    Right r -> r

parseFile :: String -> IO (Exp Untyped)
parseFile filename =
  do contents <- readFile filename
     case parse program filename contents of
       Left e  -> error (show e)
       Right r -> return r

program :: Parser (Exp Untyped)
program =
  do whitespace
     prog <- expr
     eof
     return prog

-----------------
-- Expression

expr :: Parser (Exp Untyped)
expr = fnExpr
   <|> opExpr
   <|> arrayExpr
   <|> letExpr
   <|> valueExpr
   <?> "expression"

valueExpr :: Parser (Exp Untyped)
valueExpr = try (liftM doubleV (lexeme float))
         <|> liftM (intV . fromInteger) (lexeme natural)
         <|> liftM charV charlit
         <|> boolExpr
         <|> infExpr
         <|> (oneOf "~-" >> liftM (UnaryOp NegateInt) valueExpr) -- TODO: negation of doubles must be performed by "negd"
         <|> liftM2 Var identifier (return Untyped)
         <?> "value or identifier"

boolExpr :: Parser (Exp Untyped)
boolExpr = try (reserved "tt" >> return true)
       <|> try (reserved "ff" >> return false)
       <?> "boolean value"

infExpr :: Parser (Exp Untyped)
infExpr = try (reserved "inf" >> return (Infinity Untyped))

arrayExpr :: Parser (Exp Untyped)
arrayExpr = liftM2 Vector (brackets (sepBy expr comma)) (return Untyped)

letExpr :: Parser (Exp Untyped)
letExpr =
  do reserved "let"
     (ident, _) <- typedIdent
     _ <- symbol "="
     e1 <- expr
     reserved "in"
     e2 <- expr
     return (Let ident e1 e2 Untyped)

opExpr :: Parser (Exp Untyped)
opExpr =
  do ident <- try $ do i <- identifier
                       _ <- lookAhead (oneOf "({")
                       return i
     args <- parens (sepBy expr comma)
     return (switch ident args)
  where
    switch "addi" [e1,e2] = BinOp AddI e1 e2
    switch "subi" [e1,e2] = BinOp SubI e1 e2
    switch "muli" [e1,e2] = BinOp MulI e1 e2
    switch "divi" [e1,e2] = BinOp DivI e1 e2
    switch "maxi" [e1,e2] = BinOp MaxI e1 e2
    switch "mini" [e1,e2] = BinOp MinI e1 e2
    switch "eqi"  [e1,e2] = BinOp EqI e1 e2
    switch "lti"  [e1,e2] = BinOp LtI e1 e2
    switch "gti"  [e1,e2] = BinOp GtI e1 e2

    switch "addd" [e1,e2] = BinOp AddD e1 e2
    switch "subd" [e1,e2] = BinOp SubD e1 e2
    switch "muld" [e1,e2] = BinOp MulD e1 e2
    switch "divd" [e1,e2] = BinOp DivD e1 e2
    switch "maxd" [e1,e2] = BinOp MaxD e1 e2
    switch "mind" [e1,e2] = BinOp MinD e1 e2
    switch "eqd"  [e1,e2] = BinOp EqD e1 e2
    switch "ltd"  [e1,e2] = BinOp LtD e1 e2
    switch "gtd"  [e1,e2] = BinOp GtD e1 e2
    switch "powd" [e1,e2] = BinOp PowD e1 e2

    switch "ln"    [e1] = UnaryOp Ln e1
    switch "negd"  [e1] = UnaryOp NegateDouble e1
    switch "signd" [e1] = UnaryOp SignDouble e1
    switch "expd"  [e1] = UnaryOp Exp e1
    switch "absd"  [e1] = UnaryOp AbsoluteDouble e1
    switch "absi"  [e1] = UnaryOp AbsoluteInt e1
    switch "i2d"   [e1] = UnaryOp I2D e1
    switch "b2i"   [e1] = UnaryOp B2I e1
    -- eh what? What's the difference between b2iV and b2i?
    switch "b2iV"  [e1] = UnaryOp B2I e1


    switch "zipWith" [f,e1,e2] = ZipWith f e1 e2 Untyped
    switch "foldl"   [f,e1]    = Foldl f e1 Untyped
    switch "reduce"  [f,e1,e2] = Reduce f e1 e2 Untyped
    switch "scan"    [f,e1]    = Scan f e1 Untyped
    switch "reshape" [e1,e2]   = Reshape e1 e2 Untyped
    switch "transp"  [e1]      = Transpose e1 Untyped
    switch "transp2" [e1,e2]   = Transpose2 e1 e2 Untyped
    switch "rav"     [e1]      = Ravel e1 Untyped

    switch "each"   [f,e1]  = Each f e1 Untyped
    switch "take"   [e1,e2] = Take e1 e2 Untyped
    switch "drop"   [e1,e2] = Drop e1 e2 Untyped
    switch "cat"    [e1,e2] = Catenate e1 e2 Untyped
    switch "iota"   [e1]    = Iota e1 Untyped
    switch "shape"  [e1]    = Shape e1 Untyped
    switch "cons"   [e1,e2] = Cons e1 e2 Untyped
    switch "snoc"   [e1,e2] = Snoc e1 e2 Untyped
    switch "rotate" [e1,e2] = Rotate e1 e2 Untyped

    switch "eachV"   [f,e1]  = EachV f e1 Untyped
    switch "takeV"   [e1,e2] = TakeV e1 e2 Untyped
    switch "dropV"   [e1,e2] = DropV e1 e2 Untyped
    switch "catV"    [e1,e2] = CatenateV e1 e2 Untyped
    switch "iotaV"   [e1]    = IotaV e1 Untyped
    switch "shapeV"  [e1]    = ShapeV e1 Untyped
    switch "consV"   [e1,e2] = ConsV e1 e2 Untyped
    switch "snocV"   [e1,e2] = SnocV e1 e2 Untyped
    switch "rotateV" [e1,e2] = RotateV e1 e2 Untyped

    switch n _ = error ("Operator not supported: " ++ n)

fnExpr :: Parser (Exp Untyped)
fnExpr =
  do reserved "fn"
     (ident, typ) <- typedIdent
     _ <- symbol "=>"
     e <- expr
     return (Lam ident typ e Untyped)

typedIdent :: Parser (Name, Type)
typedIdent =
  do ident <- identifier
     _ <- colon
     typ <- typeExpr
     return (ident, typ)

------------------
-- Types

typeExpr :: Parser Type
typeExpr = liftM (foldr1 (:>)) $
  sepBy1 (     arrayType
           <|> pVectorType
           <|> singleElemType
           <|> singletonType
           <?> "type" )
         ( symbol "->" )

arrayType :: Parser Type
arrayType = liftM2 ArrayT (brackets basicType) rank

pVectorType :: Parser Type
pVectorType = liftM2 VectorT (angles basicType) rank

singletonType :: Parser Type
singletonType =
  do _ <- symbol "S"
     parens $ do
       typ <- basicType
       _ <- comma
       val <- rank
       return (SingleT typ val)

singleElemType :: Parser Type
singleElemType =
  do _ <- try (symbol "SV")
     parens $ do
       typ <- basicType
       _ <- comma
       val <- rank
       return (SingleVecT typ val)

rank :: Parser Rank
rank = liftM (Rank . fromInteger) (lexeme natural)
   -- <|> (liftM Rv identifier)  Unsupported
   <?> "rank"

basicType :: Parser BaseType
basicType = (reserved "int" >> return IntT)
        <|> (reserved "double" >> return DoubleT)
        <|> (reserved "bool" >> return BoolT)
        <|> (reserved "char" >> return CharT)
        <?> "basic type"
