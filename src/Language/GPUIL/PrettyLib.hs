------------------------------
-- Pretty printing combinators
-- ---------------------------
--
-- Yes, I could have used something else, like language-c-quote or
-- on of the pretty-printing packages. This works for now.

module Language.GPUIL.PrettyLib where

data Doc = Text String
         | Doc :+: Doc
         | Doc :<>: Doc
         | Indent Doc
         | Newline
         | Par Doc

render :: Int -> Int -> Doc -> String
render startIndent indentWidth r =
  let mkIndent :: Int -> String
      mkIndent n = replicate (n*indentWidth) ' '
      
      loop :: Int -> [String] -> Doc -> [String]
      loop _ a (Text s) = s : a
      loop n a Newline = ('\n' : mkIndent n) : a
      loop n a (Indent d) = loop (n+1) a d
      loop n a (r1 :+: r2) = loop n (loop n a r1) r2
      loop n a (r1 :<>: r2) = loop n (" " : (loop n a r1)) r2
      loop n a (Par e) = loop n a (char '(' :+: e :+: char ')')
  in concat (mkIndent startIndent : reverse (loop startIndent [] r))
     

indent :: Doc -> Doc
indent = Indent

parens :: Doc -> Doc
parens (Par d) = Par d
parens d = Par d

unpar :: Doc -> Doc
unpar (Par d) = d
unpar d = d

brackets :: Doc -> Doc
brackets d = char '[' :+: d :+: char ']'

text :: String -> Doc
text = Text

char :: Char -> Doc
char c = Text [c]

int :: (Show a, Integral a) => a -> Doc
int x = Text (show (fromIntegral x :: Integer))

double :: Double -> Doc
double = Text . show

cat :: [Doc] -> Doc
cat [] = text ""
cat ls = foldl1 (:+:) ls

hsep :: [Doc] -> Doc
hsep = foldl1 (:<>:)

sep :: Doc -> [Doc] -> Doc
sep _ [] = text ""
sep seperator ls = foldl1 f ls
 where
   f :: Doc -> Doc -> Doc
   f d0 d1 = d0 :+: seperator :<>: d1
