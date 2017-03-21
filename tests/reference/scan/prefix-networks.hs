------------------------
-- From Mary's JFP paper on prefix-sum networks
------------------------
type Fan a = [a] -> [a]

mkFan :: (a -> a -> a) -> Fan a
mkFan op (i:is) = i:[op i k | k <- is]

pplus :: Fan Int
pplus = mkFan (+)

type PP a = Fan a -> [a] -> [a]

-- example
ser3 :: PP a
ser3 f [a,b,c] =
  let [a1,b1] = f [a,b]
      [b2,c2] = f [b1,c]
  in [a1,b2,c2]

delFan :: [Int] -> [Int]
delFan [d] = [d]
delFan ds = [maximum ds + 1 | i <- ds]

type WDels = [(Int,Int)]

wdFan :: WDels -> WDels
wdFan [wd] = [wd]
wdFan wds = [(w,maximum ds + 1) | w <- ws]
  where (ws,ds) = unzip wds

zdel :: Int -> WDels
zdel n = [(i,0) | i <- [1..n]]
