{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

module GnuplotParser
  ( parseGnu
  , setTitle
  )
where

import qualified Text.ParserCombinators.ReadP  as P

data Line = S Set | P Plot | Generic String deriving (Show)

data Set = Output FilePath String
         | Title String String deriving (Show)

data Plot = Plot FilePath String deriving (Show)

ws :: String -> String -> String
ws a b = a <> " " <> b

quote :: String -> String
quote s = "\"" <> s <> "\""

-- instance Show Line where
--   show (S       s) = "set" `ws` show s
--   show (P       p) = "plot" `ws` show p
--   show (Generic s) = s

-- instance Show Set where
--   show (Output f s) = "output" `ws` quote f `ws` s
--   show (Title  t s) = "title" `ws` quote t `ws` s

-- instance Show Plot where
--   show (Plot fp r) = quote fp `ws` r

gnuFile :: P.ReadP [Line]
gnuFile = P.manyTill p P.eof
  where p = gnuLine >>= \o -> P.char '\n' >> return o

gnuLine :: P.ReadP Line
gnuLine = (S <$> gnuSet) P.<++ (P <$> gnuPlot) P.<++ unImplemented

gnuSet :: P.ReadP Set
gnuSet = P.string "set" >> P.many (P.char ' ') >> gnuOutput P.<++ gnuTitle

gnuTwoItem :: (String -> String -> b) -> String -> P.ReadP b
gnuTwoItem constructor str = do
  _    <- P.string str
  _    <- P.munch (== ' ')
  _    <- P.char '"'
  used <- P.munch (/= '"')
  _    <- P.char '"'
  rest <- P.munch (/= '\n')
  return (constructor used rest)

gnuOutput :: P.ReadP Set
gnuOutput = gnuTwoItem Output "output"

gnuTitle :: P.ReadP Set
gnuTitle = gnuTwoItem Title "title"

gnuPlot :: P.ReadP Plot
gnuPlot = gnuTwoItem Plot "plot"

unImplemented :: P.ReadP Line
unImplemented = Generic <$> P.munch (/= '\n')

parseGnu :: String -> [Line]
parseGnu s = case P.readP_to_S gnuFile s of
  x : _xs -> fst x
  _       -> error ("failed to parse gnufile:\n" <> s)

setTitle :: String -> [Line] -> [Line]
setTitle str ls = go ls
 where
  go []                    = error "no match on plot"
  go (S (Title _s r) : xs) = S (Title str r) : xs
  go (P p            : xs) = S (Title str "") : (P p) : xs
  go (x              : xs) = x : go xs

setInput :: String -> [Line] -> [Line]
setInput str ls = go ls
 where
  go []                   = P (Plot str "") : []
  go (P (Plot _p r) : xs) = P (Plot str r) : xs
  go (x             : xs) = x : go xs

-- testing
testStr =
  "set term pdfcairo enhanced mono font \"Arial,12\" size 3,2\nset output \"naiveCloudSunlight.pdf\"\nset style boxplot sorted nooutliers\nset style data boxplot\nset border 2\nunset key\nset xtics nomirror scale 0.0\nset ylabel \"Surface Solar Radiation (W m^{-1})\"\nset ytics nomirror\nplot \"dat/cloudSunlight.csv\" using (0.0):\"sunlight\":(0.5):\"cloud\"\n"

-- test =
--   (reverse . dropWhile (== '\n') . reverse . unlines $ fmap show
--                                                             (parseGnu testStr)
--     )
--     == testStr
-- -- >>> Test
-- -- >>> True
