{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

module GnuplotParser
  ( parseGp
  , setTitle
  , setInput
  )
where

import qualified Text.ParserCombinators.ReadP  as P

data Line = S Set | P Plot | Generic String -- deriving (Show)

data Set = Output FilePath String
         | Title String String -- deriving (Show)

data Plot = Plot FilePath String -- deriving (Show)
ws :: String -> String -> String
ws a b | b == ""   = a
       | otherwise = a <> " " <> b

quote :: String -> String
quote s = "\"" <> s <> "\""

instance Show Line where
  show (S       s) = "set" `ws` show s
  show (P       p) = "plot" `ws` show p
  show (Generic s) = s

instance Show Set where
  show (Output f s) = "output" `ws` quote f `ws` s
  show (Title  t s) = "title" `ws` quote t `ws` s

instance Show Plot where
  show (Plot fp r) = quote fp `ws` r

gpFile :: P.ReadP [Line]
gpFile = P.manyTill p P.eof where p = gpLine >>= \o -> P.char '\n' >> return o

gpLine :: P.ReadP Line
gpLine = (S <$> gpSet) P.<++ (P <$> gpPlot) P.<++ unImplemented

gpSet :: P.ReadP Set
gpSet = P.string "set" >> P.many (P.char ' ') >> gpOutput P.<++ gpTitle

gpTwoItem :: (String -> String -> b) -> String -> P.ReadP b
gpTwoItem constructor str = do
  _    <- P.string str
  _    <- P.munch (== ' ')
  _    <- P.char '"'
  used <- P.munch (/= '"')
  _    <- P.char '"'
  _    <- P.munch (== ' ')
  rest <- P.munch (/= '\n')
  return (constructor used rest)

gpOutput :: P.ReadP Set
gpOutput = gpTwoItem Output "output"

gpTitle :: P.ReadP Set
gpTitle = gpTwoItem Title "title"

gpPlot :: P.ReadP Plot
gpPlot = gpTwoItem Plot "plot"

unImplemented :: P.ReadP Line
unImplemented = Generic <$> P.munch (/= '\n')

parseGp :: String -> [Line]
parseGp s = case P.readP_to_S gpFile s of
  x : _xs -> fst x
  _       -> error ("failed to parse gpfile:\n" <> s)

setTitle :: String -> [Line] -> [Line]
setTitle str = go
 where
  go []                    = error "no match on plot"
  go (S (Title _s r) : xs) = S (Title str r) : xs
  go (P p            : xs) = S (Title str "") : P p : xs
  go (x              : xs) = x : go xs

setInput :: String -> [Line] -> [Line]
setInput str = go
 where
  go []                   = [P (Plot str "")]
  go (P (Plot _p r) : xs) = P (Plot str r) : xs
  go (x             : xs) = x : go xs

-- -- testing
-- testStr =
--   "set term pdfcairo enhanced mono font \"Arial,12\" size 3,2\nset output \"naiveCloudSunlight.pdf\"\nset style boxplot sorted nooutliers\nset style data boxplot\nset border 2\nunset key\nset xtics nomirror scale 0.0\nset ylabel \"Surface Solar Radiation (W m^{-1})\"\nset ytics nomirror\nplot \"dat/cloudSunlight.csv\" using (0.0):\"sunlight\":(0.5):\"cloud\"\n"

-- test = (unlines $ fmap show (parseGp testStr)) == testStr
-- -- >>> Test
-- -- >>> True
