{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

module GnuplotParser
  ()
where

import qualified Text.ParserCombinators.ReadP  as P

data Line = S Set | P Plot | Generic String

data Set = Output FilePath String
         | Title String String

data Plot = Plot { dat :: FilePath
                 , suffix :: String
                 }

ws :: String -> String -> String
ws a b = a <> " " <> b

quote s = "\"" <> s <> "\""

instance Show Line where
  show (S       s) = "set" `ws` show s
  show (P       p) = "plot" `ws` show p
  show (Generic s) = s

instance Show Set where
  show (Output f s) = "output" `ws` quote f `ws` s
  show (Title  t s) = "title" `ws` t `ws` s

instance Show Plot where
  show p = quote (dat p) `ws` suffix p

gnuFile :: P.ReadP [Line]
gnuFile = P.sepBy1 gnuLine (P.char '\n')

gnuLine :: P.ReadP Line
gnuLine = (S <$> gnuSet) P.<++ (P <$> gnuPlot) P.<++ unImplemented

gnuSet :: P.ReadP Set
gnuSet = P.string "set" >> P.many (P.char ' ') >> gnuOutput P.<++ gnuTitle


gnuOutput = do
  _     <- P.string "output"
  _     <- P.munch (== ' ')
  _     <- P.char '"'
  fname <- P.munch (not . (== '"'))
  _     <- P.char '"'
  rest  <- P.munch (const True)
  return (Output fname rest)

gnuTitle = do
  _     <- P.string "title"
  _     <- P.munch (== ' ')
  _     <- P.char '"'
  title <- P.munch (not . (== '"'))
  _     <- P.char '"'
  rest  <- P.munch (const True)
  return (Title title rest)

gnuPlot = do
  _     <- P.string "plot"
  _     <- P.munch (== ' ')
  _     <- P.char '"'
  fname <- P.munch (not . (== '"'))
  _     <- P.char '"'
  rest  <- P.munch (const True)
  return (Plot fname rest)

unImplemented = Generic <$> P.munch (const True)

parseGnu s = case P.readP_to_S gnuFile s of
  x : _xs -> fst x
  _       -> error ("failed to parse gnufile:\n" <> s)


-- testing
testStr =
  "set term pdfcairo enhanced mono font \"Arial,12\" size 3,2\nset output \"naiveCloudSunlight.pdf\"\nset style boxplot sorted nooutliers\nset style data boxplot\nset border 2\nunset key\nset xtics nomirror scale 0.0\nset ylabel \"Surface Solar Radiation (W m^{-1})\"\nset ytics nomirror\nplot \"dat/cloudSunlight.csv\" using (0.0):\"sunlight\":(0.5):\"cloud\""

test =
  (reverse . dropWhile (== '\n') . reverse . unlines $ fmap show
                                                            (parseGnu testStr)
    )
    == testStr
-- >>> Test
-- >>> True
