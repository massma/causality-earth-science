{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

module GraphDiagrams
  ( genericGraph
  , cloudAerosol
  )
where


import           Diagrams.Prelude
import           Diagrams.Backend.PGF
import qualified Data.Attoparsec.ByteString.Char8
                                               as P
import qualified Data.ByteString               as BS
import           Data.Either
import           Development.Shake.FilePath

type DGram = QDiagram B V2 Double Any

masterText :: String
masterText = "sunlight (S)"

nodeWidth :: Double
nodeWidth = width $ ((hboxSurf latexSurface masterText :: DGram) # pad 1.3)
node :: DGram
node = circle (0.5 * nodeWidth) # pad 1.1

observed :: String -> DGram
observed label = hboxSurf latexSurface label # centerXY <> node

unObserved :: String -> DGram
unObserved label =
  hboxSurf latexSurface label # centerXY <> node # dashingN [0.01, 0.01] 0.01
-- common default of pixel per inch is 150, so 5 x 3 in = 750 x 450 pixel

-- | from: http://ipdfdev.com/2016/07/06/what-resolution-pdf-files/ ;
-- maybe cairo point (1 inch = 72 points) = pixel
-- renderSize :: Num n => SizeSpec V2 n
-- renderSize = dims $ r2 (5 * 72, 3 * 72)

boundedLabel :: String -> DGram
boundedLabel l = hboxSurf latexSurface l

diagramUnits :: Double -> Double
diagramUnits x = x * nodeWidth * 1.5

timeSlice :: (String -> DGram, String -> DGram, Double) -> ([Double], DGram)
timeSlice (f1, f2, t) =
  ( dnames
  , atPoints ps [f1 "" # named (dnames !! 0), f2 "" # named (dnames !! 1)]
  )
 where
  vLoc   = negate t
  ps     = fmap (\hLoc -> P (r2 (diagramUnits hLoc, diagramUnits vLoc))) [0, 1]
  dnames = [vLoc * 2, vLoc * 2 + 1]

displayDiagram :: FilePath -> DGram -> IO ()
displayDiagram fpath = renderPGF' fpath def

diagramState :: [String -> DGram] -> [String -> DGram] -> [Double] -> DGram
diagramState f1s f2s ts = namedF
  (foldr
    (<>)
    (atPoints [P (r2 (diagramUnits 0.5, diagramUnits vLoc))]
              [observed "E(t+1)" # named (vNames !! 0)]
    )
    diagrams
  )
 where
  (hNames, diagrams) = unzip $ fmap timeSlice (zip3 f1s f2s ts)
  vLoc               = negate (last ts + 1)
  vNames             = [vLoc * 2]
  namedF             = foldr (.) id $ zipWith
    (\n1s n2s -> foldr (.) id ((\n1 n2 -> connectOutside n1 n2) <$> n1s <*> n2s)
    )
    hNames
    (drop 1 hNames <> [vNames])



labelState :: [Double] -> DGram
labelState ts = atPoints ps labels
 where
  ps     = fmap (\t -> P (r2 (0, diagramUnits (negate t)))) ts
  labels = fmap
    (\t -> boundedLabel $ case t of
      x | x == 0  -> "S(t)"
        | x == -1 -> "S(t-1/2)"
        | x == -2 -> "S(t-1)"
      _ -> error "unimplemented label state"
    )
    ts

parsePDFDims :: BS.ByteString -> Either String (Double, Double)
parsePDFDims str = P.parseOnly dimensions str
 where
  dimensions :: P.Parser (Double, Double)
  dimensions = do
    _  <- P.manyTill P.anyChar (P.string "<</Type/Page/MediaBox [")
    x0 <- intSpace
    y0 <- intSpace
    x1 <- intSpace
    y1 <- realToFrac <$> P.scientific
    return (x1 - x0, y1 - y0)
  intSpace :: P.Parser Double
  intSpace = do
    i <- realToFrac <$> P.scientific
    P.skipSpace
    return i

testStr = -- "<</Type/Page/MediaBox [0 0 176 224]"
  "endobj\n6 0 obj\n632\nendobj\n4 0 obj\n<</Type/Page/MediaBox [0 0 176 224]\n/Rotate 0/Parent 3 0 R\n/Resources<</ProcSet[/PDF /Text]\n/ExtGState 10 0 R\n"

addLabel lab d = hboxSurf latexSurface lab # alignTL <> (alignTL d)

cloudAerosol :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
cloudAerosol texPath obsPath unObsPath fpath = do
  obsImg   <- loadImg obsPath
  unObsImg <- loadImg unObsPath
  displayDiagram
    fpath
    (addLabel "A)" (image obsImg) ||| addLabel "B)" d ||| addLabel
      "C)"
      (image unObsImg)
    )
 where
  ps       = [P (r2 (diagramUnits 0, 0)), P (r2 (diagramUnits 2, 0))]
  (n1, n2) = ((0 :: Int), (2 :: Int))
  shaft'   = arc xDir (-1 / 5 @@ turn)
  arrowStyle =
    (with & arrowShaft .~ shaft' & arrowTail .~ spike' & shaftStyle %~ dashingN
      [0.01, 0.01]
      0.01
    )
  d =
    atPoints
        ps
        [observed "cloud (C)" # named n1, observed "sunlight (S)" # named n2]
      # connectOutside n1 n2
      # connectPerim' arrowStyle n1 n2 (2 / 12 @@ turn) (4 / 12 @@ turn)
  h = height d
  loadImg p =
    (   fromRight (error ("getting dims from " <> p))
      .   parsePDFDims
      <$> BS.readFile p
      )
      >>= \(x, y) ->
            let ratio = 3 * h / y
            in  return $ uncheckedImageRef (makeRelative texPath p)
                                           (round (ratio * x))
                                           (round (ratio * y))

genericGraph :: FilePath -> IO ()
genericGraph fpath = displayDiagram
  fpath
  (   labelState ts
  ||| diagramState [observed, unObserved, observed]
                   (replicate 3 unObserved)
                   ts
  )
  where ts = [-2, -1, 0]
