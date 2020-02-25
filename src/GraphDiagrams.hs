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
type Width = Double

masterText :: String
masterText = "sunlight (S)"

nodeWidth :: String -> Double
nodeWidth txt = width $ ((hboxSurf latexSurface txt :: DGram) # pad 1.3)

node :: Width -> DGram
node w = circle (0.5 * w) # pad 1.1

hSpace :: Width -> Double -> DGram
hSpace w f = rect (w * f) w # lcA transparent

vSpace :: Width -> Double -> DGram
vSpace w f = rect w (w * f) # lcA transparent

observed :: Width -> String -> DGram
observed w label = boundedLabel label # centerXY <> node w

unObserved :: Width -> String -> DGram
unObserved w label =
  boundedLabel label # centerXY <> node w # dashingN [0.01, 0.01] 0.01
-- common default of pixel per inch is 150, so 5 x 3 in = 750 x 450 pixel

-- | from: http://ipdfdev.com/2016/07/06/what-resolution-pdf-files/ ;
-- maybe cairo point (1 inch = 72 points) = pixel
-- renderSize :: Num n => SizeSpec V2 n
-- renderSize = dims $ r2 (5 * 72, 3 * 72)

boundedLabel :: String -> DGram
boundedLabel l = hboxSurf latexSurface l

diagramUnits :: Width -> Double -> Double
diagramUnits w x = x * w * 1.5

timeSlice
  :: Width
  -> (Width -> String -> DGram, Width -> String -> DGram, Double)
  -> ([Double], DGram)
timeSlice w (f1, f2, t) =
  ( dnames
  , atPoints ps [f1 w "" # named (dnames !! 0), f2 w "" # named (dnames !! 1)]
  )
 where
  vLoc = negate t
  ps = fmap (\hLoc -> P (r2 (diagramUnits w hLoc, diagramUnits w vLoc))) [0, 1]
  dnames = [vLoc * 2, vLoc * 2 + 1]

displayDiagram :: FilePath -> DGram -> IO ()
displayDiagram fpath = renderPGF' fpath def

diagramState
  :: Width
  -> [Width -> String -> DGram]
  -> [Width -> String -> DGram]
  -> [Double]
  -> DGram
diagramState w f1s f2s ts = namedF
  (foldr
    (<>)
    (atPoints [P (r2 (diagramUnits w 0.5, diagramUnits w vLoc))]
              [observed w "E(t+1)" # named (vNames !! 0)]
    )
    diagrams
  )
 where
  (hNames, diagrams) = unzip $ fmap (timeSlice w) (zip3 f1s f2s ts)
  vLoc               = negate (last ts + 1)
  vNames             = [vLoc * 2]
  namedF             = foldr (.) id $ zipWith
    (\n1s n2s -> foldr (.) id ((\n1 n2 -> connectOutside n1 n2) <$> n1s <*> n2s)
    )
    hNames
    (drop 1 hNames <> [vNames])

labelState :: Width -> [Double] -> DGram
labelState w ts = atPoints ps labels
 where
  ps     = fmap (\t -> P (r2 (0, diagramUnits w (negate t)))) ts
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

addLabel lab d = hboxPoint lab <> (alignTL d)

cloudAerosol :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
cloudAerosol texPath obsPath unObsPath fpath = do
  displayDiagram
    fpath
    (addLabel "A)" obs === addLabel "B)" pearl === addLabel "C)" unObs)
 where
  w            = nodeWidth "sunlight (S)"
  dashConfig   = dashingN [0.01, 0.01] 0.01
  (nC, nS, nA) = ((0 :: Int), (2 :: Int), (4 :: Int))
  shaft'       = arc xDir (-1 / 5 @@ turn)
  dashed       = (with & arrowShaft .~ shaft' & shaftStyle %~ dashConfig)
  doubleDashed = dashed & arrowTail .~ spike'
  c            = observed w "cloud (C)" # named nC
  s            = observed w "sunlight (S)" # named nS
  a            = observed w "aerosol (A)" # named nA
  pearl = (c ||| hSpace w 2.0 ||| s) # connectOutside nC nS # connectPerim'
    doubleDashed
    nC
    nS
    (2 / 12 @@ turn)
    (4 / 12 @@ turn)
  obs =
    (a === (c ||| hSpace w 2.0 ||| s) # center)
      # connectOutside nC nS
      # connectOutside nA nC
      # connectOutside nA nS
  unObs =
    (a # dashConfig === (c ||| hSpace w 2.0 ||| s) # center)
      # connectOutside nC nS
      # connectOutside' dashed nA nC
      # connectOutside' dashed nA nS

genericGraph :: FilePath -> IO ()
genericGraph fpath = displayDiagram
  fpath
  (   labelState w ts
  ||| diagramState
        w
        [observed, unObserved, observed]
        (replicate 3 unObserved)
        ts
  )
 where
  ts = [-2, -1, 0]
  w  = nodeWidth "E(t+1)"
