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
  ( genericGraphs
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
node w = circle (0.5 * w) -- # pad 1.1

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

sizeBoundedLabel w l =
  hboxSurf latexSurface l # center <> hSpace w 1.0 # center

diagramUnits :: Width -> Double -> Double
diagramUnits w x = x * w * 1.5

timeSlice :: Width -> ([Width -> DGram], Double) -> ([Double], DGram)
timeSlice w (fs, t) = case fs of
  [] -> ([], mempty)
  _  -> (fmap fst namedFs, foldr1 fr (fmap (\(n, f) -> f w # named n) namedFs))
 where
  fr d' d = d' ||| hSpace w 0.5 ||| d
  namedFs = zipWith (\i d -> (10 * t + i, d)) [0 ..] fs

displayDiagram :: FilePath -> DGram -> IO ()
displayDiagram fpath = renderPGF' fpath def

(||||) :: DGram -> DGram -> DGram
(||||) d1 d2 = beside (r2 (-1, 0)) d2 d1

diagramStateLayOut
  :: [Double] -> Width -> [[Width -> DGram]] -> [String] -> DGram
diagramStateLayOut vSpaces w fss labels = snd
  $ foldr fr ([], mempty) (zip (zip labels vSpaces) namedDs)
 where
  namedDs = fmap (timeSlice w) (zip fss [0 ..])
  fr (_s, (n1s, d')) ([], _) = (n1s, d' # center)
  fr ((str, vSpc), (n1s, d')) (n2s, d) =
    ( n1s
    , (foldr (.) id ((\n1 n2 -> connectOutside n1 n2) <$> n1s <*> n2s))
      ((sizeBoundedLabel w str |||| (d' # center)) === vSpace w vSpc === d)
    )

diagramState :: Width -> [[Width -> DGram]] -> [String] -> DGram
diagramState w fss = diagramStateLayOut (fmap (const 0.5) fss) w fss

--
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

addLabel :: Double -> Double -> String -> DGram -> DGram
addLabel wFactor hFactor lab d =
  hboxPoint ("\\textbf{" <> lab <> "}")
    #  translateY (hFactor * height d)
    #  translateX (wFactor * width d)
    <> (alignBL d)

cloudAerosol :: FilePath -> FilePath -> IO ()
cloudAerosol cloudAPath mutPath =
  displayDiagram
      cloudAPath
      (addLabel' "A)" obs === addLabel' "B)" pearl === addLabel' "C)" unObs)
    >> displayDiagram mutPath mutilated
 where
  addLabel'    = addLabel 0.0 0.75
  w            = nodeWidth "sunlight (S)"
  dashConfig   = dashingN [0.01, 0.01] 0.01
  (nC, nS, nA) = ((0 :: Int), (2 :: Int), (4 :: Int))
  dashed       = (with & shaftStyle %~ dashConfig)
  shaft'       = arc xDir (-1 / 5 @@ turn)
  doubleDashed = dashed & arrowTail .~ spike' & arrowShaft .~ shaft'
  c            = observed w "cloud (C)" # named nC
  s            = observed w "sunlight (S)" # named nS
  a            = observed w "aerosol (A)" # named nA
  pearl = (c ||| hSpace w 2.0 ||| s) # connectOutside nC nS # connectPerim'
    doubleDashed
    nC
    nS
    (1 / 6 @@ turn)
    (1 / 2 - 1 / 6 @@ turn) -- (1 / 3 @@ turn)
  mutilated =
    (a === (c ||| hSpace w 2.0 ||| s) # center)
      # connectOutside nA nC
      # connectOutside nA nS
  obs = mutilated # connectOutside nC nS
  unObs =
    (a # dashConfig === (c ||| hSpace w 2.0 ||| s) # center)
      # connectOutside nC nS
      # connectOutside' dashed nA nC
      # connectOutside' dashed nA nS

genericGraphs :: FilePath -> IO ()
genericGraphs fpath = displayDiagram
  fpath
  (full # alignB ||| reconstructed # alignB ||| noTemporal # alignB)
 where
  addLabelW = addLabel 0.25 0.1
  addLabel' = addLabel (0.25 * 3.0 / 2.0) 0.1
  observed' name w' = observed w' name # center
  w = nodeWidth "E(t+1)"
  o w' = observed w' ""
  unO w' = unObserved w' ""
  full = addLabel' "A)" $ diagramState
    w
    [[o, unO], [unO, unO], [o, unO], [observed' "E(t+1)"]]
    ["S(t-1)", "S(t-1/2)", "S(t)", ""]
  reconstructed = addLabelW "B)" $ vSpace w 3 === diagramState
    w
    [[observed' "S(t)"], [observed' "E(t+1)"]]
    ["", ""]
  noTemporal = addLabel' "C)" $ diagramStateLayOut
    [2.0, 0.5, 0.5]
    w
    [ [observed' "S(t-1)"]
    , [observed' "S'(t)", observed' "C(t)"]
    , [observed' "E(t+1)"]
    ]
    ["", "", ""]
