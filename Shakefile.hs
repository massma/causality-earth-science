{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util
import qualified Data.ByteString               as BS
import           Data.List                      ( isInfixOf )
import           Text.Printf
import qualified CloudSunlight
import qualified GnuplotParser


genGraphvis :: FilePath -> FilePath -> Rules ()
genGraphvis cmdStr pat = pat %> \out -> do
  let s = out -<.> "dot"
  liftIO $ putStrLn s
  need [s]
  Stdout o <- cmd cmdStr ["-Tpdf", s]
  liftIO $ BS.writeFile out o

genDot :: FilePath -> Rules ()
genDot = genGraphvis "dot"

genCirco :: FilePath -> Rules ()
genCirco = genGraphvis "circo"

generateFigGp
  :: FilePath -> ([GnuplotParser.Line] -> [GnuplotParser.Line]) -> Action ()
generateFigGp out pp = do
  need [s]
  gp <-
    pp . (GnuplotParser.header out <>) . GnuplotParser.parseGp <$> readFile' s
  cmd_ (Stdin (unlines (fmap show gp))) "gnuplot"
  where s = out -<.> "gp"

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = "_build" } $ do
  let dotfigs =
        [ "cloud-aerosol.pdf"
        , "mutilated-cloud-aerosol.pdf"
        , "generic-graph.pdf"
        , "forcing-graph.pdf"
        ]
  let circofigs = ["bidirected.pdf"]

  let figs = ["lightcone.pdf", "naiveCloudSunlight.pdf", "aerosolSunlight.pdf"]

  want ["causality.pdf"]

  "causality.pdf" %> \out -> do
    let b = out -<.> "bbl"
    let s = out -<.> "tex"
    need ([b, s, "def.tex"] <> dotfigs <> circofigs <> figs)
    Stdout o <- cmd "pdflatex" (b -<.> "tex")
    if isInfixOf "Rerun to get citations correct." o
      then cmd_ "pdflatex" (b -<.> "tex")
      else return ()

  "causality.bbl" %> \out -> do
    aux <- doesFileExist (out -<.> "aux")
    if not aux
      then need (dotfigs <> figs) >> cmd_ "pdflatex" (out -<.> "tex")
      else return ()
    need ["references.bib", "def.tex"]
    cmd_ "bibtex" $ out -<.> ""

  "lightcone.pdf" %> \out -> do
    need ["stack.yaml"]
    cmd_ "stack" ["exec", "spacetime-cause-exe", "--", out]


  ["naiveCloudSunlight.pdf", "aerosolSunlight.pdf"] &%> \[naive, joint] -> do
    need ["src/CloudSunlight.hs", "src/GnuplotParser.hs"]
    (estDiff, naiveDiff) <- liftIO $ CloudSunlight.cloudSunlightExperiment 1000
    generateFigGp
      naive
      (GnuplotParser.setTitle
        (printf "Average difference: %5.2f W/m^2" naiveDiff)
      )
    liftIO $ putStrLn (printf "Estimated difference: %5.2f W/m2" estDiff)
    generateFigGp joint id

  mapM_ genDot   dotfigs

  mapM_ genCirco circofigs

  phony "clean" $ do
    liftIO $ putStrLn "Cleaning files in _build"
    removeFilesAfter "_build" ["//*"]
