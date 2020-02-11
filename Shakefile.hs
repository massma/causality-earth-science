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
import qualified System.Directory              as D
import           Text.Printf
import qualified CloudSunlight
import qualified GnuplotParser
import qualified GraphDiagrams

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

generateFigGp :: FilePath -> String -> Action ()
generateFigGp out title = do
  need [s]
  gp <-
    GnuplotParser.setTitle title
    .   (GnuplotParser.header out <>)
    .   GnuplotParser.parseGp
    <$> readFile' s
  cmd_ (Stdin (unlines (fmap show gp))) "gnuplot"
  where s = out -<.> "gp"

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = "_build" } $ do
  let dotfigs =
        [ "cloud-aerosol.pdf"
        , "mutilated-cloud-aerosol.pdf"
        , "forcing-graph.pdf"
        ]
  let circofigs = ["bidirected.pdf"]

  let figs =
        ["naiveCloudSunlight.pdf", "aerosolSunlight.pdf", "generic-graph.pdf"]

  want ["causality.pdf"]

  "causality.pdf" %> \out -> do
    let b = out -<.> "bbl"
    let s = out -<.> "tex"
    need ([b, s, "def.tex"] <> dotfigs <> circofigs <> figs)
    Stdout o <- cmd "pdflatex" (b -<.> "tex")
    if isInfixOf "Rerun to get citations correct." o
      then cmd_ "pdflatex" ["--synctex=1", (b -<.> "tex")]
      else return ()

  "causality.bbl" %> \out -> do
    aux <- doesFileExist (out -<.> "aux")
    if not aux
      then need (dotfigs <> figs) >> cmd_ "pdflatex" (out -<.> "tex")
      else return ()
    need ["references.bib", "def.tex"]
    cmd_ "bibtex" $ out -<.> ""

  "generic-graph.pdf" %> \out -> do
    need ["src/GraphDiagrams.hs"]
    putInfo ("# GraphDiagrams for " <> out)
    liftIO $ GraphDiagrams.genericGraph out

  ["naiveCloudSunlight.pdf", "aerosolSunlight.pdf"] &%> \[naive, joint] -> do
    need ["src/CloudSunlight.hs", "src/GnuplotParser.hs", "Shakefile.hs"]
    liftIO $ D.createDirectoryIfMissing True "dat"
    (estDiff, naiveDiff, trueDiff) <- liftIO
      $ CloudSunlight.cloudSunlightExperiment 1000
    generateFigGp
      naive
      (printf
        "Average sunlight difference: %5.2f W/m^2 (true effect of cloud on sunlight: %3.0f W/m^2)"
        naiveDiff
        trueDiff
      )
    liftIO $ putStrLn (printf "Estimated difference: %5.2f W/m^22" estDiff)
    generateFigGp
      joint
      (printf
        "Effect of cloud on sunlight as estimated from data: %5.2f W/m^2 (true effect: %3.0f W/m^2)"
        estDiff
        trueDiff
      )

  mapM_ genDot   dotfigs

  mapM_ genCirco circofigs

  phony "clean" $ do
    liftIO $ putStrLn "Cleaning files in _build and dat"
    removeFilesAfter "_build" ["//*"]
    removeFilesAfter "dat"    ["//*"]
