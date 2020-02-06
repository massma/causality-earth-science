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
import qualified CloudSunlight

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

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = "_build" } $ do
  let dotfigs =
        [ "cloud-aerosol.pdf"
        , "mutilated-cloud-aerosol.pdf"
        , "generic-graph.pdf"
        , "forcing-graph.pdf"
        ]
  let circofigs = ["bidirected.pdf"]

  let figs      = ["lightcone.pdf"]

  let cloudSunlight =
        fmap ("dat" </>) ["cloudSunlight.csv", "cloud.csv", "noCloud.csv"]

  want (["causality.pdf"] <> cloudSunlight)

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

  cloudSunlight &%> \fpaths -> do
    need ["src/CloudSunlight.hs"]
    liftIO $ CloudSunlight.writeData 1000 fpaths

  mapM_ genDot   dotfigs

  mapM_ genCirco circofigs

  phony "clean" $ do
    liftIO $ putStrLn "Cleaning files in _build"
    removeFilesAfter "_build" ["//*"]
