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
  let s       = "dot" </> takeFileName out -<.> "dot"
  let figtype = drop 1 $ takeExtension out
  liftIO $ putStrLn s
  need [s]
  Stdout o <- cmd cmdStr ["-T" <> figtype, s]
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
  where s = "gnuplot" </> takeFileName out -<.> "gp"

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = "_build" } $ do
  let dotfigs = fmap
        (("doc" </>) . ("figs" </>))
        [ "cloud-aerosol.pdf"
        , "mutilated-cloud-aerosol.pdf"
        , "forcing-graph.pdf"
        , "reconstruction.pdf"
        , "no-temporal.pdf"
        , "observe-everything.pdf"
        ]

  let figs = fmap
        (("doc" </>) . ("figs" </>))
        [ "naiveCloudSunlight.pdf"
        , "aerosolSunlight.pdf"
        , "generic-graph.pdf"
        , "bidirected.pdf"
        ]

  want ["doc/causality.pdf"]

  "doc/causality.pdf" %> \out -> do
    let b = out -<.> "bbl"
    let s = out -<.> "tex"
    let d = takeDirectory out
    need ([b, s, d </> "def.tex"] <> dotfigs <> figs)
    Stdout o <- cmd (Cwd d)
                    "pdflatex"
                    ["--synctex=1", dropDirectory1 (b -<.> "tex")]
    if isInfixOf "Rerun to get citations correct." o
      then cmd_ (Cwd d)
                "pdflatex"
                ["--synctex=1", dropDirectory1 (b -<.> "tex")]
      else return ()

  "doc/causality.bbl" %> \out -> do
    aux <- doesFileExist (out -<.> "aux")
    let s = out -<.> "tex"
    let d = takeDirectory out
    if not aux
      then need ([s, d </> "def.tex"] <> dotfigs <> figs)
        >> cmd_ (Cwd d) "pdflatex" (dropDirectory1 (out -<.> "tex"))
      else return ()
    need (fmap (d </>) ["references.bib", "def.tex"])
    cmd_ (Cwd d) "bibtex" (dropDirectory1 out -<.> "")

  "//*generic-graph.pdf" %> \out -> do
    need ["src/GraphDiagrams.hs"]
    putInfo ("# GraphDiagrams for " <> out)
    liftIO $ GraphDiagrams.genericGraph out

  "//*bidirected.pdf" %> \out -> do
    need ["src/GraphDiagrams.hs"]
    putInfo ("# GraphDiagrams for " <> out)
    liftIO $ GraphDiagrams.bidirectedArrow out

  ["//*naiveCloudSunlight.pdf", "//*aerosolSunlight.pdf"]
    &%> \[naive, joint] -> do
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
          liftIO
            $ putStrLn (printf "Estimated difference: %5.2f W/m^22" estDiff)
          generateFigGp
            joint
            (printf
              "Effect of cloud on sunlight as estimated from data: %5.2f W/m^2 (true effect: %3.0f W/m^2)"
              estDiff
              trueDiff
            )

  mapM_ genDot dotfigs

  phony "clean" $ do
    liftIO $ putStrLn "Cleaning files in _build and dat"
    removeFilesAfter "_build" ["//*"]
    removeFilesAfter "dat"    ["//*"]
