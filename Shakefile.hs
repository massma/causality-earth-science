{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

import qualified CloudSunlight
import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import qualified GnuplotParser
import qualified GraphDiagrams
import qualified System.Directory as D
import Text.Printf

genGraphvis :: FilePath -> FilePath -> Rules ()
genGraphvis cmdStr pat =
  pat %> \out -> do
    let s = "dot" </> takeFileName out -<.> "dot"
    let figtype = drop 1 $ takeExtension out
    liftIO $ putStrLn s
    need [s]
    if figtype == "pdf"
      then do
        Stdout o <- cmd cmdStr ["-Tps2", s]
        cmd_ (StdinBS o) "ps2pdf" ["-", out]
      else do
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
      . (GnuplotParser.header out <>)
      . GnuplotParser.parseGp
      <$> readFile' s
  cmd_ (Stdin (unlines (fmap show gp))) "gnuplot"
  where
    s = "gnuplot" </> takeFileName out -<.> "gp"

figPath :: FilePath -> FilePath
figPath = ("doc" </>) . ("figs" </>)

main :: IO ()
main = shakeArgs shakeOptions {shakeFiles = "_build"} $ do
  let dotFigs =
        fmap
          (figPath . (<.> "pdf"))
          [ "forcing-graph",
            "ccope",
            "observe-everything",
            "reconstruction",
            "cloud-aerosol"
          ]

  let figs =
        fmap (figPath . (<.> "pdf")) ["naiveCloudSunlight", "aerosolSunlight"]

  let texFigs =
        fmap
          figPath
          [ "generic-graph.tex"
          ]

  want ["doc/causality.pdf"]

  "doc/causality.pdf" %> \out -> do
    let b = out -<.> "bbl"
    let s = out -<.> "tex"
    let d = takeDirectory out
    need ([b, s, d </> "def.tex"] <> dotFigs <> figs <> texFigs)
    Stdout o <-
      cmd
        (Cwd d)
        "pdflatex"
        ["--synctex=1", "-interaction=nonstopmode", dropDirectory1 (b -<.> "tex")]
    if isInfixOf "Rerun to get citations correct." o
      then
        cmd_
          (Cwd d)
          "pdflatex"
          [ "--synctex=1",
            "-interaction=nonstopmode",
            dropDirectory1 (b -<.> "tex")
          ]
      else return ()

  ["//*/cloud-aerosol.tex", "//*/mutilated-cloud-aerosol.tex"] &%> \[c, mc] ->
    do
      need ["src/GraphDiagrams.hs"]
      liftIO $ putStrLn (printf "# GraphDiagrams.cloudAeorosl for %s, %s" c mc)
      liftIO $ GraphDiagrams.cloudAerosol c mc

  "doc/causality.bbl" %> \out -> do
    aux <- doesFileExist (out -<.> "aux")
    let s = out -<.> "tex"
    let d = takeDirectory out
    if not aux
      then
        need ([s, d </> "def.tex"] <> dotFigs <> figs)
          >> cmd_
            (Cwd d)
            "pdflatex"
            [ "--synctex=1",
              "-interaction=nonstopmode",
              dropDirectory1 (out -<.> "tex")
            ]
      else return ()
    need (fmap (d </>) ["references.bib", "def.tex"])
    cmd_ (Cwd d) "bibtex" (dropDirectory1 out -<.> "")

  "//*generic-graph*" %> \out -> do
    need ["src/GraphDiagrams.hs"]
    liftIO $ putStrLn (printf "# GraphDiagrams.genericGraph for %s" out)
    liftIO $ GraphDiagrams.genericGraphs out

  ["//*naiveCloudSunlight.pdf", "//*aerosolSunlight.pdf"]
    &%> \[naive, joint] -> do
      need ["src/CloudSunlight.hs", "src/GnuplotParser.hs", "Shakefile.hs"]
      liftIO $ D.createDirectoryIfMissing True "dat"
      (estDiff, naiveDiff, trueDiff) <-
        liftIO $
          CloudSunlight.cloudSunlightExperiment 2000
      generateFigGp
        naive
        ( printf
            "Average sunlight difference: %5.2f W/m^2 (true effect of cloud on sunlight: %3.0f W/m^2)"
            naiveDiff
            trueDiff
        )
      liftIO $
        putStrLn (printf "Estimated difference: %5.2f W/m^22" estDiff)
      generateFigGp
        joint
        ( printf
            "Effect of cloud on sunlight as estimated from data: %5.2f W/m^2 (true effect: %3.0f W/m^2)"
            estDiff
            trueDiff
        )

  mapM_ genDot dotFigs

  phony "clean" $ do
    liftIO $ putStrLn "Cleaning files in _build, dat, and doc/figs"
    removeFilesAfter "_build" ["//*"]
    removeFilesAfter "dat" ["//*"]
    removeFilesAfter ("doc" </> "figs") ["//*.pdf"]
