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
    .   (GnuplotParser.header out <>)
    .   GnuplotParser.parseGp
    <$> readFile' s
  cmd_ (Stdin (unlines (fmap show gp))) "gnuplot"
  where s = "gnuplot" </> takeFileName out -<.> "gp"

figPath :: FilePath -> FilePath
figPath = ("doc" </>) . ("figs" </>)

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = "_build" } $ do
  let dotFigs = fmap
        (figPath . (<.> "pdf"))
        [ "cloud-aerosol"
        , "mutilated-cloud-aerosol"
        , "forcing-graph"
        , "reconstruction"
        , "no-temporal"
        , "observe-everything"
        , "unobserved-aerosol"
        ]

  let figs = fmap (figPath . (<.> "pdf"))
                  ["naiveCloudSunlight", "aerosolSunlight", "generic-graph"]

  let texFigs = fmap figPath ["cloud-aerosol.tex"]
  want ["doc/causality.pdf"]

  "doc/causality.pdf" %> \out -> do
    let b = out -<.> "bbl"
    let s = out -<.> "tex"
    let d = takeDirectory out
    need ([b, s, d </> "def.tex"] <> dotFigs <> figs <> texFigs)
    Stdout o <- cmd
      (Cwd d)
      "pdflatex"
      ["--synctex=1", "-interaction=nonstopmode", dropDirectory1 (b -<.> "tex")]
    if isInfixOf "Rerun to get citations correct." o
      then cmd_
        (Cwd d)
        "pdflatex"
        [ "--synctex=1"
        , "-interaction=nonstopmode"
        , dropDirectory1 (b -<.> "tex")
        ]
      else return ()

  "//*cloud-aerosol.tex" %> \out -> do
    let c = figPath . (<.> "pdf") $ "cloud-aerosol"
    let u = figPath . (<.> "pdf") $ "unobserved-aerosol"
    need [c, u, "src/GraphDiagrams.hs"]
    putInfo ("# GraphDiagrams for " <> out)
    liftIO $ GraphDiagrams.cloudAerosol "doc" c u out

  "doc/causality.bbl" %> \out -> do
    aux <- doesFileExist (out -<.> "aux")
    let s = out -<.> "tex"
    let d = takeDirectory out
    if not aux
      then need ([s, d </> "def.tex"] <> dotFigs <> figs) >> cmd_
        (Cwd d)
        "pdflatex"
        [ "--synctex=1"
        , "-interaction=nonstopmode"
        , dropDirectory1 (out -<.> "tex")
        ]
      else return ()
    need (fmap (d </>) ["references.bib", "def.tex"])
    cmd_ (Cwd d) "bibtex" (dropDirectory1 out -<.> "")

  "//*generic-graph.pdf" %> \out -> do
    need ["src/GraphDiagrams.hs"]
    putInfo ("# GraphDiagrams for " <> out)
    liftIO $ GraphDiagrams.genericGraph out


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

  mapM_ genDot dotFigs

  phony "clean" $ do
    liftIO $ putStrLn "Cleaning files in _build, dat, and doc/figs"
    removeFilesAfter "_build"           ["//*"]
    removeFilesAfter "dat"              ["//*"]
    removeFilesAfter ("doc" </> "figs") ["//*.pdf"]
