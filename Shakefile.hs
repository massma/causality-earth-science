import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util
import qualified Data.ByteString               as BS

genDot :: FilePath -> Rules ()
genDot pat = pat %> \out -> do
  let s = out -<.> "dot"
  liftIO $ putStrLn s
  need [s]
  Stdout o <- cmd "dot" ["-Tpdf", s]
  liftIO $ BS.writeFile out o

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = "_build" } $ do
  let dotfigs =
        [ "cloud-aerosol.pdf"
        , "mutilated-cloud-aerosol.pdf"
        , "generic-graph.pdf"
        ]
  let figs = ["lightcone.pdf"]

  want ["causality.pdf"]

  "causality.pdf" %> \out -> do
    let b = out -<.> "bbl"
    let s = out -<.> "tex"
    need ([b, s] <> dotfigs <> figs)
    cmd_ "pdflatex" s

  "causality.bbl" %> \out -> do
    need ["references.bib"]
    cmd_ "bibtex" $ out -<.> ""

  "lightcone.pdf" %> \out -> do
    need ["spacetime-causality/spacetime-cause.cabal"]
    cmd_ (Cwd "spacetime-causality") "stack" ["exec", "spacetime-cause-exe"]

  ("spacetime-causality" </> "spacetime-cause.cabal") %> \out -> do
    cmd_ "git" ["submodule", "init"]
    cmd_ "git" ["submodule", "update"]

  mapM_ genDot dotfigs
