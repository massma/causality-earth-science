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
  let figs =
        [ "cloud-aerosol.pdf"
        , "mutilated-cloud-aerosol.pdf"
        , "generic-graph.pdf"
        ]
  want ["causality.pdf"]

  "causality.pdf" %> \out -> do
    let b = out -<.> "bbl"
    let s = out -<.> "tex"
    need ([b, s] <> figs)
    cmd_ "pdflatex" s

  "causality.bbl" %> \out -> do
    need ["references.bib"]
    cmd_ "bibtex" $ out -<.> ""

  mapM_ genDot figs
