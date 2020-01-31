import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util
import qualified Data.ByteString               as BS
import           Data.List                      ( isInfixOf )

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
        , "forcing-graph.pdf"
        ]
  let figs = ["lightcone.pdf"]
  let srcFiles = fmap
        ("spacetime-causality" </>)
        [ "spacetime-cause.cabal"
        , "app" </> "Main.hs"
        , "src" </> "Display.hs"
        , "src" </> "Lib.hs"
        ]

  phony "clean" $ do
    liftIO $ putStrLn "Cleaning files in _build"
    removeFilesAfter "_build" ["//*"]

  want ["causality.pdf"]

  "causality.pdf" %> \out -> do
    let b = out -<.> "bbl"
    let s = out -<.> "tex"
    need ([b, s, "def.tex"] <> dotfigs <> figs)
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
    need srcFiles
    cmd_ (Cwd "spacetime-causality") "stack" ["build"]
    cmd_ (Cwd "spacetime-causality")
         "stack"
         ["exec", "spacetime-cause-exe", "--", ".." </> out]

  ("spacetime-causality" </> "spacetime-cause.cabal") %> \out -> do
    cmd_ Shell "git" ["submodule", "init"]
    cmd_ Shell "git" ["submodule", "update"]

  mapM_ genDot dotfigs
