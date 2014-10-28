import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

targetName = "master-thesis"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
    want ["_build" </> targetName <.> "pdf"]

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

    "_build" </> targetName <.> "pdf" *> \out -> do
        texs <- getDirectoryFiles "" ["//*.tex"]
        need texs
        cmd "pdflatex --output-directory=_build -jobname=" [out] texs

{-
    "_build//*.o" *> \out -> do
        let c = dropDirectory1 $ out -<.> "c"
        let m = out -<.> "m"
        () <- cmd "gcc -c" [c] "-o" [out] "-MMD -MF" [m]
        needMakefileDependencies m
-}
