import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Control.Monad ( void )

targetName   = "master-thesis"
buildDir     = "_build"
chapterDir   = "chapter"
outType      = "pdf"
masterFile   = "main.tex"
-- for output redirection and workaround for a pdflatex bug
-- https://bugs.launchpad.net/ubuntu/+source/texlive-base/+bug/1132843
buildChapter = buildDir </> chapterDir

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=buildDir</>""} $ do
    want [ buildDir </> targetName <.> outType ]

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter buildDir ["//*"]

    buildDir </> targetName <.> outType *> \out -> do
        texs <- getDirectoryFiles "" ["//*.tex"]
        need texs
        need [buildChapter]
        -- idiomatic not possible due whitespacing
        -- cmd "pdflatex -output-format=pdf -output-directory=" [ buildDir ] "-jobname=" [ targetName ] "main.tex"
        let target = "-jobname=" ++ targetName
            outDir = "-output-directory=" ++ buildDir
            outFmt = "-output-format=" ++ outType
            pdflatexCmd = cmd "pdflatex" [outDir, outFmt, target, masterFile]

        void pdflatexCmd
        need [out -<.> "bbl"]
        void pdflatexCmd
        pdflatexCmd

    buildDir </> "*.bbl" *> \out -> do
        command_ [] "cp" [ "-r", "bib", buildDir]
        command [ Cwd buildDir ] "bibtex" [ dropDirectory1 out -<.> "aux" ]

    -- the pdflatex bug workaround
    buildChapter *> \out ->
        cmd "mkdir -p" [ out ]


{-
    "_build//*.o" *> \out -> do
        let c = dropDirectory1 $ out -<.> "c"
        let m = out -<.> "m"
        () <- cmd "gcc -c" [c] "-o" [out] "-MMD -MF" [m]
        needMakefileDependencies m
-}
