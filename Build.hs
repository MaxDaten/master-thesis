import Development.Shake
import Development.Shake.FilePath
import Control.Monad ( void )

targetName, presentationTarget, buildDir, chapterDir :: FilePath
outType, masterFile, bibCmd, buildChapter :: FilePath
targetName         = "master-thesis"
presentationTarget = "2015-06-02-presentation"
buildDir           = "_build"
chapterDir         = "chapter"
outType            = "pdf"
masterFile         = "main.tex"
bibCmd             = "biber"
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
    texs <- getDirectoryFiles "" ["chapter/*.tex", "bib/*.tex", "tex/*.tex", "master.tex"]
    srcs <- getDirectoryFiles "" ["/src/*.hs"]
    need texs
    need srcs
    need [buildChapter]

    -- idiomatic not possible due whitespacing
    -- cmd "pdflatex -output-format=pdf -output-directory=" [ buildDir ] "-jobname=" [ targetName ] "main.tex"
    let target = "-jobname=" ++ targetName
        outDir = "-output-directory=" ++ buildDir
        outFmt = "-output-format=" ++ outType
        pdflatexCmd = cmd "pdflatex" [outDir, outFmt, target, masterFile]

    -- move the bib into the build directory
    command_ [] "cp" [ "-r", "bib", buildDir]
    void pdflatexCmd
    need [out -<.> "bcf"]
    void pdflatexCmd
    pdflatexCmd

  -- build the bibliography
  buildDir </> "*.bcf" *> \out ->
    command [ Cwd buildDir ] bibCmd [ dropDirectory1 out ]

  -- the pdflatex bug workaround
  buildChapter *> \out ->
    cmd "mkdir -p" [ out ]

  phony "view" $ do
    need [ buildDir </> targetName <.> outType ]
    cmd "open" [ buildDir</>targetName<.>outType ]


  phony "presentation" $ do
    putNormal "building presentation"
    need [ buildDir </> presentationTarget <.> "pdf" ]

  buildDir </> presentationTarget <.> "pdf" *> \_out -> do
    texs <- getDirectoryFiles "" [presentationTarget </> "*.tex"]
    need texs

    let target = "-jobname=" ++ presentationTarget
        outDir = "-output-directory=" ++ buildDir
        outFmt = "-output-format=" ++ "pdf"
        pdflatexCmd = cmd "pdflatex" [outDir, outFmt, target, presentationTarget </> "presentation-master.tex"]

    void pdflatexCmd
    pdflatexCmd
{-
    "_build//*.o" *> \out -> do
        let c = dropDirectory1 $ out -<.> "c"
        let m = out -<.> "m"
        () <- cmd "gcc -c" [c] "-o" [out] "-MMD -MF" [m]
        needMakefileDependencies m
-}
