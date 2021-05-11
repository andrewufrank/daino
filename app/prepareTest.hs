---------------------------------------------------------------------
--
-- Module      :   a shake to produce the files for the debuging and testing
--------------------------------------------------------------
-- {-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports#-}
{-# OPTIONS -fno-warn-unused-matches#-}

module Main where

-- import Uniform.Convenience.StartApp (startProg)

import Lib.CmdLineArgs
import Lib.Foundation
import ShakeBake.ConvertFiles
import ShakeBake.ReadSettingFile
import ShakeBake.Shake2
import Uniform.Shake
import Uniform2.Filetypes4sites
import UniformBase

programName, progTitle :: Text
programName = "ssgPrepareTest" :: Text
progTitle = "preparing the data files necessary for testing 0.0.4.3" :: Text

main :: IO ()
main =
    startProg
        programName
        -- progTitle
        ( do
            let flags = True -- the debug flag
                sitefn :: Path Abs File
                sitefn = makeAbsFile "/home/frank/Workspace11/ssg/docs/site/dough/settings2"
            -- checkProcess flags sitefn
            shakeTesting True sitefn
            return ()
        )

-- test_shake :: IO ()
-- test_shake =  do
--                 runErrorVoid $ shakeTesting  layoutDefaults allFlags
--                 return ()

-- bake errors are reported

shakeTesting :: Bool -> Path Abs File -> ErrIO ()
-- -- start the testing by executing the tests and building teh
-- -- intermediate results
shakeTesting debug setting2 = do
    (layout, port2) <- readSettings debug setting2

    let flags = zero :: PubFlags
        -- doughP = doughDir layout -- the regular dough
        templatesP = themeDir layout `addFileName` templatesDirName
        testP = testDir layout
    -- staticD = testD </>"static"
    -- where all the static files go
    -- --  setCurrentDir (doughDir layout)
    -- --  fs <- getDirectoryDirs' . toFilePath $ testP
    -- --  putIOwords ["shakeTesting", "to delete", showT fs]
    -- --  mapM_ deleteDirRecursive fs
    callIO $ shakeTestWrapped debug flags layout templatesP testP
    return ()

mdTestFiles :: [FilePath]
mdTestFiles =
    [ "/home/frank/Workspace11/ssg/docs/site/dough/Blog/blog1.md"
    , "/home/frank/Workspace11/ssg/docs/site/dough/Blog/index.md"
    , "/home/frank/Workspace11/ssg/docs/site/dough/Blog/postwk.md"
    ]

shakeTestWrapped :: Bool -> PubFlags -> SiteLayout -> Path Abs Dir -> Path Abs Dir -> IO ()
shakeTestWrapped debug flags layout templatesP testP =
    shakeArgs
        shakeOptions
            { shakeFiles = toFilePath testP
            , shakeVerbosity = Verbose -- Loud
            , shakeLint = Just LintBasic
            --                , shakeRebuild=[(RebuildNow,"allMarkdownConversion")]
            --                  seems not to produce an effect
            }
        $ do
            let doughP = doughDir layout -- the regular dough
                bakedP = bakedDir layout
            let doughD = toFilePath doughP
                templatesD = toFilePath templatesP
                testD = toFilePath testP
            let masterSettings = doughP </> makeRelFile "settings2.yaml" :: Path Abs File
                masterTemplate = templatesP </> makeRelFile "master4.dtpl" :: Path Abs File

            want ["mdFiles"]

            phony "mdFiless" $ do
                needP . map makeAbsFile $ mdTestFiles
            let debug2 = True

            (toFilePath bakedP <> "**/*.docrep") %> \out -> -- insert pdfFIles1  -- here start with doughP
                md22docrep debug doughP bakedP flags layout out

            (toFilePath bakedP <> "**/*.panrep") %> \out -> -- insert pdfFIles1
                convertAny debug2 bakedP bakedP flags layout out convDocrep2panrep

            (toFilePath bakedP <> "**/*.html") %> \out -> -- from Panrep
            -- calls the copy html and the conversion from md
                panrep22html debug doughP bakedP flags layout out
            return ()

md22docrep debug doughP bakedP flags layout out = do
    bibs <- getNeeds debug doughP bakedP "bib" "bib"
    needP bibs
    csls <- getNeeds debug doughP bakedP "csl" "csl"
    needP csls
    putIOwords ["rule **/*.docrep need", showT bibs]
    putIOwords ["rule **/*.docrep need", showT csls]

    convertAny debug doughP bakedP flags layout out convMD2docrep

panrep22html debug doughP bakedP flags layout out = do
    csss <- getNeeds debug doughP bakedP "css" "css"
    putIOwords ["rule **/*.html need", showT csss]
    imgs <- getNeeds debug doughP bakedP "jpg" "jpg"
    imgs2 <- getNeeds debug doughP bakedP "JPG" "JPG"
    needP imgs
    needP imgs2
    putIOwords ["rule **/*.html need", showT imgs, showT imgs2]
    convertAny debug bakedP bakedP flags layout out convPanrep2html

-----------------------------------------OLD --------------------
--         mdFiles1 ::[ Path Rel File] <- getDirectoryFilesP doughP ["**/*.md", "**/*.markdown"]
--         let mdFiles3 =  map removeExtension mdFiles1
--         liftIO $ putIOwords ["\nshakeWrapped - markdown and md files to work on\n"
--                         , showT mdFiles3]
-- --            ["landingPage","Blog/postTufteStyled","Blog/postwk","Blog/postwk2"
-- --            ,"Blog/postwkTufte","PublicationList/postWithReference"]

--         needP [testP </> md <.> makeExtension "tripleDoc" | md <- mdFiles3]  -- markdownToPandoc
--         needP [testP </> md <.> makeExtension "pandocBiblio" | md <- mdFiles3]  -- markdownToPandoc
--         needP [testP </> md <.> makeExtension "content.docval" | md <- mdFiles3]  -- pandocToContentHtml
--         needP [testP </> md <.> makeExtension "allyaml.docval" | md <- mdFiles3]  -- pandocToContentHtml
--         needP [testP </> md <.> makeExtension "inTemplate.html" | md <- mdFiles3]  -- applyTemplate3
--         needP [testP </> md $-<.> "a.html" | md <- mdFiles1]

-- -- in order of bakeOneFile :

--     (toFilePath testP <> "//*.tripleDoc") %> \out -> do
--         --        liftIO $ putIOwords ["\n.withSettings.pandoc", s2t out]
--                 let outP = makeAbsFile out :: Path Abs File
--                 let source = doughP </> makeRelativeP testP  (outP $--<.> "md")
--                 needP [source]
--                 runErr2action $
--                         do
--         --                    intext <- read8 (makeAbsFile source) markdownFileType
--         --                    let resourcesPath = doughP `addDir` resourcesDirName :: Path Abs Dir
--                             triple <- getTripleDoc layout source
--                             -- p <- markdownToPandocBiblio True allFlags doughP triple
--                             -- case mp of
--                             --     Nothing -> return ()
--                             writeFile2 ( outP) (showT triple)

--     (toFilePath testP <> "//*.pandocBiblio") %> \out -> do
--         --        liftIO $ putIOwords ["\n.withSettings.pandoc", s2t out]
--                 let outP = makeAbsFile out :: Path Abs File
--                 let source = --testP </> (makeRelativeP testP
--                                 (outP $--<.> "tripleDoc")
--                 needP [source]
--                 runErr2action $
--                         do
--                         --    intext <- read8 (makeAbsFile source) markdownFileType
--         --                    let resourcesPath = doughP `addDir` resourcesDirName :: Path Abs Dir
--                             tripleText <- readFile2 source
--                             let triple = readNote "readTriple 42345" tripleText :: TripleDoc
--                             p <- markdownToPandocBiblio True allFlags doughP triple
--                             -- case mp of
--                             --     Nothing -> return ()
--                             writeFile2 ( outP) (showT p)

--     (toFilePath testP <> "//*content.docval") %> \out -> do
--         let outP = makeAbsFile out :: Path Abs File
--         let source = outP  $--<.>   "tripleDoc"
--         needP [source]
--         runErr2action $   -- pandocToContentHtml
--             do
--                 triple  <- readFile2 ( source)
--                 let pandoc = fst3 (readNote "we23" triple :: TripleDoc)
--                 p :: HTMLout <- pandocToContentHtml True pandoc

--                 write8 ( outP) htmloutFileType p

--     (toFilePath testP <> "//*allyaml.docval") %> \out -> do
--         let outP = makeAbsFile out :: Path Abs File
--         let source = outP $--<.>   "content.docval"
--         -- let source2 = doughP </> makeRelativeP testP (outP $--<.> "md")
--         let triple = outP $--<.> "tripleDoc"
--         needP [source]
--         -- does not track the settingss and the pageN.yaml
--         runErr2action $   -- docValToAllVal
--             do
--                 valText :: HTMLout  <-   read8 ( source )
--                                                 htmloutFileType
--                 metarecText :: String <- readFile2 triple
--                 let metarec = readNote "read metarec 23243ou" metarecText :: MetaRec
--                 p :: DocValue <- docValToAllVal debug layout flags valText
--                                   metarec
--                 write8 ( outP) docValueFileType p

-- -- apply val to template
--     (toFilePath testP <> "//*inTemplate.html") %> \out -> do --    apply   template to values
--         let outP = makeAbsFile out :: Path Abs File
--         let source = outP $--<.>   "allyaml.docval"
--         needP [source]
--         runErr2action $   -- applyTemplate3
--             do
--                 valText :: DocValue  <-   read8 ( source )
--                                                 docValueFileType
--                 p :: HTMLout <- putValinMaster False valText ( templatesP)
--                 write8 ( outP) htmloutFileType p

--     (toFilePath testP <> "//*.a.html") %> \out -> do
--         let outP = makeAbsFile out :: Path Abs File
--         let  mdSource1 =  outP $-<.> ""
--              mdSource2 = doughP </> makeRelativeP testP  (mdSource1 $-<.> "md")
--         needP [mdSource2, masterSettings, masterTemplate]
--         runErr2action $ do
--                     r <- bakeOneFile2html False flags ( mdSource2)
--                             layout
--                             ( outP)
--                     return ()

-- instance Exception Text

-- runErr2action :: Show a => ErrIO a -> Action ()
-- runErr2action op = liftIO $ do
--     res <- runErr  op
--     case res of
--         Left msg -> throw msg
--         Right _ -> do
-- --                        putIOwords ["runErr2action", "got return", showT a] --
--                         return ()

-- ($--<.>) :: Path a File  -> Text  -> Path a File
-- -- take away two extensions and replace with a new oneLine
-- f $--<.> ext =  (f $-<.> zero) $-<.> ext
-- imported from Uniform.Shake
