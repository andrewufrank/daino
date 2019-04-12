-----------------------------------------------------------------------------
--
-- Module      :   a shake to produce the files for the debuging and testing
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports#-}

module ShakeStartTests
     where


-- import Development.Shake
-- import Development.Shake.FilePath
import Lib.Bake
import Lib.Foundation

import Lib.Foundation_test (testLayout) -- (progName, SiteLayout (..), layoutDefaults, templatesDirName)
import Lib.Pandoc
-- import Lib.FileMgt
import Lib.Templating (putValinMaster )
import Test.Framework 
         -- (markdownToPandoc, pandocToContentHtml,docValToAllVal)
        -- with a simplified Action ~ ErrIO
-- import Text.Pandoc  (Pandoc)
import Uniform.Error
import Uniform.FileIO            hiding ((<.>), (</>)) -- (resourcesDirName)
import Uniform.Pandoc -- (applyTemplate3, Pandoc, DocValue, doc HTMLout, htmloutFileType)
-- import Uniform.Shake
import Lib.CmdLineArgs (allFlags, PubFlags)
import Lib.CheckInput (checkOneMdFile, MetaRec(..), TripleDoc(..))
import Uniform.Pointless (fst3)

test_shake :: IO ()
test_shake =  do
                runErrorVoid $ shakeTesting  layoutDefaults allFlags
                return ()

-- bake errors are reported

shakeTesting :: SiteLayout -> PubFlags -> ErrIO ()
-- start the testing by executing the tests and building teh
-- intermediate results
shakeTesting layout flags = do
  let
      doughP      =    doughDir  layout  -- the regular dough
      templatesP =   themeDir  layout `addFileName` templatesDirName
      testP =   testDir layout
    --              staticD = testD </>"static"  -- where all the static files go
--  setCurrentDir (doughDir layout)
--  fs <- getDirectoryDirs' . toFilePath $ testP
--  putIOwords ["shakeTesting", "to delete", showT fs]
--  mapM_ deleteDirRecursive fs
  callIO $ shakeTestWrapped flags layout doughP  templatesP testP


shakeTestWrapped :: PubFlags -> SiteLayout ->  Path Abs Dir  -> Path Abs Dir  -> Path Abs Dir ->  IO  ()
shakeTestWrapped flags layout doughP  templatesP testP =
    shakeArgs shakeOptions {shakeFiles= toFilePath testP
            , shakeVerbosity=Chatty -- Loud
            , shakeLint=Just LintBasic
    --                , shakeRebuild=[(RebuildNow,"allMarkdownConversion")]
    --                  seems not to produce an effect
                    } $ do
    -- let doughD = toFilePath doughP
    --     templatesD = toFilePath templatesP
    --     testD = toFilePath testP
    let
        masterSettings = doughP </> makeRelFile "settings2.yaml" :: Path Abs File 
        masterTemplate =  templatesP </> makeRelFile "master4.dtpl"  :: Path Abs File 

    want ["allTests"]

    phony "allTests" $ do
        needP [masterSettings, masterTemplate]

        mdFiles1 ::[ Path Rel File] <- getDirectoryFilesP doughP ["**/*.md", "**/*.markdown"] 
        let mdFiles3 =  map removeExtension mdFiles1
        liftIO $ putIOwords ["\nshakeWrapped - markdown and md files to work on\n"
                        , showT mdFiles3]
--            ["landingPage","Blog/postTufteStyled","Blog/postwk","Blog/postwk2"
--            ,"Blog/postwkTufte","PublicationList/postWithReference"]



        needP [testP </> md <.> makeExtension "tripleDoc" | md <- mdFiles3]  -- markdownToPandoc
        needP [testP </> md <.> makeExtension "pandocBiblio" | md <- mdFiles3]  -- markdownToPandoc
        needP [testP </> md <.> makeExtension "content.docval" | md <- mdFiles3]  -- pandocToContentHtml
        needP [testP </> md <.> makeExtension "allyaml.docval" | md <- mdFiles3]  -- pandocToContentHtml
        needP [testP </> md <.> makeExtension "inTemplate.html" | md <- mdFiles3]  -- applyTemplate3
        needP [testP </> md $-<.> "a.html" | md <- mdFiles1]


-- in order of bakeOneFile :

    (toFilePath testP <> "//*.tripleDoc") %> \out -> do
        --        liftIO $ putIOwords ["\n.withSettings.pandoc", s2t out]
                let outP = makeAbsFile out :: Path Abs File
                let source = doughP </> makeRelativeP testP  (outP $--<.> "md")
                needP [source]
                runErr2action $
                        do
        --                    intext <- read8 (makeAbsFile source) markdownFileType
        --                    let resourcesPath = doughP `addDir` resourcesDirName :: Path Abs Dir
                            triple <- checkOneMdFile layout source 
                            -- p <- markdownToPandocBiblio True allFlags doughP triple 
                            -- case mp of
                            --     Nothing -> return ()
                            writeFile2 ( outP) (showT triple)

    (toFilePath testP <> "//*.pandocBiblio") %> \out -> do
        --        liftIO $ putIOwords ["\n.withSettings.pandoc", s2t out]
                let outP = makeAbsFile out :: Path Abs File
                let source = --testP </> (makeRelativeP testP  
                                (outP $--<.> "tripleDoc")
                needP [source]
                runErr2action $
                        do
                        --    intext <- read8 (makeAbsFile source) markdownFileType
        --                    let resourcesPath = doughP `addDir` resourcesDirName :: Path Abs Dir
                            tripleText <- readFile2 source 
                            let triple = readNote "readTriple 42345" tripleText :: TripleDoc 
                            p <- markdownToPandocBiblio True allFlags doughP triple 
                            -- case mp of
                            --     Nothing -> return ()
                            writeFile2 ( outP) (showT p)
                                
    (toFilePath testP <> "//*content.docval") %> \out -> do
        let outP = makeAbsFile out :: Path Abs File
        let source = outP  $--<.>   "tripleDoc"
        needP [source]
        runErr2action $   -- pandocToContentHtml
            do
                triple  <- readFile2 ( source)
                let pandoc = fst3 (readNote "we23" triple :: TripleDoc)
                p :: HTMLout <- pandocToContentHtml True pandoc
                                
                write8 ( outP) htmloutFileType p

    (toFilePath testP <> "//*allyaml.docval") %> \out -> do
        let outP = makeAbsFile out :: Path Abs File
        let source = outP $--<.>   "content.docval"
        let source2 = doughP </> makeRelativeP testP (outP $--<.> "md")
        let triple = outP $--<.> "tripleDoc"
        needP [source]
        -- does not track the settingss and the pageN.yaml
        runErr2action $   -- docValToAllVal
            do
                valText :: HTMLout  <-   read8 ( source )
                                                htmloutFileType
                metarecText :: String <- readFile2 triple 
                let metarec = readNote "read metarec 23243ou" metarecText :: MetaRec
                p :: DocValue <- docValToAllVal True layout flags valText
                                 ( source2) metarec 
                write8 ( outP) docValueFileType p


-- apply val to template
    (toFilePath testP <> "//*inTemplate.html") %> \out -> do --    apply   template to values
        let outP = makeAbsFile out :: Path Abs File
        let source = outP $--<.>   "allyaml.docval"
        needP [source]
        runErr2action $   -- applyTemplate3
            do
                valText :: DocValue  <-   read8 ( source )
                                                docValueFileType
                p :: HTMLout <- putValinMaster False valText ( templatesP)
                write8 ( outP) htmloutFileType p

    (toFilePath testP <> "//*.a.html") %> \out -> do
        let outP = makeAbsFile out :: Path Abs File
        let  mdSource1 =  outP $-<.> ""
             mdSource2 = doughP </> makeRelativeP testP  (mdSource1 $-<.> "md")
        needP [mdSource2, masterSettings, masterTemplate]
        runErr2action $ do 
                    r <- bakeOneFile False flags ( mdSource2)
                            layout
                            ( outP)
                    return ()



instance Exception Text

-- runErr2action :: Show a => ErrIO a -> Action ()
-- runErr2action op = liftIO $ do
--     res <- runErr  op
--     case res of
--         Left msg -> throw msg
--         Right _ -> do
-- --                        putIOwords ["runErr2action", "got return", showT a] --
--                         return ()

($--<.>) :: Path a File  -> Text  -> Path a File
-- take away two extensions and replace with a new oneLine
f $--<.> ext =  (f $-<.> zero) $-<.> ext
