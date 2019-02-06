-----------------------------------------------------------------------------
--
-- ModuKe      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports#-}

module ShakeStartTests
     where


import           Test.Framework
import Uniform.FileIO            hiding ((<.>), (</>))
import Uniform.Error
import Development.Shake
import Development.Shake.FilePath
--import Development.Shake.Util
--import System.Path (copyDir)

import Lib.Foundation -- (progName, SiteLayout (..), layoutDefaults, templatesDirName)
import Lib.Bake
import Lib.FileMgt
import Lib.Foundation_test (testLayout)
--import Lib.Foundation (templatesDirName)
import Lib.Pandoc  -- (markdownToPandoc, pandocToContentHtml,docValToAllVal)
        -- with a simplified Action ~ ErrIO
import Text.Pandoc  (Pandoc)
import Lib.Templating (applyTemplate3 )
--import qualified System.Directory as IO
--import Data.List (sort)

test_shake :: IO ()
test_shake =  do
                runErrorVoid $ shakeTesting layoutDefaults
                return ()

-- bake errors are reported

shakeTesting :: SiteLayout -> ErrIO ()
-- start the testing by executing the tests and building teh
-- intermediate results
shakeTesting layout = do
  let
      doughD      =   toFilePath . doughDir $ layout  -- the regular dough
      templatesD =   (toFilePath . themeDir $ layout) </> (toFilePath templatesDirName)
      testD = toFilePath  $  testDir layout
    --              staticD = testD </>"static"  -- where all the static files go
  setCurrentDir (doughDir layout)
  fs <- getDirectoryDirs' testD
  putIOwords ["shakeTesting", "to delete", showT fs]
  mapM_ deleteDirRecursive fs
  callIO $ shakeTestWrapped doughD templatesD testD

--getDirectoryContentsIO :: FilePath -> IO [FilePath]
---- getDirectoryContents "" is equivalent to getDirectoryContents "." on Windows,
---- but raises an error on Linux. We smooth out the difference.
--getDirectoryContentsIO dir = fmap (sort . filter (not . all (== '.')))
--        $ IO.getDirectoryContents $ if dir == "" then "." else dir
--
--
--getDirectoryDirsIO :: FilePath -> IO [FilePath]
--getDirectoryDirsIO dir = filterM f =<< getDirectoryContentsIO dir
--    where f x = IO.doesDirectoryExist $ dir </> x

shakeTestWrapped :: FilePath -> FilePath -> FilePath ->  IO  ()
shakeTestWrapped doughD templatesD testD =
    shakeArgs shakeOptions {shakeFiles= testD
            , shakeVerbosity=Chatty -- Loud
            , shakeLint=Just LintBasic
    --                , shakeRebuild=[(RebuildNow,"allMarkdownConversion")]
    --                  seems not to produce an effect
                    } $ do
      let
        masterSettings = doughD</>"settings2.yaml"
        masterTemplate = templatesD</>"master4.dtpl"
      want ["allTests"]
      phony "allTests" $ do
    --                need [staticD</>"Master3.gtpl", staticD</>"master.yaml"]
    --                -- get css
    --                cssFiles1 <- getDirectoryFiles templatesD ["*.css"] -- no subdirs
    ----                liftIO $ putIOwords ["\nshakeWrapped - phony cssFiles1", showT cssFiles1]
    --                let cssFiles = [replaceDirectory c staticD  | c <- cssFiles1]
    --                need cssFiles
        need [masterSettings, masterTemplate]

        mdFiles1 <- getDirectoryFiles doughD ["**/*.md", "**/*.markdown"]
        let mdFiles3 =  map dropExtension mdFiles1
        liftIO $ putIOwords ["\nshakeWrapped - makrdown and md files to work on\n"
                        , showT mdFiles3]
--            ["landingPage","Blog/postTufteStyled","Blog/postwk","Blog/postwk2"
--            ,"Blog/postwkTufte","PublicationList/postWithReference"]

--        -- overall test
--        let htmlFiles2 = [testD </> md <.> "html" | md <- mdFiles3]
--        liftIO $ putIOwords ["\nshakeWrapped - htmlFile", showT htmlFiles2]

    --                need [testD </> md -<.> "z.html" | md <- mdFiles1]

--        need [testD </> md <.> "withSettings.md" | md <- mdFiles3]  -- spliceMarkdown
        need [testD </> md <.> "withSettings.pandoc" | md <- mdFiles3]  -- markdownToPandoc
        need [testD </> md <.> "content.docval" | md <- mdFiles3]  -- pandocToContentHtml
        need [testD </> md <.> "allyaml.docval" | md <- mdFiles3]  -- pandocToContentHtml
--        need [testD </> md <.> "dtpl" | md <- mdFiles3]  -- spliceTemplates
        need [testD </> md <.> "inTemplate.html" | md <- mdFiles3]  -- applyTemplate3
--
--        need [testD </> md <.> "a.html" | md <- mdFiles3]  -- bakeOneFile


-- in order of bakeOneFile :

      (testD <> "//*.withSettings.pandoc") %> \out -> do
        liftIO $ putIOwords ["\n.withSettings.pandoc", s2t out]
        let source = doughD </> (makeRelative testD  (out --<.> "md"))
        need [source]
        runErr2action $
                do
                    intext <- read8 (makeAbsFile source) markdownFileType
                    p <- markdownToPandoc True intext
                    writeFile2 (makeAbsFile out) (showT p)

      (testD <> "//*content.docval") %> \out -> do
        let source = out --<.>   "withSettings.pandoc"
        need [source]
        runErr2action $   -- pandocToContentHtml
            do
                pandocText  <- readFile2 (makeAbsFile source)
                p :: DocValue <- pandocToContentHtml True
                                (readNote "we23" pandocText :: Pandoc)
                write8 (makeAbsFile out) docValueFileType p

      (testD <> "//*allyaml.docval") %> \out -> do
        let source = out --<.>   "content.docval"
        need [source]
        -- does not track the settingss and the pageN.yaml
        runErr2action $   -- docValToAllVal
            do
                valText :: DocValue  <-   read8 (makeAbsFile source )
                                                docValueFileType
                p :: DocValue <- docValToAllVal True
                                valText (makeAbsDir doughD) (makeAbsDir templatesD)
                write8 (makeAbsFile out) docValueFileType p

---- produe the template for the md
--      (testD <> "//*dtpl") %> \out -> do  -- produce the combined template
--        let source = out -<.>  "allyaml.docval"
--        need [source]
--        runErr2action $
--            do
--                putIOwords ["testD - dtpl", showT source]
--                val  <- read8 (makeAbsFile source)  docValueFileType
--                let mmt = getMaybeStringAtKey val "masterTemplate"
--        let masterfn = maybe "master4.dtpl" id mmt
--                template <- read8 (makeAbsFile $ templatesD </> masterfn) dtmplFileType
----
----                gtempl <- read8 (makeAbsFile masterTemplate) gtmplFileType
----                p :: Dtemplate <- spliceTemplates (valText :: DocValue)  (gtempl :: Gtemplate)
--                write8 (makeAbsFile out) dtmplFileType template


-- apply val to template
      (testD <> "//*inTemplate.html") %> \out -> do --    apply   template to values
        let source = out --<.>   "allyaml.docval"
--        let tpl =  out --<.>  "dtpl"
        need [source]
        runErr2action $   -- applyTemplate3
            do
                valText :: DocValue  <-   read8 (makeAbsFile source )
                                                docValueFileType
--                dtempl :: Dtemplate  <- read8 (makeAbsFile tpl ) dtmplFileType
                p :: HTMLout <- putValinMaster True valText (makeAbsDir templatesD)
                write8 (makeAbsFile out) htmloutFileType p

--      (testD <> "//*dtpl") %> \out -> do  -- produce the combined template
--        let source = out -<.>  "content.docval"
--        need [source, masterTemplate]
--        runErr2action $   -- spliceTemplates
--    --is the product of a gtempl and a page template
--    -- but is produced for each page (wasteful)
--            do
--                putIOwords ["testD - dtpl", showT source]
--                valText  <- read8 (makeAbsFile source)  docValueFileType
--                gtempl <- read8 (makeAbsFile masterTemplate) gtmplFileType
--                p :: Dtemplate <- spliceTemplates (valText :: DocValue)  (gtempl :: Gtemplate)
--                write8 (makeAbsFile out) dtmplFileType p



--      (testD <> "//*.withSettings.md") %> \out -> do
--        let mdSource2 = doughD </> makeRelative testD  ((out -<.> "")  -<.> "md")
--        need [mdSource2, masterSettings, masterTemplate]
--        runErr2action $ -- spliceMarkdown
--            do
--                yml <- read8 (makeAbsFile masterSettings) yamlFileType
--                source  <-read8 (makeAbsFile mdSource2) markdownFileType
--                let spliced = spliceMarkdown yml source
--                write8 (makeAbsFile out)  markdownFileType spliced

      (testD <> "//*.a.html") %> \out -> do
        let  mdSource1 =  out -<.> ""
             mdSource2 = doughD </> makeRelative testD  (mdSource1 -<.> "md")
        need [mdSource2, masterSettings, masterTemplate]
        runErr2action $
            do
                bakeOneFile True (makeAbsFile mdSource2)
                            (makeAbsDir doughD) (makeAbsDir templatesD)
                            (makeAbsFile out)

      (testD <> "//*.z.html") %> \out -> do
        liftIO $ putIOwords ["\nshakeWrapped - testD html -  out ", showT out]
        let md =   doughD </> ( makeRelative testD $ out -<.> "md")
        liftIO $ putIOwords ["\nshakeWrapped - testD html - c ", showT md]

        need [md]
        need [masterSettings]
        need [masterTemplate]

        runErr2action $ bakeOneFile False
            (makeAbsFile md)
             (makeAbsDir doughD) (makeAbsDir templatesD)
            (makeAbsFile out)


instance Exception Text

runErr2action :: Show a => ErrIO a -> Action ()
runErr2action op = liftIO $ do
    res <- runErr  op
    case res of
        Left msg -> throw msg
        Right _ -> do
--                        putIOwords ["runErr2action", "got return", showT a] --
                        return ()

(--<.>) :: FilePath -> FilePath -> FilePath
-- take away two extensions and replace with a new oneLine
f --<.> ext =  (f -<.> "") -<.> ext
