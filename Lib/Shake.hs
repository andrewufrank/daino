
------------------------------------------------------------------------------
--
-- Module      :   the  process to convert
--              files in any input format to html
--              orginals are found in dire doughDir and go to bakeDir
--

-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans #-}

module Lib.Shake
     where

import Uniform.Strings (putIOwords) -- hiding ((<.>), (</>))
import Uniform.Filenames -- (toFilePath, makeAbsFile, makeRelFile, makeRelDir, stripProperPrefix')
         hiding ((<.>), (</>))
import Uniform.FileStrings () -- for instances
--import Uniform.Error
import Lib.Templating


import Lib.Foundation (SiteLayout (..), templatesDirName, staticDirName)
import Lib.Bake

import Development.Shake
--import Development.Shake.Command
import Development.Shake.FilePath
--import Development.Shake.Util

shake :: SiteLayout ->    ErrIO ()
shake layout   = do
    putIOwords ["\nshake start", shownice layout]
    callIO $ shakeWrapped
                (toFilePath . doughDir $ layout)
                ((toFilePath . themeDir $ layout) </> (toFilePath templatesDirName))
                (toFilePath . bakedDir $ layout)

    putIOwords ["\nshake done", "\n"]

    return ()


shakeWrapped :: FilePath -> FilePath -> FilePath ->  IO  ()
shakeWrapped doughD templatesD bakedD = shakeArgs shakeOptions {shakeFiles=bakedD
                , shakeVerbosity=Chatty -- Loud
                , shakeLint=Just LintBasic
--                , shakeRebuild=[(RebuildNow,"allMarkdownConversion")]
--                  seems not to produce an effect
                } $ do

    let staticD = bakedD </> (toFilePath staticDirName)  -- where all the static files go

--    phony "clean" $ do
--        putNormal "delete all files in "
--        removeFiles bakedD<>"*"
            -- need one for ssg and one for baked

    want ["allMarkdownConversion"]

    phony "allMarkdownConversion" $ do

        liftIO $ putIOwords ["\nshakeWrapped phony allMarkdonwConversion" ]

        -- get markdown files
        mdFiles1 <- getDirectoryFiles  doughD ["//*.md", "//*.markdown"]
        let htmlFiles2 = [bakedD </> md -<.> "html" | md <- mdFiles1]
        liftIO $ putIOwords ["\nshakeWrapped - htmlFile"
                ,  showT (map (makeRelative doughD) htmlFiles2)]

        -- get css
        cssFiles1 <- getDirectoryFiles templatesD ["*.css"] -- no subdirs
    --                liftIO $ putIOwords ["\nshakeWrapped - phony cssFiles1", showT cssFiles1]
        let cssFiles2 = [replaceDirectory c staticD  | c <- cssFiles1]
    --                let cssFiles2 = [dropDirectory1 staticD </> c  | c <- cssFiles1]
        liftIO $ putIOwords ["\nshakeWrapped - phony cssFiles2", showT cssFiles2]
    --                mapM_ (\fn -> copyFileChanged (templatesD </> fn) (staticD </> fn)) cssFiles1

        -- pages templates
        pageTemplates <- getDirectoryFiles templatesD ["/*.gtpl", "/*.dtpl"]
        let pageTemplates2 = [templatesD </> tpl | tpl <- pageTemplates]
        liftIO $ putIOwords ["\nshakeWrapped - phony pageTemplates", showT pageTemplates2]


        need pageTemplates2
        need cssFiles2
        need htmlFiles2
        -- the templates static files are copied with watch

    (bakedD <> "//*.html") %> \out -> do

        liftIO $ putIOwords ["\nshakeWrapped - bakedD html -  out ", showT out]
        let md =   doughD </> ( makeRelative bakedD $ out -<.> "md")
        liftIO $ putIOwords ["\nshakeWrapped - bakedD html - c ", showT md]

        let masterTemplate = templatesD</>"Master3.gtpl"
            masterSettings_yaml = doughD </> "settings2.yaml"
        need [md]
        need [masterSettings_yaml]
        need [masterTemplate]
        runErr2action $ bakeOneFileFPs  md  masterSettings_yaml masterTemplate out  -- c relative to dough/

--        (templatesD</>"page33.dtpl") %> \out ->     -- construct the template from pieces
--            do
--                liftIO $ putIOwords ["\nshakeWrapped - templatesD dtpl -  out ", showT out]
--                let mf = templatesD</>"Master3.gtpl"
--                let pf = templatesD</>"Page3.dtpl"
--                need [mf, pf]
--                liftIO $ makeOneMaster mf pf out
----                copyFileChanged (replaceDirectory out templatesD) out

    (staticD </> "*.css") %> \out ->  do           -- insert css
        liftIO $ putIOwords ["\nshakeWrapped - staticD - *.css", showT out]
--        let etFont = staticD</>"et-book"  -- the directory with the fonts
--        copyFileChanged (replaceDirectory etFont templatesD) etFont
--          gives need on dir - all static files are copied_
--           with watch
        copyFileChanged (replaceDirectory out templatesD) out


instance Exception Text

runErr2action :: ErrIO a -> Action a
runErr2action op = liftIO $ do
    res <- runErr  op
    case res of
        Left msg -> throw msg
        Right a -> return a

makeOneMaster :: FilePath -> FilePath -> FilePath -> IO ()
-- makes the master plus page style in template
makeOneMaster master page result = do
    putIOwords ["makeOneMaster", showT result]

    res <- runErrorVoid $ putPageInMaster
                     (makeAbsFile page) (makeAbsFile master)
                    "body" (makeAbsFile result)
    putIOwords ["makeOneMaster done", showT res ]
    return ()


--bakeOneFileIO2 :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
---- bake one file absolute fp , page template and result html
--bakeOneFileIO2 md masterSettingsFn template ht = do
--            et <- runErr $ bakeOneFileIO md masterSettingsFn template ht
----            do
----                    putIOwords ["bakeOneFileIO - from shake xx", s2t md] --baked/SGGdesign/Principles.md
------                    let fp1 = fp
--------                            let fp1 = "/"<>fp
----                    let md2 = makeAbsFile md
----                    let template2 = makeAbsFile template
----                    let ht2 = makeAbsFile ht
----                    putIOwords ["bakeOneFileIO - files", showT md2, "\n", showT template2, "\n", showT ht2]
------                    let fp2 = makeRelFile fp1
------                    fp3 :: Path Rel File  <- stripProperPrefix' (makeRelDir doughD) fp2
------                    putIOwords ["bakeOneFileIO - next run shake", showT fp2]
------                            bakeOneFileIO - next run shake "baked/SGGdesign/Principles.md"
------                    let fp3 = fp2
----                    let masterSettings = makeAbsFile masterSettingsFn
----                    res <- bakeOneFile True md2 masterSettings template2 ht2
----                    putIOwords ["bakeOneFileIO - done", showT ht2, res]
--            case et of
--                Left msg -> throw msg
--                Right _ -> return ()
