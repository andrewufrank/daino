
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
import Uniform.FileIO -- (toFilePath, makeAbsFile
--                , makeRelFile, makeRelDir, stripProperPrefix')
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
import Development.Shake.Linters (yamllint)
--import Path.IO (setCurrentDir)

shake :: SiteLayout ->    ErrIO ()
shake layout   = do
    putIOwords ["\nshake start", shownice layout]
    let
      doughD      =   toFilePath . doughDir $ layout  -- the regular dough
      templatesD =   (toFilePath . themeDir $ layout) </> (toFilePath templatesDirName)
--      testD = toFilePath  $  testDir layout
      bakedD = toFilePath . bakedDir $ layout
    --              staticD = testD </>"static"  -- where all the static files go
      masterSettings = doughD</>"settings2.yaml"
      masterTemplate = templatesD</>"master4.dtpl"
    setCurrentDir (doughDir layout)

    -- delete old
    fs <- getDirectoryDirs' bakedD
    putIOwords ["shakeTesting", "to delete", showT fs]
--  mapM_ removeDirectoryRecursive fs

    callIO $ shakeWrapped doughD templatesD bakedD
--                (toFilePath . doughDir $ layout)
--                ((toFilePath . themeDir $ layout) </> (toFilePath templatesDirName))
--                (toFilePath . bakedDir $ layout)

    putIOwords ["\nshake done", "\n"]

    return ()

--getDirectoryDirs' :: FilePath -> ErrIO [FilePath]
--getDirectoryDirs' dir = filterM f =<< getDirCont  dir
--    where f x =  doesDirExist' $ dir </> x


shakeWrapped :: FilePath -> FilePath -> FilePath ->  IO  ()
shakeWrapped doughD templatesD bakedD = shakeArgs shakeOptions {shakeFiles=bakedD
                , shakeVerbosity=Chatty -- Loud
                , shakeLint=Just LintBasic
--                , shakeRebuild=[(RebuildNow,"allMarkdownConversion")]
--                  seems not to produce an effect
                } $ do

    let staticD = bakedD </> (toFilePath staticDirName)
        -- where all the static files go

--    phony "clean" $ do
--        putNormal "delete all files in "
--        removeFiles bakedD<>"*"
            -- need one for ssg and one for baked

    want ["allMarkdownConversion"]
    phony "allMarkdownConversion" $ do

        liftIO $ putIOwords ["\nshakeWrapped phony allMarkdonwConversion" ]

--        yamllint  -- issue with create process
--        yamllint: createProcess: runInteractiveProcb


        -- get markdown files
        mdFiles1 <- getDirectoryFiles  doughD ["//*.md", "//*.markdown"]
        let htmlFiles2 = [bakedD </> md -<.> "html" | md <- mdFiles1]
        liftIO $ putIOwords ["\nshakeWrapped - htmlFile"
                ,  showT (map (makeRelative doughD) htmlFiles2)]
                -- list of file.html (no dir)

        -- get css
        cssFiles1 <- getDirectoryFiles templatesD ["*.css"] -- no subdirs
    --                liftIO $ putIOwords
--                    ["\nshakeWrapped - phony cssFiles1", showT cssFiles1]
        let cssFiles2 = [replaceDirectory c staticD  | c <- cssFiles1]
    --                let cssFiles2 = [dropDirectory1 staticD </> c  | c <- cssFiles1]
        liftIO $ putIOwords ["\nshakeWrapped - phony cssFiles2", showT cssFiles2]
    --                mapM_ (\fn -> copyFileChanged (templatesD </> fn)
--                    (staticD </> fn)) cssFiles1

        -- pages templates
        pageTemplates <- getDirectoryFiles templatesD ["/*.gtpl"]
        let pageTemplates2 = [templatesD </> tpl | tpl <- pageTemplates]
        liftIO $ putIOwords ["\nshakeWrapped - phony pageTemplates", showT pageTemplates2]

        -- index pages
        ixPages <- getDirectoryFiles doughD ["//*.dtpl" ]
        liftIO $ putIOwords ["\nshakeWrapped - index file tempaltes"
                ,  showT (map (makeRelative doughD) ixPages)]


        need pageTemplates2
        need cssFiles2
        need htmlFiles2
        -- the templates static files are copied with watch

    (bakedD <> "//*.html") %> \out -> do

        liftIO $ putIOwords ["\nshakeWrapped - bakedD html -  out ", showT out]
        let md =   doughD </>  (makeRelative bakedD $ out -<.> "md")
        liftIO $ putIOwords ["\nshakeWrapped - bakedD html - c ", showT md]

--        let masterTemplate = templatesD</>"Master3.gtpl"
--            masterSettings_yaml = doughD </> "settings2.yaml"
        need [md]
--        need [masterSettings_yaml]
--        need [masterTemplate]
        runErr2action $ bakeOneFileFPs  md  doughD templatesD out
            -- c relative to dough/

--    (templatesD</>"*.dtpl") %> \out ->     -- check only existence
--        do


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

--makeOneMaster :: FilePath -> FilePath -> FilePath -> IO ()
---- makes the master plus page style in template
--makeOneMaster master page result = do
--    putIOwords ["makeOneMaster", showT result]
--
--    res <- runErrorVoid $ putPageInMaster
--                     (makeAbsFile page) (makeAbsFile master)
--                    "body" (makeAbsFile result)
--    putIOwords ["makeOneMaster done", showT res ]
--    return ()

