
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

module Lib.Shake
     where

import Uniform.Strings (putIOwords) -- hiding ((<.>), (</>))
import Uniform.Filenames -- (toFilePath, makeAbsFile, makeRelFile, makeRelDir, stripProperPrefix')
         hiding ((<.>), (</>))
import Uniform.FileStrings () -- for instances
import Uniform.Error
import Lib.Templating


import Lib.Foundation
import Lib.Bake

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

shake ::    ErrIO ()
shake   = do
    putIOwords ["\nshake start"]
    callIO $ shakeWrapped-- bakeAllInSiteMD (bakeOneFile2 False)  doughPath  reportFilePath
    putIOwords ["\nshake done", "\n"]

    return ()

bakedD  = "site/baked" -- toFilePath bakedPath
doughD = "site/dough"
templatesD = "theme/templates"
staticD = "site/baked/static"  -- where all the static files go

shakeWrapped :: IO  ()
shakeWrapped = shakeArgs shakeOptions {shakeFiles=bakedD
                , shakeVerbosity=Chatty -- Loud
                , shakeLint=Just LintBasic
--                , shakeRebuild=[(RebuildNow,"allMarkdownConversion")]
--                  seems not to produce an effect
                } $
    do
        want ["allMarkdownConversion"]

        phony "allMarkdownConversion" $
            do
                liftIO $ putIOwords ["shakeWrapped phony allMarkdonwConversion" ]

                -- get markdown files
                mdFiles1 <- getDirectoryFiles  doughD ["//*.md", "//*.markdown"]
                let htmlFiles2 = [bakedD </> md -<.> "html" | md <- mdFiles1]
                liftIO $ putIOwords ["shakeWrapped - htmlFile", showT htmlFiles2]

                -- get css
                cssFiles1 <- getDirectoryFiles templatesD ["*.css"] -- no subdirs
                liftIO $ putIOwords ["shakeWrapped - phony cssFiles1", showT cssFiles1]
                let cssFiles2 = [replaceDirectory c staticD  | c <- cssFiles1]
--                let cssFiles2 = [dropDirectory1 staticD </> c  | c <- cssFiles1]
                liftIO $ putIOwords ["shakeWrapped - phony cssFiles2", showT cssFiles2]
--                mapM_ (\fn -> copyFileChanged (templatesD </> fn) (staticD </> fn)) cssFiles1

--                liftIO $ putIOwords ["shakeWrapped - htmlFile", showT htmlFiles2]
--           shakeWrapped - htmlFile ["site/baked/index.html","site/baked/Blog/postwk.html"...
                need cssFiles2
--                need [staticD</>"page33.dtpl"]
                need htmlFiles2

        (bakedD <> "//*.html") %> \out ->
            do
                liftIO $ putIOwords ["shakeWrapped - bakedD html -  out ", showT out]
                let c =   dropDirectory1 . dropDirectory1 $ out -<.> "md"
                liftIO $ putIOwords ["shakeWrapped - bakedD html - c ", showT c]
                need [doughD </> c]
                need [templatesD</>"page33.dtpl"]
                liftIO $  bakeOneFileIO  c  -- c relative to dough/

        (templatesD</>"page33.dtpl") %> \out ->     -- construct the template from pieces
            do
                liftIO $ putIOwords ["shakeWrapped - templatesD dtpl -  out ", showT out]
                let mf = templatesD</>"Master3.gtpl"
                let pf = templatesD</>"Page3.dtpl"
                need [mf, pf]
                liftIO $ makeOneMaster out
--                copyFileChanged (replaceDirectory out templatesD) out

        (staticD </> "*.css") %> \out ->            -- insert css
            do
                liftIO $ putIOwords ["shakeWrapped - staticD - *.css", showT out]
                copyFileChanged (replaceDirectory out templatesD) out


instance Exception Text

makeOneMaster :: FilePath -> IO ()
-- makes the master plus page style in template
makeOneMaster fp = do
    putIOwords ["makeOneMaster", showT fp]

    res <- runErrorVoid $ putPageInMaster templatePath
                     (makeRelFile "Page3") (makeRelFile "Master3")
                    "body" (makeRelFile "page33")
    putIOwords ["makeOneMaster done"]
    return ()


bakeOneFileIO :: FilePath -> IO ()
-- bake one file (relative to dough)
bakeOneFileIO fp = do
            et <- runErr $ do
                    putIOwords ["bakeOneFileIO - from shake xx", s2t fp] --baked/SGGdesign/Principles.md
                    let fp1 = fp
--                            let fp1 = "/"<>fp
                    putIOwords ["bakeOneFileIO - gp1", s2t fp1]
                    let fp2 = makeRelFile fp1
--                    fp3 :: Path Rel File  <- stripProperPrefix' (makeRelDir doughD) fp2
                    putIOwords ["bakeOneFileIO - next run shake", showT fp2]
--                            bakeOneFileIO - next run shake "baked/SGGdesign/Principles.md"
                    let fp3 = fp2
                    res <- bakeOneFile True fp3
                    putIOwords ["bakeOneFileIO - done", showT fp3, res]
            case et of
                Left msg -> throw msg
                Right _ -> return ()
