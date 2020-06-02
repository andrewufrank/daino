------------------------------------------------------------------------------
--
-- Module      :  Files needed
--              find all the files in dough
--              and construct the corresponding filepath
--              for baked
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans #-}

module Lib.FilesNeeded where

import           Uniform.Error (ErrIO, callIO, liftIO)
import           Uniform.Shake 
import           Development.Shake -- (Rules(..))
-- import          Uniform.Shake.Path
import           Uniform.Strings (putIOwords, showT)
import           Lib.Foundation (SiteLayout(..), resourcesDirName, staticDirName
                               , templatesDir, templatesImgDirName
                               , imagesDirName)
import           Lib.CmdLineArgs (PubFlags(..))
import           Lib.Bake (bakeOneFile)

-- the bakeXX are  files to construct what files are required 
-- perhaps establish need for a file      
bakePDF :: Bool -> Path Abs Dir -> Path Abs Dir -> Action [Path Abs File]
bakePDF debug doughP _ = do 
        pdfFiles1 :: [Path Rel File]
            <- getDirectoryToBake "DNB" doughP ["**/*.pdf"] -- subdirs
        let pdfFiles2 = [doughP </> c | c <- pdfFiles1]
        when debug $ liftIO
            $ putIOwords
                ["===================\nbakePDF - pdf files1", showT pdfFiles1]
        when debug $ liftIO $ putIOwords ["\nbakePDF - pdf files 2", showT pdfFiles2]
        return pdfFiles2
        -- --
       -- static html files 
bakeStaticHTML debug resourcesP staticP = do 
        htmlFiles11 :: [Path Rel File]
            <- getDirectoryToBake "DNB" resourcesP ["**/*.html"] -- subdirs
        let htmlFiles22 = [staticP </> c | c <- htmlFiles11]
        when debug $ liftIO
            $ putIOwords
                ["===================\nbakeStaticHTML - html 11 files", showT htmlFiles11]
        when debug $ liftIO $ putIOwords ["\nbakeStaticHTML - html 22 files", showT htmlFiles22]
        return htmlFiles22

bakeBiblio debug resourcesP = do 
        biblio :: [Path Rel File] <- getDirectoryToBake "DNB" resourcesP ["*.bib"]
        let biblio2 = [resourcesP </> b | b <- biblio] :: [Path Abs File]
        when debug $ putIOwords ["shake bakeBiblio", "biblio", showT biblio2]
        return biblio2
 
        -- yamlPageFiles <- getDirectoryToBake "DNB" templatesP ["*.yaml"]
        -- let yamlPageFiles2 = [templatesP </> y | y <- yamlPageFiles]
        -- when debug $ 
        --     putIOwords ["===================\nshakeMD", "yamlPages", showT yamlPageFiles2]

bakeImagesForBlog debug imagesP imagesTargetP = do 
        -- images for blog 
        imgFiles :: [Path Rel File]
            <- getDirectoryToBake "DNB" imagesP ["*.JPG", "*.jpg"]  -- no subdirs (may change in future)
        let imagesFiles2 = [imagesTargetP </> i  | i <- imgFiles]
        when True $ putIOwords ["===================\nbakeImagesForBlog"
                    , "shake imgFiles", showT imagesP, "found", showT imagesFiles2]
        return imagesFiles2

bakeCSS debug templatesP staticP = do 
        cssFiles1 :: [Path Rel File]
            <- getDirectoryToBake "DNB" templatesP ["*.css"] -- no subdirs
        let cssFiles2 = [staticP </> c | c <- cssFiles1] :: [Path Abs File]
        when debug $ liftIO
            $ putIOwords
                [ "========================\nbakeCSS - css files 1"
                , showT cssFiles1]
        when debug $ liftIO $ putIOwords ["\nbakeCSS - css files", showT cssFiles2]
        return cssFiles2


bakeMDfiles debug doughP bakedP = do 
    mdFiles1 :: [Path Rel File]
        <- getDirectoryToBake "DNB" doughP ["**/*.md"] -- includes subfiledirectories
    let htmlFiles3 = map 
            (replaceExtension' "html" . (bakedP </>)) mdFiles1 :: [Path Abs File]
        -- [( bakedP </>  md) -<.> "html" | md <- mdFiles1] 
        
    -- , not $ isInfixOf' "index.md" md]
    -- let htmlFiles3 = map (replaceExtension "html") htmlFiles2 :: [Path Abs File]
    when debug $  liftIO
        $ putIOwords
            [ "============================\nbakeMDfiles - mdFiles1"
            , showT mdFiles1]
    when debug $ 
        liftIO $ putIOwords ["\nbakeMDfiles - htmlFile3 x"
            , showT htmlFiles3]
    -- needP mdFiles1
    return htmlFiles3  -- includes the index files 