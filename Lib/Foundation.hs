
------------------------------------------------------------------------------
--
-- Module      :   the defintion at the bottom
--              there will be command line args to override these

--            all the  content must be in the site and the resources
--            not clear what role the resources play
--            the bibliographies could go with the blog
--            all themes in the theme dir (templates and css, possibly images
--

-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lib.Foundation  -- (openMain, htf_thisModuelsTests)
     where

import Uniform.Strings hiding ((</>))
import Uniform.Filenames
--import Uniform.FileStrings

progName :: Text
progName = "SSG"

data SiteLayout = SiteLayout
    { themeDir :: Path Abs Dir -- ^ the place of the  theme files (includes templates)
    , doughDir                  -- ^ where the content is originally (includes resources)
    , bakedDir :: Path Abs Dir -- ^ where all the files serving are
--    , templateDir :: Path Rel Dir -- ^ where the templates are
    , reportFile :: Path Abs File  -- ^ the report from processing baked with pipe
    , testDir :: Path Abs Dir -- ^ the directory the test results go
                        -- not important
    } deriving (Show, Ord, Eq, Read)

instance NiceStrings SiteLayout where
    shownice d = replace' ", " ",\n " (showT d)

sourceDir :: Path Abs Dir
sourceDir = makeAbsDir "/home/frank/Workspace8/ssg"

--testDir = makeAbsDir $ ("/home/frank" :: FilePath)   </> (t2s progName)

layoutDefaults :: SiteLayout
layoutDefaults = SiteLayout{  doughDir = sourceDir </> makeRelDir "site/dough"
            , bakedDir = sourceDir </> makeRelDir "site/baked"
            , reportFile = makeAbsFile "/home/frank/SSGreport.txt"
--            , templateDir = makeAbsDir "templates"
            , themeDir = sourceDir </> makeRelDir "theme"
            , testDir = makeAbsDir
                $ ("/home/frank" :: FilePath)   </> (t2s progName <> "test")
            }

templatesDirName, staticDirName :: Path Rel Dir
templatesDirName = (makeRelDir "templates")
staticDirName = makeRelDir "static"
resourcesDirName = makeRelDir "resources"


