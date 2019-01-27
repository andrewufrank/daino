
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

import Uniform.Strings
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
                        -- not important
    } deriving (Show, Ord, Eq, Read)

instance NiceStrings SiteLayout where
    shownice d = replace' ", " ",\n " (showT d)

--siteDir = makeAbsDir "/home/frank/Workspace8/SSG/site"

layoutDefaults :: SiteLayout
layoutDefaults = SiteLayout{  doughDir = makeAbsDir "/home/frank/Workspace8/SSG/site/dough"
            , bakedDir = makeAbsDir "/home/frank/Workspace8/SSG/site/baked"
            , reportFile = makeAbsFile "/home/frank/reportBakeAll.txt"
--            , templateDir = makeAbsDir "templates"
            , themeDir = makeAbsDir "/home/frank/Workspace8/SSG/theme"
}

--doughDir, bakedDir :: Path Rel Dir
---- ^ the names of the two dir, under siteDir
--doughDir = makeRelDir "dough"
--bakedDir = makeRelDir "baked"
--
--siteDir :: Path Abs Dir
---- ^ the path to the siteDir (absolute - need not inside package
--siteDir = makeAbsDir "/home/frank/Workspace8/SSG/site"

-- all Path will become functions with Default as argument
doughPath, bakedPath :: Path Abs Dir
-- the path (absolute) to dough an baked
doughPath =  (doughDir layoutDefaults) :: Path Abs Dir
bakedPath =   (bakedDir layoutDefaults) :: Path Abs Dir

--sitePath = siteDir layoutDefaults
templatesPath :: Path Abs Dir
reportFilePath :: Path Abs File
templatesDir :: Path Rel Dir
templatesDir = makeRelDir "templates"
templatesPath = addDir (themeDir layoutDefaults) (templatesDir)
reportFilePath = reportFile layoutDefaults
