
------------------------------------------------------------------------------
--
-- Module      :   the defintion at the bottom
--              there will be command line args to override these
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

progName = "SSG" :: Text

data Defaults = Defaults
    { siteDir, themeDir :: Path Abs Dir -- ^ the place of the site and the theme files
    , doughDir, bakedDir :: Path Rel Dir -- ^ the names of the two dir, under siteDir
    , templateDir :: Path Rel Dir -- ^ where the templates are
    , reportFile :: Path Abs File  -- ^ the report from processing baked with pipe
    } deriving (Show, Ord, Eq, Read)

instance NiceStrings Defaults where
    showNice d = replace' ", " ",\n " (showT d)

defaults :: Defaults
defaults = Defaults{ siteDir = makeAbsDir "/home/frank/Workspace8/SSG/site"
            , doughDir = makeRelDir "dough"
            , bakedDir = makeRelDir "baked"
            , reportFile = makeAbsFile "/home/frank/reportBakeAll.txt"
            , templateDir = makeRelDir "templates"
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
doughPath = addDir (siteDir defaults) (doughDir defaults) :: Path Abs Dir
bakedPath = addDir (siteDir defaults) (bakedDir defaults) :: Path Abs Dir

templatePath :: Path Abs Dir
reportFilePath :: Path Abs File
templatePath = addDir (themeDir defaults) (templateDir defaults)
reportFilePath = reportFile defaults
