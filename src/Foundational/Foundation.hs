----------------------------------------------------------------------
--
-- Module      :   the defintion at the bottom
--              there will be command line args to override these

--            all the  content must be in the site and the resources
--            not clear what role the resources play
--            the bibliographies could go with the blog
--            all themes in the theme dir (templates and css, possibly images
--
----------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric     #-}

module Foundational.Foundation  -- (openMain, htf_thisModuelsTests)
                      where
import UniformBase
import GHC.Generics 

-- import           Uniform.Strings        -- hiding ( (</>) )
-- import           Uniform.Filenames
--import Uniform.FileStrings

progName :: Text
progName = "SSG"

data SiteLayout = SiteLayout
    { themeDir :: Path Abs Dir -- ^ the place of the  theme files (includes templates)
    , doughDir :: Path Abs Dir -- ^ where the content is originally (includes resources)
    , bakedDir :: Path Abs Dir -- ^ where all the files serving are
--    , templateDir :: Path Rel Dir -- ^ where the templates are
    , reportFile :: Path Abs File  -- ^ the report from processing baked with pipe
    , testDir :: Path Abs Dir -- ^ the directory the test results go
    , bannerImage :: Path Rel File -- ^ the name of the banner image, needs special copy of
    -- , landingPage :: Path Rel File -- ^ the name of the landing page (html), where web server sarts                   
    , uploadServer :: Text 
    } deriving (Show, Read, Ord, Eq, Generic, Zeros)  --  Read known issue of reading path

instance NiceStrings SiteLayout where
    shownice d = replace' ", " ",\n " (showT d)

sourceDirTest :: Path Abs Dir
sourceDirTest = makeAbsDir "/home/frank/Workspace11/ssg"

bannerImageFileName :: Path Rel File
bannerImageFileName = makeRelFileT "cropped-DSC05127-1024x330.jpg"

defaultPageTypeName :: Path Rel File
defaultPageTypeName = makeRelFileT "page3.yaml"  -- the current best
                -- use this and complete locally!
defaultPageType :: SiteLayout -> Path Abs File
defaultPageType layout = templatesDir layout </> defaultPageTypeName

--testDir = makeAbsDir $ ("/home/frank" :: FilePath)   </> (t2s progName)
landingPageName :: Path Rel File
landingPageName = makeRelFile "index.html"  -- "landingPage.html"
-- this is default value for browser for page to start with 
-- settingsFileName = makeRelFile "settings2"

layoutDefaults :: SiteLayout
-- used for finding the test cases
-- must correspond to the settings2.yaml in source code repository
layoutDefaults = SiteLayout
    { doughDir    = sourceDirTest </> makeRelDir "docs/site/dough"
    , bakedDir    = sourceDirTest </> makeRelDir "docs/site/baked"
    , reportFile  = makeAbsFile "/home/frank/SSGreport.txt"
--            , templateDir = makeAbsDir "templates"
    , themeDir    = sourceDirTest </> makeRelDir "theme"
    , testDir     = makeAbsDir
                    $   ("/home/frank" :: FilePath)
                    </> ("." <> t2s progName)
    , bannerImage = bannerImageFileName
    -- , landingPage = landingPageName
    , uploadServer = uploadServerTest
    }

uploadServerTest :: Text
uploadServerTest = "test.gerastree.at"

templatesDirName, staticDirName :: Path Rel Dir
templatesDirName = makeRelDir "templates"
templatesDir :: SiteLayout -> Path Abs Dir
templatesDir layout = themeDir layout `addFileName` templatesDirName

staticDirName = makeRelDir "static"
resourcesDirName :: Path Rel Dir
resourcesDirName = makeRelDir "resources"  
imagesDirName :: Path Rel Dir
imagesDirName = makeRelDir "img"
-- imagesResourcesDirName = resourcesDirName </> imagesDirName 
lastUploadFileName :: Path Rel File
lastUploadFileName = makeRelFile "lastload.txt" :: Path Rel File 
        
templatesImgDirName :: Path Rel Dir
templatesImgDirName = makeRelDir "img"

settingsFileName :: Path Rel File
-- ^ the yaml file in which the settings are fixec
settingsFileName = makeRelFile "settings2" -- the yaml file
-- the value for cannot go into layout as this is its name!

testSettingsFileName :: Path Abs File
-- the settings file for tests 
testSettingsFileName =
    sourceDirTest </> makeRelDirT "docs/site/dough/" </> settingsFileName
testLastUploadFileName :: Path Abs File
testLastUploadFileName = sourceDirTest </> 
        makeRelDirT "docs/site/dough/" </> lastUploadFileName 
        :: Path Abs File 

masterTemplateFileName :: Path Rel File
-- ^ the name of the master template
-- should probably be in the settings?
masterTemplateFileName = makeRelFile "master4.dtpl"
-- content of settings2.yaml
--storage:
--    themeDir:  /home/frank/Workspace11/ssg/theme
--    doughDir: /home/frank/Workspace11/ssg/site/dough
--    bakedDir: /home/frank/Workspace11/ssg/site/baked
--    reportFile: /home/frank/SSGreport.txt
--    testDir: /home/frank/.SSG  -- fixed in testharness
--localhostPort: 3000

