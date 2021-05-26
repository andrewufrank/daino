{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
----------------------------------------------------------------------
--
-- Module      :   the defintion at the bottom
--              there will be command line args to override these
--            all the  content must be in the site and the resources
--            not clear what role the resources play
--            the bibliographies could go with the blog
--            all themes in the theme dir (templates and css, possibly images
--
-- the tests must always use layoutDefaults and testFlags! 
----------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | the layoutDefault and the settings3.yml file read from 
-- the test site must be the stame 
-- all values must be read from layoutDefults

module Foundational.LayoutFlags where 
import UniformBase


progName :: Text
progName = "SSG"


data SiteLayout = SiteLayout
    { -- | the place of the  theme files (includes templates)
      themeDir :: Path Abs Dir
    , -- | where the content is originally (includes resources)
      doughDir :: Path Abs Dir
    , -- | the webroot, the dir with all the produced files 
      bakedDir :: Path Abs Dir
      --    , templateDir :: Path Rel Dir -- ^ where the templates are
    , -- | the report from processing baked with pipe
      reportFile :: Path Abs File
    , -- | the directory the test results go
      testDir :: Path Abs Dir
    -- , -- | the name of the banner image, needs special copy of
      -- , landingPage :: Path Rel File -- ^ the name of the landing page (html), where web server sarts
    --   bannerImage :: Path Rel File
    -- , uploadServer :: Text
    , masterTemplateFile :: Path Rel File 
    }
    deriving (Show, Read, Ord, Eq, Generic, Zeros) 
        --  Read known issue of reading path

instance NiceStrings SiteLayout where
    shownice d = replace' ", " ",\n " (showT d)

sourceDirTestDocs :: Path Abs Dir 
sourceDirTestDocs = makeAbsDir "/home/frank/Workspace11/ssg/docs/"

sourceDirTestSite :: Path Abs Dir
sourceDirTestSite = sourceDirTestDocs </> (makeRelDir "site")
-- ^ the dir with the source for the test site 

-- bannerImageFileName :: Path Rel File
-- bannerImageFileName = makeRelFileT "cropped-DSC05127-1024x330.jpg"

-- defaultPageTypeName :: Path Rel File
-- defaultPageTypeName = makeRelFileT "page3.yaml" -- the current best
-- use this and complete locally!

-- defaultPageType :: SiteLayout -> Path Abs File
-- defaultPageType layout = templatesDir layout </> defaultPageTypeName

landingPageName :: Path Rel File
landingPageName = makeRelFile "index.html" -- "landingPage.html"
-- used in runScotty 
-- this is default value for browser for page to start with
-- settingsFileName = makeRelFile "settings2"

layoutDefaults :: SiteLayout
-- used for finding the test cases
-- must correspond to the settings2.yaml in source code repository
layoutDefaults =
    SiteLayout
        { doughDir = sourceDirTestSite </> makeRelDir "dough"
        , bakedDir = sourceDirTestSite </> makeRelDir "baked"
        , --            , templateDir = makeAbsDir "templates"
          themeDir = sourceDirTestDocs </> makeRelDir "theme"
        , reportFile = makeAbsFile "/home/frank/ssgReport.txt"
        , testDir =  makeAbsDir $
                ("/home/frank" :: FilePath)
                    </> (".ssg" :: FilePath)
        -- , bannerImage = bannerImageFileName
         -- , landingPage = landingPageName
        -- ,  uploadServer = uploadServerTest
        ,  masterTemplateFile = makeRelFile "master5.dtpl"
        }

-- uploadServerTest :: Text
-- uploadServerTest = "test.gerastree.at"

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


templatesImgDirName :: Path Rel Dir
templatesImgDirName = makeRelDir "img"

settingsFileName :: Path Rel File
-- ^ the yaml file in which the settings are fixec
settingsFileName = makeRelFile "settings3" -- the yaml file
-- the value for cannot go into layout as this is its name!

testSettingsFileName :: Path Abs File
-- the settings file for tests
testSettingsFileName =
    sourceDirTestSite </>  settingsFileName

lastUploadFileName :: Path Rel File
lastUploadFileName = makeRelFile "lastload.txt" :: Path Rel File
testLastUploadFileName :: Path Abs File
testLastUploadFileName =
    sourceDirTestSite
        </> makeRelDirT "site/dough/"
        </> lastUploadFileName ::
        Path Abs File

masterTemplateFileName :: Path Rel File
{- ^ the name of the master template
 should probably be in the settings?
-}
masterTemplateFileName = masterTemplateFile layoutDefaults

-- content of settings2.yaml
--storage:
--    themeDir:  /home/frank/Workspace11/ssg/theme
--    doughDir: /home/frank/Workspace11/ssg/site/dough
--    bakedDir: /home/frank/Workspace11/ssg/site/baked
--    reportFile: /home/frank/SSGreport.txt
--    testDir: /home/frank/.SSG  -- fixed in testharness
--localhostPort: 3000

-- | the switches for material to include
data PubFlags = PubFlags
    { publishFlag
      , oldFlag
      , draftFlag
      , testFlag
      , watchFlag
      , serverFlag ::
        Bool
    , uploadFlag :: Bool
    , settingsFile :: Path Abs File
    }
    deriving (Show, Eq) -- no read for path

instance Zeros PubFlags where
    zero = PubFlags zero zero zero zero zero zero zero zero

testFlags :: PubFlags
testFlags =
    zero
        { publishFlag = True -- not including draft
        , oldFlag = True
        , draftFlag = False
        , settingsFile = testSettingsFileName
        }