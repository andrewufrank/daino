----------------------------------------------------------------------
--
-- Module      : layout and flags  takes data from the settinsgN.yaml file (metaPage deals with the md file YAML header)
----------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- | the defintion for a layout and a flags type
  which carry info from the command line and the siteHeader file
 the defaults for flags are set up for testing  are overridden
 the defaults for layout must correspond to what is set in the test siteHeader file.
 layout defaults are used in testing

 content dirs are those, which have *.md files
-}
module Foundational.SettingsPage
    (module Foundational.SettingsPage
    , def 
    , settingsFileName
    ) where

import UniformBase
import Data.Default.Class ( Default(def) ) -- to define a default class for siteLayout 
import Uniform.Json ( FromJSON, ToJSON )

progName :: Text
progName = "SSG"  

settingsFileName :: Path Rel File
-- ^ the yaml file in which the siteHeader are fixec
settingsFileName = makeRelFile "settings3" -- the yaml file

-- | the siteHeader file with all fields 
data Settings = Settings
    { siteLayout :: SiteLayout 
    , localhostPort :: Int 
    , settingsAuthor :: Text 
    , settingsDate :: Text -- should be UTC 
    , siteHeader :: SiteHeader 
    , menuitems :: MenuItems
    -- , today :: Text
    } deriving (Show, Read, Ord, Eq, Generic, Zeros)

instance ToJSON Settings
instance FromJSON Settings

data SiteHeader = SiteHeader 
    { sitename :: FilePath 
    , byline :: Text 
    , banner :: FilePath 
    , bannerCaption :: Text 
    } deriving (Show, Read, Ord, Eq, Generic, Zeros)
instance ToJSON SiteHeader
instance FromJSON SiteHeader

newtype MenuItems = MenuItems {menuNav:: [MenuItem]
                            -- , menuB:: Text
                            } deriving (Show, Read, Ord, Eq, Generic, Zeros)
instance ToJSON MenuItems 
instance FromJSON MenuItems 

data MenuItem = MenuItem  
    { navlink :: FilePath 
    , navtext :: Text
    -- , navpdf :: Text  -- for the link to the pdf 
    -- not a good idead to put here
    } deriving (Show, Read, Ord, Eq, Generic, Zeros)
instance ToJSON MenuItem
instance FromJSON MenuItem

data SiteLayout = SiteLayout
    { -- | the place of the  theme files (includes templates)
      themeDir :: Path Abs Dir
    , -- | where the content is originally (includes resources)
      doughDir :: Path Abs Dir
    , -- | the webroot, the dir with all the produced files
      bakedDir :: Path Abs Dir
    , masterTemplateFile :: Path Rel File
    , doNotPublish :: Text 
    , blogAuthorToSuppress :: [Text]
    }
    deriving (Show, Read, Ord, Eq, Generic, Zeros)
instance ToJSON SiteLayout
instance FromJSON SiteLayout


sourceDirTestDocs :: Path Abs Dir
sourceDirTestDocs = makeAbsDir "/home/frank/Workspace11/ssg/docs/"

sourceDirTestSite :: Path Abs Dir
sourceDirTestSite = sourceDirTestDocs </> (makeRelDir "site")
-- ^ the dir with the source for the test site

layoutDefaults :: SiteLayout
-- used for finding the test cases
-- must correspond to the settings3.yaml in source code repository
layoutDefaults =
    SiteLayout
        { doughDir = sourceDirTestSite </> makeRelDir "dough"
        , bakedDir = sourceDirTestSite </> makeRelDir "baked"
        ,  themeDir = sourceDirTestDocs </> makeRelDir "theme"
 
        ,  masterTemplateFile = makeRelFile "master5.dtpl"
        , doNotPublish = "DNB"
        , blogAuthorToSuppress = []
        }

instance Default SiteLayout where 
        def = layoutDefaults

notDNB :: SiteLayout -> FilePath -> Bool 
notDNB siteLayout = not . isInfixOf' (t2s $ doNotPublish siteLayout)

templatesDirName = makeRelDir "templates"
templatesDir :: SiteLayout -> Path Abs Dir
templatesDir layout = themeDir layout `addFileName` templatesDirName

blankAuthorName :: [Text] -> Text -> Text 
-- suppress/oppress author name, if the author name is the same as one in the first arg (AUF, Andrew U..) then set it to empty else copy 
-- idea is to avoid to have each page say the obvious "author XX"
blankAuthorName names current = 
    if current `elem` names 
        then zero 
        else current 



