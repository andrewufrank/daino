----------------------------------------------------------------------
--
-- Module      : layout and flags 
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
  which carry info from the command line and the settings file
 the defaults for flags are set up for testing  are overridden
 the defaults for layout must correspond to what is set in the test settings file.
 layout defaults are used in testing

 content dirs are those, which have *.md files
-}
module Foundational.LayoutFlags
    (module Foundational.LayoutFlags
    , def ) where

import UniformBase
import Data.Default.Class -- to define a default class for pub flags 
import Uniform.Json

progName :: Text
progName = "SSG"  

-- | the settings file with all fields 
data Settings = Settings
    { storage :: SiteLayout 
    , localhostPort :: Int 
    , settingsAuthor :: Text 
    , settingsDate :: Text -- should be UTC 
    , settings :: Settings2 
    , menuitems :: MenuItems
    -- , today :: Text
    } deriving (Show, Read, Ord, Eq, Generic, Zeros)

instance ToJSON Settings
instance FromJSON Settings

data Settings2 = Settings2 
    { sitename :: FilePath 
    , byline :: Text 
    , banner :: FilePath 
    , bannerCaption :: Text 
    } deriving (Show, Read, Ord, Eq, Generic, Zeros)
instance ToJSON Settings2
instance FromJSON Settings2

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
-- must correspond to the settings2.yaml in source code repository
layoutDefaults =
    SiteLayout
        { doughDir = sourceDirTestSite </> makeRelDir "dough"
        , bakedDir = sourceDirTestSite </> makeRelDir "baked"
        ,  themeDir = sourceDirTestDocs </> makeRelDir "theme"
 
        ,  masterTemplateFile = makeRelFile "master5.dtpl"
        , doNotPublish = "DNB"
        , blogAuthorToSuppress = []
        }

notDNB :: SiteLayout -> FilePath -> Bool 
notDNB siteLayout = not . isInfixOf' (t2s $ doNotPublish siteLayout)

templatesDirName = makeRelDir "templates"
templatesDir :: SiteLayout -> Path Abs Dir
templatesDir layout = themeDir layout `addFileName` templatesDirName


settingsFileName :: Path Rel File
-- ^ the yaml file in which the settings are fixec
settingsFileName = makeRelFile "settings3" -- the yaml file
-- -- the value for cannot go into layout as this is its name!
-- is then set in flags
-- testNew bakes all test data, test alone continue the previous test

-- | the switches for material to include
-- they are (for now) just bools which allow the 
-- baking of all md (higher than the switch)
data PubFlags = PubFlags
    { privateFlag
      , draftFlag
    --   , oldFlag
      , testFlag
      , testNewFlag 
      , quickFlag
      , watchFlag
      , serverFlag :: Bool
    -- , uploadFlag :: Bool
    , settingsFile :: Path Abs File
    }
    deriving (Show, Eq) -- no read for path

instance Zeros PubFlags where
    zero = PubFlags zero zero zero zero zero zero zero zero 
instance Default PubFlags where 
        def = testFlags 

testFlags :: PubFlags
testFlags =
    zero
        { privateFlag = False -- not including draft
        , draftFlag = False
        , settingsFile = sourceDirTestSite </> settingsFileName
        }

blankAuthorName :: [Text] -> Text -> Text 
-- suppress/oppress author name, if the author name is the same as one in the first arg (AUF, Andrew U..) then set it to empty else copy 
-- idea is to avoid to have each page say the obvious "author XX"
blankAuthorName names current = 
    if current `elem` names 
        then zero 
        else current 

-- hpAuthor :: [Text]
-- the list of authornames which are blanked
-- should be the author of the blog
-- hpAuthor = ["AUF", "Andrew U. Frank"]

-- old ideas
--  Read known issue of reading path

-- instance NiceStrings SiteLayout where
--     shownice d = replace' ", " ",\n " (showT d)

-- instance Default SiteLayout where 
--     def = layoutDefaults
-- instance Default Settings where 
--     def = settingsDefault

-- settingsDefault :: Settings
-- -- | the default values for settings
-- -- valid should be the content from the settings3.yaml file
-- -- which is stored ind docs/site (and linked in ssg/ )
-- settingsDefault = Settings 
--     {storage = SiteLayout 
--         {themeDir = makeAbsDir "/home/frank/Workspace11/ssg/docs/theme/", 
--         doughDir = makeAbsDir "/home/frank/Workspace11/ssg/docs/site/dough/", 
--         bakedDir = makeAbsDir "/home/frank/Workspace11/ssg/docs/site/baked/", 
--         masterTemplateFile = makeRelFile "master5.dtpl"
--         }, 
--     localhostPort = 3000, 
--     settingsAuthor = "Author of Settings", 
--     settingsBlogAuthorOppressed = ["AUF", "Andrew U. Frank"],
--     settingsDate = "2019-01-01", 
--     settings = Settings2 
--         {sitename = "siteNameExample-Default", 
--         byline = "siteByLineExample-Default", 
--         banner = "/templates/img/symmetricGeras2.jpg"
--         }, 
--     menuitems = MenuItems 
--         {menuNav=[
--             MenuItem {navlink = "/Blog/index.html", navtext = "Blog"},
--             MenuItem {navlink = "/PublicationList/index.html", navtext = "Publications"},
--             MenuItem {navlink = "/SSGdesign/index.html", navtext = "SSG Documentation"}
--             ]
--         }
--     -- today = "2021-05-30"
--     }

-- MenuItem where 
--     def = MenuItem{navlink = "/Blog/index.html", navtext = "SingleMenu"}
-- newtype Menu1 = Menu1 {mENU::[MenuItem]}deriving (Show, Read, Ord, Eq, Generic, Zeros)

-- instance Default Menu1 where 
--         def = Menu :: [def]