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
    , toJSON, fromJSON
    ) where

import UniformBase
import Data.Default.Class ( Default(def) ) -- to define a default class for siteLayout 
import Uniform.Json ( FromJSON, ToJSON (toJSON), fromJSON )
import Uniform.MetaPlus hiding (MetaPlus(..), Settings(..), ExtraValues(..))
import qualified Data.Map as M
import Uniform.Latex

import Path (parent)
progName, progTitle :: Text
progName = "daino"  
progTitle = "constructing a static site generator" :: Text

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

data DainoMetaPlus = DainoMetaPlus 
                { metap :: Meta    -- ^ the pandoc meta 
                , sett :: Settings -- ^ the data from the settingsfile
                , extra :: DainoValues -- ^ other values to go into template
                , metaMarkdown :: M.Map Text Text 
                , metaHtml ::  M.Map Text Text
                , metaLatex ::  M.Map Text Text
                }
    deriving (Eq, Ord, Show, Read, Generic) -- Zeros, ToJSON, FromJSON)
instance ToJSON DainoMetaPlus
instance FromJSON DainoMetaPlus
instance Zeros DainoMetaPlus where 
        zero = DainoMetaPlus zero zero zero zero zero zero

instance ToJSON Settings
instance FromJSON Settings

-- the extraValues will eventually go into settings
data DainoValues = DainoValues 
                        { mdFile:: FilePath -- Path Abs File -- abs file path 
                        , mdRelPath :: FilePath -- Path Rel File  -- rel file path
                        , dirEntries :: [IndexEntry2] 
                        , fileEntries :: [IndexEntry2] 
                                -- only the dirs and files path
                        , dainoVersion :: Text 
                        , latLanguage :: Text 
                        , authorReduced :: Text
                        -- , extraBakedDir :: Text
                        }
    deriving (Eq, Ord, Show, Read, Generic, Zeros)


-- instance Zeros DainoValues where 
--     zero = DainoValues zero  zero zero  zero zero  zero zero
instance ToJSON DainoValues 
instance FromJSON DainoValues 

-- this was defined in uniform-latex 
-- there renamed to indexEntryRenamed 

data IndexEntry2 = IndexEntry2 
    { -- | the abs file path
      ixfn :: FilePath -- Path Abs File
    , -- | the link for this page (relative to web root)}
      link :: FilePath -- Path Rel File
    , title :: Text
    , abstract :: Text
    , author :: Text
    , date :: Text
    , content :: Text   -- in latex style, only filled bevore use
    , visibility ::   Text
    , version ::   Text 
    , sortOrder :: Text
    -- , indexPage :: Bool
    -- , dirEntries :: [FilePath] -- [Path Abs Dir] -- [IndexEntry2] -- def []
    -- , fileEntries :: [FilePath] -- [Path Abs File] -- [IndexEntry2] -- def []
    , headerShift :: Int   
    } deriving (Show, Read, Eq, Ord, Generic, Zeros)
    --  IndexTitleSubdirs | IndexTitleFiles 

-- instance Zeros IndexEntry2 where zero = IndexEntry2 [] []
-- zero zero zero zero zero zero zero

instance ToJSON IndexEntry2
instance FromJSON IndexEntry2

isIndexPage :: Path Abs File -> Bool 
isIndexPage filename =  getNakedFileName filename == "index"




data SiteLayout = SiteLayout
    { -- | the place of the  theme files (includes templates)
      themeDir :: Path Abs Dir
    , -- | where the content is originally (includes resources)
      doughDir :: Path Abs Dir
    , -- | the webroot, the dir with all the produced files
      bakedDir :: Path Abs Dir
    , masterTemplateFile :: Path Rel File  -- for html
    , texTemplateFile :: Path Rel File   -- for latex 
    , doNotBake :: Text 
    -- todo probably not used
    , blogAuthorToSuppress :: [Text]
    , defaultAuthor :: Text
    , replaceErlaubtFile :: Path Abs File
    -- the list of permitted (not to replace)
    
    -- , defaultBibliography:: Text
    -- cannot be defaulted, value must be read by pandoc 
    }
    deriving (Show, Read, Ord, Eq, Generic, Zeros)
instance ToJSON SiteLayout
instance FromJSON SiteLayout
 

sourceDirTestDocs :: Path Abs Dir
sourceDirTestDocs = makeAbsDir "/home/frank/daino/docs/"

sourceDirTestSite :: Path Abs Dir
sourceDirTestSite = sourceDirTestDocs </> (makeRelDir "site")
-- ^ the dir with the source for the test site

layoutDefaults :: Path Abs Dir -> Path Abs Dir ->  SiteLayout
-- used for finding the test cases
-- must correspond to the settings3.yaml in source code repository
-- fix this later for use in testing todo 
layoutDefaults dough4test homeDir1 =
    zero -- SiteLayout
        { doughDir = dough4test
        , bakedDir = homeDir1 </> makeRelDir "bakedTestSite" :: Path Abs Dir
        ,  themeDir = (parent (parent dough4test)) </> makeRelDir "theme"
 
        ,  masterTemplateFile = makeRelFile "master7tufte.dtpl"
        , texTemplateFile = makeRelFile "resources/theme/templates/latex7.dtpl"
        ,  doNotBake = "DNB"
        -- included in filenames (and directories) to exclude from bake process
        , blogAuthorToSuppress = []
        , defaultAuthor = "AOS"
        , replaceErlaubtFile = makeAbsFile "/home/frank/Workspace11/replaceUmlaut/nichtUmlaute.txt"
        -- , defaultBibliography = "resources/BibTexLatex.bib"
        }

-- instance Default SiteLayout where 
--         def = layoutDefaults

-- notDNB :: SiteLayout -> FilePath -> Bool 
-- notDNB siteLayout = not . isInfixOf' (t2s $ doNotPublish siteLayout)

resourcesName =  "resources"
templatesName = "templates"
themeName = "theme"

templatesDir :: SiteLayout -> Path Abs Dir
templatesDir layout = themeDir layout 
            `addFileName` (makeRelDir templatesName)

blankAuthorName :: [Text] -> Text -> Text 
-- suppress/oppress author name, if the author name is the same as one in the first arg (AUF, Andrew U..) then set it to empty else copy 
-- goal is to avoid to have each page say the obvious "author XX"
blankAuthorName names current = 
    if current `elem` names 
        then zero 
        else current 

