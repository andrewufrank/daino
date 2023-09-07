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
    , MetaPlus (..)
    ) where

import UniformBase
import Uniform.Json ( FromJSON, ToJSON (toJSON), fromJSON )
import Uniform.MetaPlus  
-- import Uniform.Latex
import Uniform.Pandoc 

import qualified Data.Map as M
import Data.Default.Class ( Default(def) ) -- to define a default class for siteLayout 
import Path (parent)


progName, progTitle :: Text
progName = "daino"  
progTitle = "constructing a static site generator" :: Text

settingsFileName :: Path Rel File
-- ^ the yaml file in which the siteHeader are fixec
settingsFileName = makeRelFile "settings3" -- the yaml file

-- | description for each md file 
-- todo include the flags 
type DainoMetaPlus = MetaPlus Settings DainoValues
-- data DainoMetaPlus = DainoMetaPlus 
--         { metap :: Meta    -- ^ the pandoc meta, the file text content
--         , sett :: Settings -- ^ the data from the settingsfile
--         , extra :: DainoValues -- ^ other values to go into template
--         , metaMarkdown :: M.Map Text Text -- todo not used
--         , metaHtml ::  M.Map Text Text
--         , metaLatex ::  M.Map Text Text
--         }
--     deriving (Eq, Ord, Show, Read, Generic) -- Zeros, ToJSON, FromJSON)
        
-- | the siteHeader file with all fields 
data Settings = Settings
    { siteLayout :: SiteLayout 
    , localhostPort :: Int 
    , settingsAuthor :: Text 
    , settingsDate :: Text -- should be UTC 
    , siteHeader :: SiteHeader 
    , menuitems :: MenuItems
    -- , today :: Text
    } deriving (Show, Read, Ord, Eq, Generic)

instance Zeros Settings where zero = Settings zero zero zero zero zero zero
instance ToJSON Settings
instance FromJSON Settings

-- the extraValues will eventually go into settings
data DainoValues = 
    DainoValues 
            { mdFile:: FilePath -- Path Abs File -- abs file path 
            , mdRelPath :: FilePath -- Path Rel File  -- rel file path
            , textual0md :: TextualIx Text  -- | the textual content in different reps for this file
            , textual0pan :: TextualIx MetaValue   
            , textual0html :: TextualIx Text
            , textual0tex :: TextualIx Text
            -- index entries are for subordinated dirs and files
            -- filled in docrep2panrep
            , dirEntries :: [IndexEntry2] 
            , fileEntries :: [IndexEntry2] 
                    -- only the dirs and files path
            , dainoVersion :: Text 
            , latLanguage :: Text 
            , authorReduced :: Text
            -- , extraBakedDir :: Text
            , bookBig :: Bool -- values, because the template system limitation
            , booklet :: Bool
            , bookprint :: Bool  -- include the empty pages for print version
            , webroot :: Text  -- the webroot
            , pdf2 :: FilePath
            }
    deriving (Eq, Ord, Show, Read, Generic, Zeros)


-- instance Zeros DainoValues where 
--     zero = DainoValues zero  zero zero  zero zero  zero zero
instance ToJSON DainoValues 
instance FromJSON DainoValues 

-- this was defined in uniform-latex 
-- there renamed to indexEntryRenamed 

data IndexEntry2 = IndexEntry2 
    { -- | the rel file path, for dirs is dir/index, to be file
            -- without extension, relative to webroot
      ixfn :: FilePath -- Path Rel File
    -- , -- | the link for this page (relative to web root)
    -- -- without an extension or filename for dir}
    --   link :: Path Rel Dir
    , textualPan :: TextualIx MetaValue    
    -- , textualMd :: TextualIx Text   -- | the textual content in different reps
    , textualHtml :: TextualIx Text
    , textualTex :: TextualIx Text
    -- , title :: Text
    -- , abstract :: Text
    -- , author :: Text
    , date :: Text
    -- , content :: Text   -- in latex style, only filled bevore use
    , visibility ::   Text
    , version ::   Text 
    , sortOrder :: Text
    , pdf1 :: Text
     -- , indexPage :: Bool
    -- , dirEntries :: [FilePath] -- [Path Abs Dir] -- [IndexEntry2] -- def []
    -- , fileEntries :: [FilePath] -- [Path Abs File] -- [IndexEntry2] -- def []
    , headerShift :: Int   
    } deriving (Show, Read, Eq, Ord, Generic, Zeros)
    --  IndexTitleSubdirs | IndexTitleFiles 

-- instance Zeros IndexEntry2 where zero = IndexEntry2 [] []
-- zero zero zero zero zero zero zero
instance Zeros Block where zero = Plain []
instance Zeros MetaValue where zero = MetaString "zero"
instance ToJSON IndexEntry2
instance FromJSON IndexEntry2

isIndexPage :: Path Abs File -> Bool 
isIndexPage filename =  getNakedFileName filename == "index"

-- | textual content in the index in different representations, each
data TextualIx v = TextualIx 
    { title :: v
    , abstract :: v
    , author :: v
    -- , content :: v   -- in latex style, only filled bevore use
    } deriving (Show, Read, Eq, Ord, Generic, Zeros)

instance ToJSON v => ToJSON (TextualIx v)
instance FromJSON v => FromJSON (TextualIx v)


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
sourceDirTestSite = sourceDirTestDocs `addDir` (makeRelDir "site")
-- ^ the dir with the source for the test site

layoutDefaults :: Path Abs Dir -> Path Abs Dir ->  SiteLayout
-- used for finding the test cases
-- must correspond to the settings3.yaml in source code repository
-- fix this later for use in testing todo 
layoutDefaults dough4test homeDir1 =
    zero -- SiteLayout
        { doughDir = dough4test
        , bakedDir = homeDir1 `addDir` makeRelDir "bakedTestSite" :: Path Abs Dir
        ,  themeDir = (parent (parent dough4test)) `addDir` makeRelDir "theme"
 
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
            `addDir` (makeRelDir templatesName)

blankAuthorName :: [Text] -> Text -> Text 
-- suppress/oppress author name, if the author name is the same as one in the first arg (AUF, Andrew U..) then set it to empty else copy 
-- goal is to avoid to have each page say the obvious "author XX"
blankAuthorName names current = 
    if current `elem` names 
        then zero 
        else current 


putInform :: MonadIO m => NoticeLevel -> [Text] -> m () 
-- produce output if debug > NoticeLevel0 
putInform debug texts = when  (inform debug)  $ 
        putIOwords texts

