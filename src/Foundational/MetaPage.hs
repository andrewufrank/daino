{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
---------------------------------------------------------------
--
-- MetaPage   :
---------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans
            -fno-warn-missing-signatures
            -fno-warn-missing-methods
            -fno-warn-duplicate-exports
            -fno-warn-unused-imports
            -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{- | The data describing a page of the site (i.e. an md file)
 the default is merged with the values in the yaml head
 all entries there should be from this list
 all JSON related functions here!
-}
module Foundational.MetaPage (
    module Foundational.MetaPage,
    Default (..),
) where

-- import Data.Aeson.Types
import Data.Default.Class  
import Foundational.LayoutFlags  
-- import Lib.CmdLineArgs (PubFlags (..))
import Uniform.Json
import Uniform.Pandoc
import Uniform.PandocImports
import Uniform.Shake (makeRelativeP)
import Uniform.Yaml
import UniformBase

data MetaPage = MetaPage
    { -- | the original dough fn
      dyFn :: FilePath
    , -- | the relative filename
      -- relative to the browser origin
      -- set in ? initializeIndex
      dyLink :: FilePath
    , -- | the fields of miniblog
      dyLang :: Text -- DocLanguage not used yet 
    , dyTitle :: Text
    , dyAbstract :: Text
    , dyAuthor :: Text
    , -- | this is maybe a string,
      --  should be utctime
      dyDate :: Maybe Text
    , dyKeywords :: Text -- should be [Text]
    , dyBibliography :: Maybe Text
    , dyStyle :: Maybe Text
    , dyReferences :: Maybe Value --  [Reference]
    , dyNoCite :: Maybe Text
    , dyPublish :: Maybe Text -- not used yet!
    -- , dyIndexPage :: Bool
    , dyIndexSort :: Maybe Text
    , dyIndexEntry :: IndexEntry
    -- , dyDirEntries   :: [IndexEntry]  -- reduce to one for indexEntry
    -- , dyFileEntries  :: [IndexEntry]
    -- is defined later, necessary here?
    }
    deriving (Show, Eq, Generic, Zeros, Read) -- ord missing for references

instance Default MetaPage where
    def =
        zero
            { dyFn = zero
            , dyLink = zero
            , dyLang = "en_US" -- DLenglish
            , dyTitle = "FILL"
            , dyAbstract = zero
            , dyAuthor = "Andrew U Frank"
            , dyDate = Just . showT $ year2000
            , dyKeywords = zero
            , dyBibliography = Just "BibTexLatex.bib"
            , dyStyle = Just "chicago-fullnote-bibliography-bb.csl"
            , dyPublish = Nothing
            -- , dyIndexPage = False
            , dyIndexSort = zero
            , dyIndexEntry = zero
            -- , dyDirEntries = zero
            -- , dyFileEntries = zero
            }

docyamlOptions :: Options
docyamlOptions =
    defaultOptions
        { fieldLabelModifier = t2s . toLowerStart . s2t . drop 2
        }

instance ToJSON MetaPage where
    toJSON = genericToJSON docyamlOptions

instance FromJSON MetaPage where
    parseJSON = genericParseJSON docyamlOptions

-- addFileMetaPage :: Path Abs Dir -> Path Abs Dir -> Path Abs File -> MetaPage
-- addFileMetaPage doughP bakedP fn =
--     if getNakedFileName fn == "index"
--         then mp1{dyIndexPage = True}
--         else mp1
--   where
--     mp1 =
--         zero
--             { dyFn = toFilePath fn
--             , dyLink =
--                 toFilePath
--                     (makeRelativeP doughP fn :: Path Rel File)
--             , dyStyle =  addBakedRoot bakedP ( dyStyle zero)
--             , dyBibliography = addBakedRoot bakedP                                           (dyBibliography zero)
--             } ::
--             MetaPage

-- addBakedRoot :: Path Abs Dir -> Maybe Text -> Maybe Text
-- addBakedRoot bakedP Nothing = Nothing
-- addBakedRoot bakedP (Just fp) = Just . s2t . toFilePath $ addFileName bakedP . t2s $ fp

-- -- | another data type to rep languages
-- not yet used - go for de_AT style and uses standard lang codes
-- data DocLanguage = DLgerman | DLenglish
--     deriving (Show, Read, Ord, Eq, Generic)

-- instance Zeros DocLanguage where zero = DLenglish

-- instance FromJSON DocLanguage

-- instance ToJSON DocLanguage

-- TODO is this clever to have a new language datatype?

-- this is not used yet:
-- data PublicationState = PSpublish | PSdraft | PSold | PSzero
--     deriving (Generic, Show, Read, Ord, Eq)
-- -- ^ is this file ready to publish

-- instance Zeros PublicationState where
--     zero = PSzero

-- instance NiceStrings PublicationState where
--     shownice = drop' 2 . showT

-- instance ToJSON PublicationState

-- instance FromJSON PublicationState

data IndexEntry = IndexEntry 
    { -- | the abs file path
      ixfn :: FilePath -- Path Abs File
    , -- | the link for this page (relative to web root)}
      link :: FilePath -- Path Rel File
    , title :: Text
    , abstract :: Text
    , author :: Text
    , date :: Text
    , publish :: Maybe Text
    -- , indexPage :: Bool
    , dirEntries :: [IndexEntry] -- def []
    , fileEntries :: [IndexEntry] -- def []
    } deriving (Show, Read, Eq, Ord, Generic, Zeros)
    --  IndexTitleSubdirs | IndexTitleFiles 

-- instance Zeros IndexEntry where zero = IndexEntry zero zero zero zero zero zero zero zero zero

instance ToJSON IndexEntry
instance FromJSON IndexEntry

initializeIndex :: MetaPage -> IndexEntry
-- initialize the index with the values from the metapage yaml
initializeIndex MetaPage{..} = ix1
  where
    ix1 =
        zero
            { ixfn = dyFn -- makeAbsFile dyFn
            , title = dyTitle
            , link = dyLink --- makeRelFile dyLink
            , abstract = dyAbstract
            , author = dyAuthor
            , date = fromMaybe (showT year2000) dyDate
            , publish = dyPublish
            -- , indexPage = dyIndexPage
            , dirEntries = zero
            , fileEntries = zero
            }

isIndexPage :: Path Abs File -> Bool 
isIndexPage filename =  getNakedFileName filename == "index"
