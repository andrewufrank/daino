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
-- import Foundational.Filetypes4sites(extPDF, extHTML)
-- import Lib.CmdLineArgs (PubFlags (..))
import Uniform.Json
import Uniform.Shake
import Uniform.Pandoc
-- import Uniform.PandocImports
-- import Uniform.Shake (makeRelativeP)
-- import Uniform.Filenames (setFileExtension)
import Uniform.Yaml
import UniformBase
-- import Uniform.HTMLout (extHTML)
-- import Uniform.Json ( gak, AtKey(getAtKey) )

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
    , dyImage ::  Text  -- empty if nothing given
    , dyImageCaption :: Text 
    , dyBibliography :: Maybe Text
    -- a bibliography is trigger to process
    , dyStyle :: Maybe Text
    , dyStyleBiber :: Text
    , dyReferences :: Maybe Value --  [Reference]
    , dyContentFiles :: [Text] -- the list of md files to include 
    , dyNoCite :: Maybe Text
    , dyPublish ::  Text -- should be "publish"
    , dyPP ::  Text -- should be "public"
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
            , dyTitle = "FILL_dytitle"
            , dyAbstract = zero
            , dyAuthor = "Andrew U Frank"
            , dyDate = Just . showT $ year2000
            , dyKeywords = zero
            , dyBibliography = Just "resources/BibTexLatex.bib"
            , dyStyle = Just "chicago-fullnote-bibliography-bb.csl"
            , dyStyleBiber = "authoryear"
            , dyPublish =  "private"
            , dyPP =  "draft"
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

pandoc2MetaPage::  Path Abs Dir ->  Path Abs File -> Pandoc -> MetaPage
pandoc2MetaPage doughP filename pd =  meta6

  where
    meta2 = flattenMeta . getMeta $ pd
    relfn = makeRelativeP doughP filename
    meta4 =
        MetaPage
            { dyFn = toFilePath filename
            , dyLink = toFilePath relfn
            , dyLang = "en_US" -- DLenglish -- getAtKey meta2 "language"
            , dyTitle = fromMaybe "FILL_dyTitle2" $ getAtKey meta2 "title"
            , dyAbstract = fromMaybe "FILL_dyAbstract" $ getAtKey meta2 "abstract"
            , dyAuthor = fromMaybe "FILL_dyAuthor" $ getAtKey meta2 "author"
            , dyDate = getAtKey meta2 "date"
            , dyBibliography = getAtKey meta2 "bibliography"
            -- used as signal for processing biblio
            , dyImage = fromMaybe "" $ getAtKey meta2 "image"
            , dyImageCaption = fromMaybe "" $ getAtKey meta2 "caption"
            , dyKeywords = fromMaybe "" $ getAtKey meta2 "keywords"
            , dyStyle = getAtKey meta2 "style"
            , dyStyleBiber = fromMaybe "authoryear" $ getAtKey meta2 "styleBiber"
            , dyNoCite = getAtKey meta2 "nocite"
            , dyReferences = gak meta2 "references"
            , dyContentFiles = maybeToList  . getAtKey meta2 $ "content"
            -- TODO make reading a list
            , dyPublish = fromMaybe "publish" $ getAtKey meta2 "publish"  -- questionalbe default
            , dyPP = fromMaybe "public" $ getAtKey meta2 "pp" -- questionalbe default 
            -- , -- TODO use pbulicationState
            --   dyIndexPage = fromMaybe False $ getAtKey meta2 "indexPage"
            , dyIndexSort = getAtKey meta2 "indexSort"
            , dyIndexEntry =   zero
                                        -- else zero
            }
            
    ix1 =  initializeIndex meta4
    meta6 = meta4{dyIndexEntry = ix1} 
 
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
    -- , publish :: Maybe Text
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
            { ixfn = dyFn  
            , title = dyTitle
            , link = dyLink 
            , abstract = dyAbstract
            , author = dyAuthor
            , date = fromMaybe (showT year2000) dyDate
            -- , publish = dyPublish
            , dirEntries = zero
            , fileEntries = zero
            }

isIndexPage :: Path Abs File -> Bool 
isIndexPage filename =  getNakedFileName filename == "index"

convertLink2html ix = s2t . -- s2t . toFilePath $ 
          setExtension (unExtension extHTML)   $ link ix

convertLink2pdf ix =  s2t . -- s2t . toFilePath $ 
          setExtension (unExtension extPDF)   $ link ix


extHTML :: Extension
extHTML = Extension "html"


extPDF :: Extension
extPDF = Extension "pdf"