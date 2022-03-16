{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
---------------------------------------------------------------
--
-- MetaPage   : The information taken from the yaml header of each md file. 
--  LayoutFlags deals with the data from the settingsN.yaml
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

import Data.Default.Class  
import Foundational.SettingsPage  
import Uniform.Json
import Uniform.Shake
import Uniform.Pandoc
import Uniform.Yaml
import Uniform.HTMLout (extHTML)

-- the fields in the yaml header of each md file 
-- maybe values can be empty

data MetaPage = MetaPage
    { -- | the original dough fn
      dyFn :: FilePath
    , -- | the relative filename
      -- relative to the browser origin
      -- set in ? initializeIndex
      dyLink :: FilePath
    , -- | the fields of miniblog
      dyLang :: Text -- DocLanguage not used yet 
    , dyTitle :: Text  -- must be set 
    , dyAbstract :: Text -- must be set 
    , dyAuthor :: Text  -- default ?
    , -- | this is maybe a string,
      --  should be utctime
      dyDate :: Maybe Text -- must be set 
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
    , dyVersion ::  Text -- should be "publish"
    , dyVisibility ::  Text -- should be "public"
    -- , dyIndexPage :: Bool
    , dyIndexSort :: Maybe Text
    , dyIndexEntry :: IndexEntry
    -- , dyDirEntries   :: [IndexEntry]  -- reduce to one for indexEntry
    -- , dyFileEntries  :: [IndexEntry]
    -- is defined later, necessary here?
    , dyHeaderShift :: Int -- pandoc seems not to parse int in the yaml, mark 'zero' or 'one' 
    -- shift the header level, that one # is hl2,
    -- because hl1 is title
    }
    deriving (Show, Eq, Generic, Zeros, Read) -- ord missing for references
instance Zeros Integer where zero = 0

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
            , dyVersion =  "private"
            , dyVisibility =  "draft"
            -- , dyIndexPage = False
            , dyIndexSort = zero
            , dyIndexEntry = zero
            -- , dyDirEntries = zero
            -- , dyFileEntries = zero
            , dyHeaderShift = zero 
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

pandoc2MetaPage::  Path Abs Dir ->  Path Abs File  -> Pandoc -> MetaPage
-- removed most default values, left only for image and caption, keywords
pandoc2MetaPage doughP filename  pd =  meta6
  where
    meta2 = flattenMeta . getMeta $ pd
    relfn = makeRelativeP doughP filename
    meta4 =
        MetaPage
            { dyFn = toFilePath filename
            , dyLink = toFilePath relfn
            , dyLang = "en_US" -- DLenglish -- getAtKey meta2 "language"
            , dyTitle = fromJustN "title" $ getAtKey meta2 "title"
            , dyAbstract = fromJustN "abstract" $ getAtKey meta2 "abstract"
            , dyAuthor = fromJustN "author" $ getAtKey meta2 "author"
            , dyDate = getAtKey meta2 "date"
            , dyBibliography = getAtKey meta2 "bibliography"
            -- used as signal for processing biblio
            , dyImage = fromMaybe "" $ getAtKey meta2 "image"
            , dyImageCaption = fromMaybe "" $ getAtKey meta2 "caption"
            , dyKeywords = fromJustN "keywords" $ getAtKey meta2 "keywords"
            , dyStyle = getAtKey meta2 "style"
            , dyStyleBiber = fromMaybe "authoryear" $ getAtKey meta2 "styleBiber"
            , dyNoCite = getAtKey meta2 "nocite"
            , dyReferences = gak meta2 "references"
            , dyContentFiles = maybeToList  . getAtKey meta2 $ "content"
            -- TODO make reading a list
            , dyVersion = fromJustN "version" $ getAtKey meta2 "version"  -- questionalbe default
            , dyVisibility = fromJustN "visibility" $ getAtKey meta2 "visibility"   
            -- , -- TODO use pbulicationState
            --   dyIndexPage = fromMaybe False $ getAtKey meta2 "indexPage"
            , dyIndexSort = getAtKey meta2 "indexSort"
            , dyIndexEntry =   zero
            , dyHeaderShift = parseHeaderShift $ getAtKey  meta2 $ "headerShift"
                                        -- value 1 is correct
            }
            
    ix1 =  initializeIndex meta4
    -- meta5 = meta4   -- {dyAuthor=blankAuthorName hpnames (dyAuthor meta4) }
    meta6 = meta4{dyIndexEntry = ix1} 
    fromJustN :: Text -> Maybe a -> a 
    fromJustN a = fromJustNoteT ["fromJust Nothing pandoc2MetaPage\n", showT filename, "\n", a]

    parseHeaderShift :: Maybe Text -> Int 
    parseHeaderShift Nothing = 0 
    parseHeaderShift (Just "zero") = 0 
    parseHeaderShift (Just "one") = 1 

    -- fromJust Nothing = errorT ["fromJust Nothing pandoc2MetaPage", showT filename]
    -- fromJust (Just a) = a
 
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
    , headerShift :: Int   
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
            -- , publish = dyVersion
            , dirEntries = zero
            , fileEntries = zero
            , headerShift = dyHeaderShift
            }

isIndexPage :: Path Abs File -> Bool 
isIndexPage filename =  getNakedFileName filename == "index"

convertLink2html ix = s2t . -- s2t . toFilePath $ 
          setExtension (unExtension extHTML)   $ link ix

convertLink2pdf ix =  s2t . -- s2t . toFilePath $ 
          setExtension (unExtension extPDF)   $ link ix


-- extHTML :: Extension
-- extHTML = Extension "html"


extPDF :: Extension
extPDF = Extension "pdf"