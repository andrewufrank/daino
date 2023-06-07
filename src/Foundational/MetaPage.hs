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
{-# LANGUAGE InstanceSigs #-}

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
import Uniform.Latex 
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
     -- | the fields of miniblog
    , dyLang :: Text -- DocLanguage not used yet 
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
    -- when references are given in markdown text
    , dyReference_section_title :: Text 
            --set default always, suppressed when not needed
    , dyContentFiles :: [Text] -- the list of md files to include 
    , dyNoCite :: Maybe Text
    , dyBook :: Text -- "booklet" to produce collection pdf
    , dyVersion ::  Text -- should be "publish"
    , dyVisibility ::  Text -- should be "public"
    -- , dyIndexPage :: Bool
    , dyIndexSort :: Maybe Text
    , dyIndexEntry :: IndexEntry
    -- , dyDirEntries   :: [IndexEntry]  -- reduce to one for indexEntry
    -- , dyFileEntries  :: [IndexEntry]
    -- is defined later, necessary here?
    , dyDoNotReplace:: [Text]
    , dyHeaderShift :: Int -- pandoc seems not to parse int in the yaml, mark, values    'zero' or 'one' 
    -- shift the header level, such that one # is hl2,
    -- because hl1 is title
    }
    deriving (Show, Eq, Generic, Zeros, Read) -- ord missing for references
instance Zeros Integer where zero = 0

-- instance Default MetaPage where
--     -- how is this used - defaults are set in pandoc2MetaPage
--     def :: MetaPage
--     def =
--         zero
--             { dyFn = zero
--             , dyLink = zero
--             , dyLang = "en_US" -- DLenglish
--             , dyTitle = "FILL_dytitle"
--             , dyAbstract = zero
--             , dyAuthor = "AOS"
--             , dyDate = Just . showT $ year2000
--             , dyKeywords = zero
--             , dyBibliography = Just "resources/BibTexLatex.bib"
--             , dyStyle = Just "chicago-fullnote-bibliography-bb.csl"
--             , dyStyleBiber = "authoryear"
--             , dyReferences = Nothing
--             , dyReference_section_title = "References"
--             , dyVersion =  "private"
--             , dyVisibility =  "draft"
--             -- , dyIndexPage = False
--             , dyIndexSort = zero
--             , dyIndexEntry = zero
--             -- , dyDirEntries = zero
--             -- , dyFileEntries = zero
--             , dyHeaderShift = zero 
--             }

docyamlOptions :: Options
docyamlOptions =
    defaultOptions
        { fieldLabelModifier = t2s . toLowerStart . s2t . drop 2
        }

instance ToJSON MetaPage where
    toJSON = genericToJSON docyamlOptions

instance FromJSON MetaPage where
    parseJSON = genericParseJSON docyamlOptions

pandoc2MetaPage::  Settings ->  Path Abs File  -> Pandoc -> MetaPage
-- removed most default values, left only for image and caption, keywords
pandoc2MetaPage sett3 filename  pd =  meta6
  where

    layout = siteLayout sett3
    doughP = doughDir layout
    defAuthor = defaultAuthor layout 
    -- defBiblio = defaultBibliography layout  

    meta2 = flattenMeta . getMeta $ pd
    relfn = makeRelativeP doughP filename
    -- fromJustN means give error if not present!
    meta4 =
        MetaPage
            { dyFn = toFilePath filename
            , dyLink = toFilePath relfn
            , dyLang = fromMaybe "en_US" $ getAtKey meta2 "language"
            , dyTitle = fromJustN "title" $ getAtKey meta2 "title"
            -- , dyAbstract = fromJustN "abstract" $ getAtKey meta2 "abstract"
            , dyAbstract = fromMaybe "" $ getAtKey meta2 "abstract" 
                    -- allow empty abstract
            -- , dyAuthor = fromJustN "author" $ getAtKey meta2 "author"
            , dyAuthor = fromMaybe defAuthor $ getAtKey meta2 "author"
            -- allow empty author ??
            , dyDate = getAtKey meta2 "date"
            , dyBibliography = getAtKey meta2 "bibliography"
            -- not defaulted, value used is the one read into pandoc
            -- , dyBibliography = Just $ fromMaybe defBiblio $ getAtKey meta2 "bibliography"
            -- used as signal for processing biblio
            -- perhaps not the best idea? 
            , dyImage = fromMaybe "" $ getAtKey meta2 "image"
            , dyImageCaption = fromMaybe "" $ getAtKey meta2 "caption"
            , dyKeywords = fromMaybe "" $ getAtKey meta2 "keywords"
            -- , dyKeywords = fromJustN "keywords" $ getAtKey meta2 "keywords"
            , dyStyle = Just $ fromMaybe "resources/chicago-fullnote-bibliography-bb.csl" $ getAtKey meta2 "style"
            , dyStyleBiber = fromMaybe "authoryear" $ getAtKey meta2 "styleBiber"
            , dyNoCite = getAtKey meta2 "nocite"
            , dyReferences = getAtKey meta2 "references"
            , dyReference_section_title= fromMaybe "References" $ getAtKey meta2 "reference-section-title"
            -- default should be set depending on the language 
            -- default does not work, needs to be put into yaml 
            , dyContentFiles = maybeToList  . getAtKey meta2 $ "content"
            -- TODO make reading a list
            , dyBook = fromMaybe "" $ getAtKey meta2 "book"
            --  to indicate to collect all lower files in a single pdf 
            , dyVersion = fromJustN "version" $ getAtKey meta2 "version"  -- no default here, must be present 
            , dyVisibility = fromMaybe "public" $ getAtKey meta2 "visibility"   
            -- public is default, private must be set
            -- but not default for publish! 
            --   dyIndexPage = fromMaybe False $ getAtKey meta2 "indexPage"
            , dyIndexSort = getAtKey meta2 "indexSort"
            , dyIndexEntry =   zero
            , dyHeaderShift = parseHeaderShift $ getAtKey  meta2 $ "headerShift"
                                        -- value 1 is correct
            , dyDoNotReplace = maybe [] words' $ getAtKey meta2 $ "doNotReplace"
            -- , dyDoNotReplace = maybe [] (\t -> fromJustNote "sdfwer" $ splitOnflip  "," t) $ getAtKey meta2 $ "doNotReplace"
            }

    -- splitOnflip sep inp = splitOn' inp sep 

    ix1 =  initializeIndex meta4
    -- meta5 = meta4   -- {dyAuthor=blankAuthorName hpnames (dyAuthor meta4) }
    meta6 = meta4{dyIndexEntry = ix1} 

    fromJustN :: Text -> Maybe a -> a 
    fromJustN a = fromJustNoteT ["fromJust Nothing pandoc2MetaPage\n", showT filename, "\n", a]

    parseHeaderShift :: Maybe Text -> Int 
    parseHeaderShift Nothing = 1   -- this is the default
    parseHeaderShift (Just "zero") = 0 
    parseHeaderShift (Just "one") = 1 
    parseHeaderShift (Just "0") = 0 
    parseHeaderShift (Just "1") = 1 
    parseHeaderShift (Just a) = errorT ["parseHeaderShift", "unexpected Value", a, "!"] 

    -- fromJust Nothing = errorT ["fromJust Nothing pandoc2MetaPage", showT filename]
    -- fromJust (Just a) = a
 

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
            , content = zero
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
