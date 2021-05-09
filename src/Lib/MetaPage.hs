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
module Lib.MetaPage where

import Data.Aeson.Types
import Data.Default (Default (..))
import Lib.CmdLineArgs (PubFlags (..))
import Lib.Foundation (SiteLayout (..))
import Uniform.Json
import Uniform.Pandoc
import Uniform.PandocImports
import Uniform.Shake (makeRelativeP)
import UniformBase

data MetaPage = MetaPage
    { dyFn :: FilePath -- the original dough fn
    , dyLink :: FilePath -- the relative filename
    , dyLang :: DocLanguage
    , -- the fields of miniblog
      dyTitle :: Text
    , dyAbstract :: Text
    , dyAuthor :: Text
    , -- | this is maybe a string,
      --  should be utctime
      dyDate :: Maybe Text
    , dyKeywords :: Text -- should be [Text]
    , dyBibliography :: Maybe Text
    , dyStyle :: Maybe Text
    , dyPublish :: Maybe Text
    , dyIndexPage :: Bool
    , dyDirEntries :: [IndexEntry]
    , dyFileEntries :: [IndexEntry]
    -- is defined later, necessary here?
    }
    deriving (Show, Ord, Eq, Generic, Zeros, Read) --Read,

-- instance Zeros MetaPage where
--     zero = MetaPage zero
--                    zero
--                    DLenglish
--                    zero
--                    zero
--                    zero
--                    zero
--                    zero
--                    zero
--                    zero
--                    zero
--                    zero
--    []
--    []

instance Default MetaPage where
    def =
        zero
            { dyFn = zero
            , dyLink = zero
            , dyLang = DLenglish
            , dyTitle = "FILL"
            , dyAbstract = zero
            , dyAuthor = "Andrew U Frank"
            , dyDate = Just . showT $ year2000
            , dyKeywords = zero
            , dyBibliography = Just "BibTexLatex.bib"
            , dyStyle = Just "chicago-fullnote-bibliography-bb.csl"
            , dyPublish = Nothing
            , dyIndexPage = False
            , dyDirEntries = zero
            , dyFileEntries = zero
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

-- generic works only when all fields are present
-- merge with metarec definition later in file
-- default values are set here and missing values to Nothing
-- parseYam ::   Value -> Parser

-- parseJSONyaml (Object o) =
--   -- withObject "person" $ \o ->
--   -- the yam part is an object
--   -- these are the required fields
--   -- at the moment default language is DLenglish
--   do
--     dyTitle <- o .: "title"
--     dyAbstract <- o .: "abstract"
--     dyAuthor <- o .:? "author" .!= ""
--     dyLang <- o .:? "lang" .!= DLenglish -- x^ default
--     dyKeywords <- o .: "keywords"
--     dyDate <- o .:? "date"
--     dyFn <- o .:? "fn" .!= "" -- x^ as a default, is overwritten but avoids error msg
--     dyLink <- o .:? "link" .!= "" -- x^ the relative link for html, derive from fn
--     dyBibliography <- o .:? "bibliography"
--     -- the bib file if needed
--     dyStyle <- o .:? "style" -- x^ the csl file
--     dyPublish <- o .:? "publish" --  .!= Nothing
--     dyIndexPage <- o .:? "indexPage" .!= False
--     -- dyDirEntries   <- o .:? "dirEntries" .!= []
--     -- dyFileEntries  <- o .:? "fileEntries" .!= []
--     return MetaPage {..}

-- completeMetaPage :: Path Abs Dir -> Path Abs Dir -> Path Abs File -> Value -> ErrIO MetaPage
-- -- there is no requirement of completeness of the values set
-- -- all can be defaulted (what about title? )
-- -- complete the meta yaml data
-- -- test for completeness of metadata in yaml
-- -- fails if required labels are not present
-- -- adds defaults when
-- -- sets filename
-- completeMetaPage doughP bakedP fn y1 = do
--   putIOwords ["completeMetaPage start"]
--   let resdy =
--         parseEither parseJSONyaml y1 ::
--           Either String MetaPage
--   case resdy of
--     Left msg ->
--       errorT
--         [ "completeMetaPage not all required fields",
--           s2t msg,
--           "in file",
--           showT fn
--         ]
--     Right resdy1 -> do
--       -- heute <- getCurrentTimeUTC
--       let nakFn = getNakedFileName fn
--       let resdy2 =
--             resdy1
--               { dyFn = toFilePath fn,
--                 dyLink = toFilePath $ makeRelativeP doughP fn,
--                 dyStyle = addBakedRoot bakedP (dyStyle resdy1),
--                 dyBibliography =
--                   addBakedRoot
--                     bakedP
--                     (dyBibliography resdy1),
--                 dyIndexPage = (nakFn == "index") || dyIndexPage resdy1
--               }
--       when False $ putIOwords ["completeMetaPage 1 resdy2", showT resdy2]
--       -- dy <- case resdy of
--       --         Error msg -> error msg
--       --         Success a -> return a
--       let dy = resdy2
--       putIOwords ["completeMetaPage dy", showT dy]
--       return dy

addFileMetaPage :: Path Abs Dir -> Path Abs Dir -> Path Abs File -> MetaPage
addFileMetaPage doughP bakedP fn =
    if getNakedFileName fn == "index"
        then mp1{dyIndexPage = True}
        else mp1
  where
    mp1 =
        zero
            { dyFn = toFilePath fn
            , dyLink =
                toFilePath
                    (makeRelativeP doughP fn :: Path Rel File)
                    --  , dyStyle =  addBakedRoot bakedP ( dyStyle resdy1)
                    --  , dyBibliography = addBakedRoot bakedP                                           (dyBibliography resdy1)
                    -- , dyIndexPage = ((getNakedFileName fn) == "index") || dyIndexPage resdy1
            } ::
            MetaPage

addBakedRoot :: Path Abs Dir -> Maybe Text -> Maybe Text
addBakedRoot bakedP Nothing = Nothing
addBakedRoot bakedP (Just fp) = Just . s2t . toFilePath $ addFileName bakedP . t2s $ fp

-- | another data type to rep languages
data DocLanguage = DLgerman | DLenglish
    deriving (Show, Read, Ord, Eq, Generic)

instance Zeros DocLanguage where zero = DLenglish

instance FromJSON DocLanguage

instance ToJSON DocLanguage

-- is this clever to have a new language datatype?

data PublicationState = PSpublish | PSdraft | PSold | PSzero
    deriving (Generic, Show, Read, Ord, Eq)
-- ^ is this file ready to publish

instance Zeros PublicationState where
    zero = PSzero

instance NiceStrings PublicationState where
    shownice = drop' 2 . showT

instance ToJSON PublicationState

instance FromJSON PublicationState

data IndexEntry = IndexEntry
    { -- | the abs file path
      fn :: FilePath -- to have read statt Path Abs File
    , -- | the link for this page (relative)}
      link :: FilePath
    , title :: Text
    , abstract :: Text
    , author :: Text
    , date :: Text
    , publish :: Maybe Text
    , indexPage :: Bool
    , dirEntries :: [IndexEntry] -- def []
    , fileEntries :: [IndexEntry] -- def []
    }
    deriving (Show, Read, Eq, Ord, Generic) -- Read,

instance ToJSON IndexEntry
instance FromJSON IndexEntry
