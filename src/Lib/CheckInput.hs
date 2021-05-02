------------------------------------------------------------------------------
--
-- Module      :  check all inputs and produce a record summary
-- puts the content of doc yaml header in to DocYaml
-- could the text, the pandoc etc. all go there? 
-- fills the DocYaml with defaults and the filename 

-- if more data are needed to describe an entry then add it here!

-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# OPTIONS -fno-warn-unused-matches #-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Lib.CheckInput where

import GHC.Generics ( Generic )
import Data.Default ( Default(..) )
import Uniform.Shake (makeRelativeP)
import Lib.Foundation ()
import Uniform.DocRep ( DocRep(DocRep) )
import UniformBase
    ( Generic,
      NiceStrings(shownice),
      Path,
      Abs,
      Dir,
      File,
      Text,
      when,
      toFilePath,
      Value(Object),
      ToJSON(toJSON),
      FromJSON(parseJSON),
      errorT,
      s2t,
      t2s,
      putIOwords,
      showT,
      toLowerStart,
      Zeros(zero),
      ErrIO,
      Filenames1(getNakedFileName),
      Filenames3(addFileName),
      CharChains(drop'),
      genericParseJSON,
      defaultOptions,
      genericToJSON,
      mergeLeftPref,
      fieldLabelModifier )
import           Data.List                      ( (\\) )
import Data.Aeson
    ( Value(Object),
      ToJSON(toJSON),
      FromJSON(parseJSON),
      genericParseJSON,
      defaultOptions,
      genericToJSON,
      Options(fieldLabelModifier),
      (.!=),
      (.:),
      (.:?) )
import Data.Aeson.Types
    ( Value(Object),
      ToJSON(toJSON),
      FromJSON(parseJSON),
      genericParseJSON,
      defaultOptions,
      genericToJSON,
      Options(fieldLabelModifier),
      parseEither,
      (.!=),
      (.:),
      (.:?) )
import qualified Data.Map                      as M

data DocLanguage = DLgerman | DLenglish
        deriving (Show, Read, Ord, Eq, Generic)
instance FromJSON DocLanguage
instance ToJSON DocLanguage
    -- is this clever to have a new language datatype? 

checkDocRep :: Path Abs Dir -> Path Abs Dir -> Path Abs File -> DocRep -> ErrIO DocRep
-- check the DocRep 
-- the bakedP root is necessary to complete the style and bib entries
-- as well as image? 
-- first for completeness of metadata in yaml 
-- fails if required labels are not present
checkDocRep doughP bakedP fn (DocRep y1 p1) = do
    y2 <- checkDocRep1 doughP bakedP fn y1
    let y3 = mergeLeftPref [toJSON y2, y1]
    putIOwords ["checkDocRep", "y3", showT y3]
    return (DocRep y3 p1)

data IndexEntry = IndexEntry
                    { fn :: Path Abs File
                        -- ^ the abs file path 
                    , link :: FilePath
                        -- ^ the link for this page (relative)}
                    , title :: Text
                    , abstract :: Text
                    , author :: Text
                    , date :: Text
                    , publish :: Maybe Text
                    , indexPage :: Bool
                    , dirEntries :: [IndexEntry]  -- def []
                    , fileEntries :: [IndexEntry] -- def []
                    } deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON IndexEntry
instance FromJSON IndexEntry



data DocYaml = DocYaml {dyFn :: FilePath  -- the original dough fn 
                        , dyLink :: FilePath -- the relative filename
                        , dyLang :: DocLanguage
                        -- the fields of miniblog
                        , dyTitle :: Text
                        , dyAbstract :: Text

                        , dyAuthor :: Text
                        , dyDate :: Maybe Text
                        -- ^ this is maybe a string, 
                        --  should be utctime 
                        , dyKeywords :: Text  -- should be [Text]
                        , dyBibliography :: Maybe Text
                        , dyStyle :: Maybe Text

                        , dyPublish :: Maybe Text
                        , dyIndexPage :: Bool
                        , dyDirEntries :: [IndexEntry]
                        , dyFileEntries :: [IndexEntry]


            } deriving (Show, Read, Ord, Eq, Generic)

instance Zeros DocYaml where
    zero = DocYaml zero
                   zero
                   DLenglish
                   zero
                   zero
                   zero
                   zero
                   zero
                   zero
                   zero
                   zero
                   zero
                   []
                   []
instance Default DocYaml where
    def = zero { dyLang = DLenglish }

docyamlOptions =
    defaultOptions
        {fieldLabelModifier = t2s . toLowerStart . s2t . drop 2 }
instance ToJSON DocYaml where
    toJSON = genericToJSON docyamlOptions

instance FromJSON DocYaml where
    parseJSON = genericParseJSON docyamlOptions


    -- generic works only when all fields are present
    -- merge with metarec definition later in file 
    -- default values are set here and missing values to Nothing
-- parseYam ::   Value -> Parser

parseJSONyaml (Object o) = -- withObject "person" $ \o -> 
  -- the yam part is an object  
  -- these are the required fields  
  -- at the moment default language is DLenglish 
                           do
    dyTitle        <- o .: "title"
    dyAbstract     <- o .: "abstract"
    dyAuthor       <- o .:? "author" .!= ""
    dyLang         <- o .:? "lang" .!= DLenglish  -- ^ default 
    dyKeywords     <- o .: "keywords"
    dyDate         <- o .:? "date"
    dyFn           <- o .:? "fn" .!= ""  -- ^ as a default, is overwritten but avoids error msg
    dyLink         <- o .:? "link" .!= ""  -- ^ the relative link for html, derive from fn
    dyBibliography <- o .:? "bibliography"
        -- the bib file if needed  
    dyStyle        <- o .:? "style" -- ^ the csl file 

    dyPublish      <- o .:? "publish"  --  .!= Nothing 
    dyIndexPage  <- o .:? "indexPage" .!= False
    dyDirEntries   <- o .:? "dirEntries" .!= []
    dyFileEntries  <- o .:? "fileEntries" .!= []
    return DocYaml { .. }

checkDocRep1 :: Path Abs Dir -> Path Abs Dir -> Path Abs File -> Value -> ErrIO DocYaml
-- check the DocRep 
-- first for completeness of metadata in yaml 
-- fails if required labels are not present
-- sets filename 
checkDocRep1 doughP bakedP fn y1 = do
    putIOwords ["checkDocRep1 start"]
    let resdy = parseEither parseJSONyaml y1
            :: Either String DocYaml
    case resdy of
        Left msg ->
            errorT
                [ "checkDocRep1 not all required fields"
                , s2t msg
                , "in file"
                , showT fn
                ]
        Right resdy1 -> do
            -- heute <- getCurrentTimeUTC 
            let nakFn = getNakedFileName fn
            let resdy2 = resdy1
                    { dyFn = toFilePath fn
                      , dyLink = toFilePath $ makeRelativeP doughP fn
                     , dyStyle =  addBakedRoot bakedP ( dyStyle resdy1)
                     , dyBibliography = addBakedRoot bakedP
                                          (dyBibliography resdy1)
                    , dyIndexPage = (nakFn == "index") || dyIndexPage resdy1
                    }
            when False $ putIOwords ["checkDocRep1 1 resdy2", showT resdy2]
            -- dy <- case resdy of 
            --         Error msg -> error msg 
            --         Success a -> return a 
            let dy = resdy2
            putIOwords ["checkDocRep1 dy", showT dy]
            return dy

addBakedRoot :: Path  Abs Dir -> Maybe Text -> Maybe Text
addBakedRoot bakedP Nothing = Nothing
addBakedRoot bakedP (Just fp) = Just. s2t . toFilePath $ addFileName bakedP . t2s $ fp


data PublicationState = PSpublish | PSdraft | PSold | PSzero
                  deriving (Generic,  Show, Read, Ord, Eq)
-- ^ is this file ready to publish

instance Zeros PublicationState where
  zero = PSzero
instance NiceStrings PublicationState where
  shownice = drop' 2 . showT

instance ToJSON PublicationState
instance FromJSON PublicationState


