---------------------------------------------------------------
--
-- MetaPage   :  
---------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass     #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports 
            -fno-warn-unused-imports 
            -fno-warn-unused-matches #-}
-- | The data describing a page of the site (i.e. an md file)
-- the default is merged with the values in the yaml head 
-- all entries there should be from this list
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib.MetaPage where


import Lib.CmdLineArgs (PubFlags (..))
import Lib.Foundation (SiteLayout (..))
import Uniform.Pandoc
import Uniform.Filetypes4sites
import           Uniform.PandocImports
import           Uniform.Json
import Data.Aeson.Types
import Data.Default ( Default(..) )
import Uniform.Shake (makeRelativeP)

import UniformBase  


data MetaPage = MetaPage {dyFn :: FilePath  -- the original dough fn 
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
                        -- , dyDirEntries :: [IndexEntry]
                        -- , dyFileEntries :: [IndexEntry]
                        -- is defined later, necessary here?


            } deriving (Show,  Ord, Eq, Generic, Zeros)  --Read,

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
    def = zero {dyFn = zero 
                , dyLink = zero
                , dyLang = DLenglish 
                , dyTitle = zero
                , dyAbstract = zero
                , dyAuthor = "Andrew U Frank"
                , dyDate = Just . showT $ year2000  
                , dyKeywords = zero
                , dyBibliography = Just "BibTexLatex.bib"
                , dyStyle = Just "chicago-fullnote-bibliography-bb.csl"
                , dyPublish = Nothing
                , dyIndexPage = False 
                -- , dyDirEntries = zero
                -- , dyFileEntries = zero
                }

docyamlOptions :: Options
docyamlOptions =
    defaultOptions
        {fieldLabelModifier = t2s . toLowerStart . s2t . drop 2 }

instance ToJSON MetaPage where
    toJSON = genericToJSON docyamlOptions

instance FromJSON MetaPage where
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
    dyLang         <- o .:? "lang" .!= DLenglish  -- x^ default 
    dyKeywords     <- o .: "keywords"
    dyDate         <- o .:? "date"
    dyFn           <- o .:? "fn" .!= ""  -- x^ as a default, is overwritten but avoids error msg
    dyLink         <- o .:? "link" .!= ""  -- x^ the relative link for html, derive from fn
    dyBibliography <- o .:? "bibliography"
        -- the bib file if needed  
    dyStyle        <- o .:? "style" -- x^ the csl file 

    dyPublish      <- o .:? "publish"  --  .!= Nothing 
    dyIndexPage  <- o .:? "indexPage" .!= False
    -- dyDirEntries   <- o .:? "dirEntries" .!= []
    -- dyFileEntries  <- o .:? "fileEntries" .!= []
    return MetaPage { .. }

checkDocrep1 :: Path Abs Dir -> Path Abs Dir -> Path Abs File -> Value -> ErrIO MetaPage
-- complete the meta yaml data  
-- test for completeness of metadata in yaml 
-- fails if required labels are not present
-- adds defaults when
-- sets filename 
checkDocrep1 doughP bakedP fn y1 = do
    putIOwords ["checkDocrep1 start"]
    let resdy = parseEither parseJSONyaml y1
            :: Either String MetaPage
    case resdy of
        Left msg ->
            errorT
                [ "checkDocrep1 not all required fields"
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
            when False $ putIOwords ["checkDocrep1 1 resdy2", showT resdy2]
            -- dy <- case resdy of 
            --         Error msg -> error msg 
            --         Success a -> return a 
            let dy = resdy2
            putIOwords ["checkDocrep1 dy", showT dy]
            return dy

addBakedRoot :: Path  Abs Dir -> Maybe Text -> Maybe Text
addBakedRoot bakedP Nothing = Nothing
addBakedRoot bakedP (Just fp) = Just. s2t . toFilePath $ addFileName bakedP . t2s $ fp

-- | another data type to rep languages
data DocLanguage = DLgerman | DLenglish
        deriving (Show, Read, Ord, Eq, Generic)
instance Zeros DocLanguage where zero = DLenglish

instance FromJSON DocLanguage
instance ToJSON DocLanguage
    -- is this clever to have a new language datatype? 


data PublicationState = PSpublish | PSdraft | PSold | PSzero
                  deriving (Generic,  Show, Read, Ord, Eq)
-- ^ is this file ready to publish

instance Zeros PublicationState where
  zero = PSzero
instance NiceStrings PublicationState where
  shownice = drop' 2 . showT

instance ToJSON PublicationState
instance FromJSON PublicationState