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
{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports 
            -fno-warn-unused-imports 
            -fno-warn-unused-matches #-}
-- | The data describing a page of the site (i.e. an md file)
-- the default is merged with the values in the yaml head 
-- all entries there should be from this list
module Lib.MetaPage where


import Lib.CmdLineArgs (PubFlags (..))
import Lib.Foundation (SiteLayout (..))
import Uniform.Pandoc
import Uniform.Filetypes4sites

import UniformBase ()


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


            } deriving (Show,  Ord, Eq, Generic, Zeros)  --Read,

-- instance Zeros DocYaml where
--     zero = DocYaml zero
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
--                    []
--                    []

instance Default DocYaml where
    def = zero {dyFn = zero 
                , dyLink = zero
                , dyLang = DLenglish 
                , dyTitle = zero
                , dyAbstract = zero
                , dyAuthor = Andrew U Frank
                , dyDate = Just year2000  
                , dyKeywords = zero
                , dyBibliography = "BibTexLatex.bib"
                , dyStyle = "chicago-fullnote-bibliography-bb.csl"
                , dyPublish = ""
                , dyIndexPage = False 
                , dyDirEntries = zero
                , dyFileEntries = zero
                }

docyamlOptions :: Options
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
    dyDirEntries   <- o .:? "dirEntries" .!= []
    dyFileEntries  <- o .:? "fileEntries" .!= []
    return DocYaml { .. }

checkDocrep1 :: Path Abs Dir -> Path Abs Dir -> Path Abs File -> Value -> ErrIO DocYaml
-- complete the meta yaml data  
-- test for completeness of metadata in yaml 
-- fails if required labels are not present
-- adds defaults when
-- sets filename 
checkDocrep1 doughP bakedP fn y1 = do
    putIOwords ["checkDocrep1 start"]
    let resdy = parseEither parseJSONyaml y1
            :: Either String DocYaml
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