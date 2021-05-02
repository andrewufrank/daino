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

import           GHC.Generics
import           Data.Default
-- import           Uniform.Strings      --   hiding ( (</>) )
-- import           Uniform.FileIO
-- import           Uniform.Time                   ( readDateMaybe
--                                                 , year2000
--                                                 , UTCTime(..)
--                                                 )
import Uniform.Shake (makeRelativeP)
-- import           Uniform.Pandoc
        -- (ToJSON, FromJSON, Pandoc, Value, getAtKey
        -- -- , readMd2meta
        -- )
-- import Uniform.Markdown (readMd2meta)
import           Lib.Foundation
        -- (SiteLayout(..), templatesDir, defaultPageTypeName)
import           Uniform.DocRep
-- import           Uniform.Time
-- import Uniform.FileIO 
import UniformBase
import           Data.List                      ( (\\) )
import           Data.Aeson
import           Data.Aeson.Types
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
    dyLang         <- o .:? "lang" .!= DLenglish  -- default 
    dyKeywords     <- o .: "keywords"
    dyDate         <- o .:? "date"
    dyFn           <- o .:? "fn" .!= ""
            -- as a default, is overwritten but avoids error msg
    dyLink         <- o .:? "link" .!= ""
        -- ^ the relative link for html, derive from fn
    dyBibliography <- o .:? "bibliography"
        -- the bib file if needed  
    dyStyle        <- o .:? "style" -- the csl file 

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
-- type TripleDoc = (Pandoc, MetaRec, Maybe Text)
-- -- ^ the pandoc content, the metarec (from yaml) and the report from conversion)

-- getTripleDoc :: SiteLayout ->   Path Abs File -> ErrIO TripleDoc
-- -- get one input file, return the values parsed
-- -- check validity   -- TODO check for md extension ! 
-- -- uses siteLayout to construct file names to abs file 
-- getTripleDoc  layout mdfn = do
--     -- putIOwords ["getTripleDoc start", showT mdfn]
--     (pandoc, meta2) :: (Pandoc, Value) <- readMd2meta mdfn -- (dough2 </> mdfn)
--     -- putIOwords ["getTripleDoc meta2", showT meta2]
--     modificationTime <- getFileModificationUTCTime mdfn
--     let (metaRec1, report1) = readMeta2rec layout mdfn meta2 modificationTime
--     return (pandoc, metaRec1, report1) 

-- readMeta2rec :: SiteLayout -> Path Abs File -> Value -> UTCTime -> (MetaRec, Maybe Text)
-- -- | read the metadata in a record and check for validity
-- -- and information what is missing
-- -- TODO should provide warning if there is a value, but not readable? 
-- readMeta2rec layout mdfn meta2 modificationTime = (ix, reportEssence)
--     where
--         ix = MetaRec    
--             { fn = toFilePath  (mdfn :: Path Abs File)
--             , relURL     = makeRelPath (doughDir layout) mdfn -- (Path Rel File))
--             ,  abstract         = fromMaybe "" (reduceMaybe2 (M.lookup Abstract labelVals))
--             , title            = fromMaybe "" (reduceMaybe2 (M.lookup Title labelVals))
--             , author           = fromMaybe "" (reduceMaybe2 (M.lookup Author labelVals))
--             , date             = if timeOK 
--                                     then fromJustNote "readDate 408ds" . readDateMaybe 
--                                             . fromJustNote "readData 1112fad" $ 
--                                                     (reduceMaybe2 (M.lookup Date labelVals)) 
--                                     else  modificationTime
--             , publicationState = text2publish  (reduceMaybe2 (M.lookup Publish labelVals))
--             , bibliography  =   fmap (\f -> toFilePath $ doughDir layout </> makeRelFileT f) 
--                         (reduceMaybe2 (M.lookup Bibliography labelVals))
--             , bibliographyGroup = (reduceMaybe2 (M.lookup BibliographyGroup labelVals))
--             , keywords = (reduceMaybe2 (M.lookup Keywords labelVals))  
--             , pageTemplate = (\f -> toFilePath $ templatesDir layout </>   f)
--                             $ fromMaybe ( defaultPageTypeName)  
--                                (fmap (makeRelFile . t2s) $ reduceMaybe2 (M.lookup PageTemplate labelVals))
--             , indexPage = fromMaybe False $ getAtKey meta2 "indexPage" ::  Bool 
--             , indexSort = text2sortargs (reduceMaybe2 (M.lookup IndexSort labelVals))
--                     -- indexSort1
--                 -- default is publish
--             }
--         vals2  = map (getAtKey meta2) (map (toLower' . showT) allLabels) :: [Maybe Text] 
--         -- [abstract1, title1, author1, date1, publish1
--         --     , bibliography1, bibliographyGroup1, keywords1, pageTemplate1
--         --     , indexSort1] = vals2
--         labelVals = M.fromList (zip allLabels vals2) :: M.Map Label (Maybe Text)
--         -- better approach - this enforces the same order!

--         -- indexPage1 = getAtKey meta2 "indexPage" :: Maybe Bool 
--         -- date0 = getAtKey meta2 "date" :: Maybe Text 
--         -- -- abstract1 = getAtKey meta2 "abstract" :: Maybe Text
--         -- -- title1    = getAtKey meta2 "title" :: Maybe Text
--         -- -- author1   = getAtKey meta2 "author" :: Maybe Text
--         -- -- date1     = getAtKey meta2 "date" :: Maybe Text
--         -- -- publish1  = getAtKey meta2 "publish" :: Maybe Text

--         (timeOK, timeIssue) = _dateIssue (reduceMaybe2 (M.lookup Date labelVals)) :: (Bool, Maybe Text)

--         reportEssence = convertToReport vals2 (missingLabels  vals2) timeIssue :: Maybe Text 

-- reduceMaybe2 :: Maybe (Maybe a) -> Maybe a
-- reduceMaybe2 (Just a) =   a
-- reduceMaybe2 (Nothing) = Nothing 

-- convertToReport :: [Maybe Text] ->  [Label] ->  Maybe Text -> Maybe Text 
-- convertToReport vals missing timeIssue  = if null mt then Nothing else Just . concatT $ mt  
--     where
--         mt = catMaybes [m,t] :: [Text]
--         m = if null missing then Nothing 
--                     else Just . concatT $ ["\n\tmissing required label values: "
--                             , concatT [showT missing] ]
--                             ::  Maybe Text 
--         t = fmap ( ("\n\ttime issues: " <>)) timeIssue  ::  Maybe Text                 

-- data Label =  Abstract | Title | Author | Date | Publish | Bibliography | BibliographyGroup 
--                     | Keywords | PageTemplate | IndexSort 
--                     deriving (Show, Enum, Read, Eq, Ord)

-- allLabels :: [Label]
-- allLabels = [Abstract .. IndexSort]

-- missingLabels :: [Maybe Text] ->  [Label]   
-- missingLabels  vals =  
--             (\\ notRequiredLabels) . map fst .  filter (isNothing . snd) $ zip allLabels vals
--         -- what can be defaulted: author (remains blank), publish (defaults to publish)
--                     -- bibliography (if not used) bibliographyGroup (if not used)
--                     -- , "pageTemplate", "indexSort"
--     where     -- what must be provided: 
--         requiredLabels =  [Abstract, Title, Keywords]:: [Label]
--         notRequiredLabels = allLabels \\ requiredLabels :: [Label]

-- _dateIssue :: Maybe Text ->   (Bool, Maybe Text)
-- --  check if date is ok, if not set False and give text 
-- _dateIssue date1x  = case date1x of
--     Nothing -> (False, Nothing)  -- "no date supplied - use file modificatin date")
--     Just d  -> case readDateMaybe d of
--       Nothing -> (False, Just $ unwords' ["date", showT date1x
--                         , "not readable - use file modification date"])
--       Just _  -> (True, Nothing)

-- -- | the data in the meta/yaml part of the md files 
-- -- todo remove and replace with json
-- data MetaRec = MetaRec  
--         { fn ::  FilePath  -- ^  filename abs file
--         , relURL :: FilePath -- ^ the filepath relative to dough dir  
--         ,  title ::  Text -- ^ the title as shown
--         , abstract ::  Text
--         , author ::  Text
--         , date ::  UTCTime -- read the time early one to find errors
--         , publicationState ::  PublicationState
--         , bibliography :: Maybe  FilePath --  (Path Abs File)
--               -- Path  reading in records is not working 
--         , bibliographyGroup :: Maybe Text 
--         , keywords :: Maybe Text 
--         , pageTemplate::  FilePath -- the template, defaulted here 
--         , indexPage ::  Bool  -- is this an index page?
--         , indexSort :: SortArgs 

--         } deriving (Generic, Eq, Ord, Show, Read)

-- instance Zeros MetaRec where
--   zero = MetaRec zero zero zero zero zero (year2000) zero zero zero zero zero zero zero
-- instance ToJSON MetaRec
-- instance FromJSON MetaRec where

-- text2publish :: Maybe Text ->  PublicationState
-- -- convert a text to a publicationstate
-- text2publish (Nothing) = PSpublish
-- --  the default is to publish 
-- text2publish (Just tt) = case toLower' tt of
--   "true"    ->  PSpublish
--   "publish" ->  PSpublish
--   "draft"   ->  PSdraft
--   "old"     ->  PSold
--   _         -> PSzero  

-- makeRelPath :: Path Abs Dir -> Path Abs File -> FilePath
-- makeRelPath dough2  mdfn = ("/" <>)   .toFilePath . fromJustNote "readMeta2rec relURL wer234c" 
--   $ (stripPrefix dough2 mdfn :: Maybe (Path Rel File))
--   -- the references must be absolute (relative to the root, which is doug/)

data PublicationState = PSpublish | PSdraft | PSold | PSzero
                  deriving (Generic,  Show, Read, Ord, Eq)
-- ^ is this file ready to publish

instance Zeros PublicationState where
  zero = PSzero
instance NiceStrings PublicationState where
  shownice = drop' 2 . showT

instance ToJSON PublicationState
instance FromJSON PublicationState

-- data SortArgs = SAtitle | SAdate | SAreverseDate | SAzero 
--       deriving (Generic,  Show, Read, Ord, Eq)
-- -- ^ the argument for the sorting of the index 
-- instance Zeros SortArgs where 
--     zero = SAzero 
-- instance NiceStrings SortArgs where 
--     shownice = drop' 2 . showT 
-- instance ToJSON SortArgs 
-- instance FromJSON SortArgs

-- text2sortargs :: (CharChains a, IsString a) => Maybe a -> SortArgs
-- text2sortargs (Nothing) = SAzero
-- text2sortargs (Just tt) = case toLower' tt of 
--     "title" -> SAtitle 
--     "titel" -> SAtitle
--     "date" -> SAdate
--     "reversedate" -> SAreverseDate
--     "reverseDate" -> SAreverseDate
--     _ -> SAzero 

