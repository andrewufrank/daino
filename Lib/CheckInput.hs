------------------------------------------------------------------------------
--
-- Module      :  check all inputs and produce a record summary
--

-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric     #-}

module Lib.CheckInput where

import           Uniform.Strings      --   hiding ( (</>) )
import           Uniform.FileIO
import           Uniform.Time                   (   readDateMaybe
                                                , year2000
                                                , UTCTime(..)
                                                )

import           Uniform.Pandoc
import Lib.Foundation (SiteLayout(..), templatesDir)

-- checkOneMdFile :: SiteLayout ->   Path Abs File -> ErrIO (Pandoc, MetaRec, Text)
-- -- check one input file, return the values parsed
-- -- uses doughP to construct file names to abs file 
-- -- TODO duplicate in functionality?
-- checkOneMdFile  layout mdfn = do
--   -- putIOwords ["checkOneMdFile start", showT mdfn]
--   -- TODO check for md extension ! 
--   -- used - duplicating getTripleDoc?
--   (pandoc, meta2) :: (Pandoc, Value) <- readMd2meta mdfn -- (dough2 </> mdfn)
--   -- putIOwords ["checkOneMdFile meta2", showT meta2]

--   modificationTime <- getFileModificationUTCTime mdfn
--     -- modification time is inserted in the date field if not set

--   let (metaRec1,report1) = readMeta2rec layout mdfn meta2 modificationTime

--   let report2 = unwords' ["\n ------------------",  "\n", report1]
--   return (pandoc, metaRec1, report2) 

type TripleDoc = (Pandoc, MetaRec, Text)

getTripleDoc :: SiteLayout ->   Path Abs File -> ErrIO TripleDoc
-- get one input file, return the values parsed
-- check validity   -- TODO check for md extension ! 
-- uses siteLayout to construct file names to abs file 
getTripleDoc  layout mdfn = do
    -- putIOwords ["getTripleDoc start", showT mdfn]
    (pandoc, meta2) :: (Pandoc, Value) <- readMd2meta mdfn -- (dough2 </> mdfn)
    -- putIOwords ["getTripleDoc meta2", showT meta2]
    modificationTime <- getFileModificationUTCTime mdfn
    let (metaRec1,report1) = readMeta2rec layout mdfn meta2 modificationTime

    let report2 = unwords' ["\n ------------------",  "\n", report1]
    return (pandoc, metaRec1, report2) 

readMeta2rec :: SiteLayout -> Path Abs File -> Value -> UTCTime -> (MetaRec, Text)
-- | read the metadata in a record and check for validity
-- and information what is missing
readMeta2rec layout mdfn meta2 modificationTime = (ix, report)
  where
    ix = MetaRec    
        { fn = -- s2t .  getNakedFileName  . 
                        toFilePath  (mdfn :: Path Abs File)
        , relURL     = makeRelPath (doughDir layout) mdfn -- (Path Rel File))
        ,  abstract         = fromMaybe "" abstract1
        , title            = fromMaybe "" title1
        , author           = fromMaybe "" author1
        , date             = if timeOK 
                                then fromJustNote "readDate 408ds" . readDateMaybe 
                                        . fromJustNote "readData 1112fad" $ date1 
                                else  modificationTime
                    -- maybe year2000 
                        -- (fromJustNote "readDate 408ds" . readDateMaybe) date1   
                        -- test early for proper format
        , publicationState = text2publish  publish1
        , bibliography  =   fmap (\f -> toFilePath $ doughDir layout </> makeRelFileT f) bibliography1
        , bibliographyGroup = bibliographyGroup1
        , keywords = keywords1
        , pageTemplate = fmap (\f -> toFilePath $ templatesDir layout </> makeRelFileT f) pageTemplate1
        , indexPage = fromMaybe False indexPage1
        , indexSort = text2sortargs indexSort1
            -- default is publish
        }
    [abstract1, title1, author1, date1, publish1
        , bibliography1, bibliographyGroup1, keywords1, pageTemplate1
        , indexSort1] = vals2
    vals2  = map (getAtKey meta2) keys2
    keys2  = ["abstract", "title", "author", "date", "publish", "bibliography", "bibliographyGroup"
                , "keywords", "pageTemplate", "indexSort"]
    indexPage1 = getAtKey meta2 "indexPage" :: Maybe Bool 
    date0 = getAtKey meta2 "date" :: Maybe Text 
    -- -- abstract1 = getAtKey meta2 "abstract" :: Maybe Text
    -- -- title1    = getAtKey meta2 "title" :: Maybe Text
    -- -- author1   = getAtKey meta2 "author" :: Maybe Text
    -- -- date1     = getAtKey meta2 "date" :: Maybe Text
    -- -- publish1  = getAtKey meta2 "publish" :: Maybe Text

    (timeOK, timeIssue) = _dateIssue date1
    -- date2 :: UTCTime 
    -- date2 = if timeOK then zero -- fromJustNote "wer2342" date1 else ("some"::Text)
    --             else modificationTime -- putAtKey meta2 "date" (showT modificationTime) 

    report = unwords' ["missing values: ", missingLabels, timeIssue]

    missingLabels =  unwords' . map fst . filter (isNothing . snd) $ zip keys2 vals2

_dateIssue :: Maybe Text ->   (Bool, Text)
--  check if date is ok, if not set False and give text 
_dateIssue date1x  = case date1x of
    Nothing -> (False, "no date supplied - use file modificatin date")
    Just d  -> case readDateMaybe d of
      Nothing -> (False, unwords' ["date", showT date1x
                        , "not readable - use file modification date"])
      Just _  -> (True, "")

-- | the data in the meta/yaml part of the md files 
data MetaRec = MetaRec  
        { fn ::  FilePath  -- ^  filename abs file
        , relURL :: FilePath -- ^ the filepath relative to dough dir  
        ,  title ::  Text -- ^ the title as shown
        , abstract ::  Text
        , author ::  Text
        , date ::  UTCTime -- read the time early one to find errors
        , publicationState ::  PublicationState
        , bibliography :: Maybe  FilePath --  (Path Abs File)
              -- Path  reading in records is not working 
        , bibliographyGroup :: Maybe Text 
        , keywords :: Maybe Text 
        , pageTemplate:: Maybe FilePath -- (Path Abs File)
        , indexPage ::  Bool
        , indexSort :: SortArgs 

        } deriving (Generic, Eq, Ord, Show, Read)

instance Zeros MetaRec where
  zero = MetaRec zero zero zero zero zero (year2000) zero zero zero zero zero zero zero
--instance FromJSON IndexEntry
instance ToJSON MetaRec
instance FromJSON MetaRec where

text2publish :: Maybe Text ->  PublicationState
-- convert a text to a publicationstate
text2publish (Nothing) = PSpublish
--  the default is to publish 
text2publish (Just tt) = case toLower' tt of
  "true"    ->  PSpublish
  "publish" ->  PSpublish
  "draft"   ->  PSdraft
  "old"     ->  PSold
  _         -> PSzero  

makeRelPath :: Path Abs Dir -> Path Abs File -> FilePath
makeRelPath dough2  mdfn = ("/" <>)   .toFilePath . fromJustNote "readMeta2rec relURL wer234c" 
  $ (stripPrefix dough2 mdfn :: Maybe (Path Rel File))
  -- the references must be absolute (relative to the root, which is doug/)

data PublicationState = PSpublish | PSdraft | PSold | PSzero
                  deriving (Generic,  Show, Read, Ord, Eq)
-- ^ is this file ready to publish

instance Zeros PublicationState where
  zero = PSzero
instance NiceStrings PublicationState where
  shownice = drop' 2 . showT

instance ToJSON PublicationState
instance FromJSON PublicationState

data SortArgs = SAtitle | SAdate | SAreverseDate | SAzero 
      deriving (Generic,  Show, Read, Ord, Eq)
-- ^ the argument for the sorting of the index 
instance Zeros SortArgs where 
    zero = SAzero 
instance NiceStrings SortArgs where 
    shownice = drop' 2 . showT 
instance ToJSON SortArgs 
instance FromJSON SortArgs

text2sortargs :: (CharChains a, IsString a) => Maybe a -> SortArgs
text2sortargs (Nothing) = SAzero
text2sortargs (Just tt) = case toLower' tt of 
    "title" -> SAtitle 
    "titel" -> SAtitle
    "date" -> SAdate
    "reversedate" -> SAreverseDate
    "reverseDate" -> SAreverseDate
    _ -> SAzero 

