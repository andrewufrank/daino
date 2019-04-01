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

import           Uniform.Strings         hiding ( (</>) )
import           Uniform.Filenames
-- import Uniform.FileStrings
-- import           Lib.Foundation
import           Uniform.Time                   (   readDateMaybe
                                                , year2000
                                                , UTCTime(..)
                                                )
-- import           Lib.YamlBlocks                 ( Value
--                                                 , readMd2meta
--                                                 , getMaybeStringAtKey
--                                                 )
-- import           Lib.Indexing

import           Uniform.Pandoc
-- import Lib.CmdLineArgs (PubFlags(..))


checkAllInputs :: [Path Abs File] -> ErrIO Text
-- ^ check the input files for syntax errors 
-- needs a shake to call check
-- with a switch -c 

checkAllInputs  mdfiles = do
  putIOwords ["checkAllInput start" ]
  val <- mapM checkOneMdFile  mdfiles

  let res = showT val
  putIOwords ["checkAllInput end", showT res]
  return . showT $ val

type TripleDoc = (Pandoc, MetaRec, Text)

checkOneMdFile ::   Path Abs File -> ErrIO (Pandoc, MetaRec, Text)
-- check one input file, return ?
checkOneMdFile  mdfn = do
  putIOwords ["checkOneMdFile start", showT mdfn]
  (pandoc, meta2) :: (Pandoc, Value) <- readMd2meta mdfn -- (dough2 </> mdfn)
  putIOwords ["checkOneMdFile meta2", showT meta2]

  let (metaRec1,report1) = readMeta2rec meta2
  -- ixEntry                       <- getOneIndexEntry allFlags dough2 (dough2 </> mdfn)
  -- what needs to be checked ?  -- check with all flags true 

  -- let doindex1 =  maybe False ("True"==) $ getMaybeStringAtKey meta2 "indexPage"  :: Bool
  -- let doindex2 = maybe False id $ getAtKey meta2 "indexPage" :: Bool

  let report2 = unwords' ["\n ------------------",  "\n", report1]
  putIOwords
    [ "checkOneMdFile end metaRec1"
    -- , showT meta2
    , showT   metaRec1]
  putIOwords ["report2 \n"
    , showT  report2, "\n"
    ]
  return (pandoc, metaRec1, report2) 

readMeta2rec :: Value -> (MetaRec, Text)
-- | read the metadata in a record and check for validity
-- and information what is missing
readMeta2rec meta2 = (ix, report)
 where
  ix = MetaRec {
        -- text     = s2t fnn
    -- , link     = ln
                 abstract         = abstract1
               , title            = title1
               , author           = author1
               , date             = maybe Nothing readDateMaybe date1   -- test early for proper format
               , publicationState = text2publish $ publish1
               , bibliography  = bibliography1
               , bibliographyGroup = bibliographyGroup1
               , keywords = keywords1
               , pageTemplate = pageTemplate1
                    -- default is publish
               }
  [abstract1, title1, author1, date1, publish1
      , bibliography1, bibliographyGroup1, keywords1, pageTemplate1] = vals2
  vals2  = map (getAtKey meta2) keys2
  keys2  = ["abstract", "title", "author", "date", "publish", "bibliography", "bibliographyGroup"
            , "keywords", "pageTemplate"]

  -- abstract1 = getAtKey meta2 "abstract" :: Maybe Text
  -- title1    = getAtKey meta2 "title" :: Maybe Text
  -- author1   = getAtKey meta2 "author" :: Maybe Text
  -- date1     = getAtKey meta2 "date" :: Maybe Text
  -- publish1  = getAtKey meta2 "publish" :: Maybe Text

  report = unwords' ["missing values: ", missingLabels, dateIssue date1]
  dateIssue :: Maybe Text -> Text
  dateIssue date1x = case date1x of
    Nothing -> "no date supplied"
    Just d  -> case readDateMaybe d of
      Nothing -> unwords' ["date", showT date1x, "not readable"]
      Just _  -> ""

  missingLabels =  unwords' . map fst . filter (isNothing . snd) $ zip keys2 vals2

-- | the data in the meta/yaml part of the md files 
data MetaRec = MetaRec {
        -- text ::  Text  -- ^ naked filename -- not shown
                              -- , link :: Text -- ^ the url relative to dough dir
                               title :: Maybe Text -- ^ the title as shown
                              , abstract :: Maybe Text
                              , author :: Maybe Text
                              , date :: Maybe UTCTime -- read the time early one to find errors
                              , publicationState :: Maybe PublicationState
                              , bibliography :: Maybe Text 
                              , bibliographyGroup :: Maybe Text 
                              , keywords :: Maybe Text 
                              , pageTemplate:: Maybe Text 

                              } deriving (Generic, Eq, Ord, Show, Read)

instance Zeros MetaRec where
  zero = MetaRec zero zero zero (Just year2000) zero zero zero zero zero
--instance FromJSON IndexEntry
instance ToJSON MetaRec
instance FromJSON MetaRec where

text2publish :: Maybe Text -> Maybe PublicationState
-- convert a text to a publicationstate
text2publish (Nothing) = Nothing
--  the default is to publish 
text2publish (Just tt) = case (toLower' tt) of
  "true"    -> Just PSpublish
  "publish" -> Just PSpublish
  "draft"   -> Just PSdraft
  "old"     -> Just PSold
  _         -> Nothing


data PublicationState = PSpublish | PSdraft | PSold | PSzero
                  deriving (Generic,  Show, Read, Ord, Eq)
-- ^ is this file ready to publish

instance Zeros PublicationState where
  zero = PSzero
instance NiceStrings PublicationState where
  shownice = drop' 2 . showT

instance ToJSON PublicationState
instance FromJSON PublicationState


