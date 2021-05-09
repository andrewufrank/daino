------------------------------------------------------------------------------
--
-- Module      :  check all inputs and produce a record summary
-- puts the content of doc yaml header in to MetaPage

-- could the text, the pandoc etc. all go there? 
-- at the moment it seems easier to keep the pandoc 
-- format separately

-- fills the MetaPage with defaults and the filename 
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
-- import Lib.Foundation ()
import Uniform.Filetypes4sites ( DocrepJSON(DocrepJSON) ) 

import UniformBase
import Uniform.Json 
import Lib.MetaPage 
    
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



completeDocRep :: Path Abs Dir -> Path Abs Dir -> Path Abs File -> DocrepJSON -> ErrIO DocrepJSON
-- complete the DocrepJSON (permitting defaults for all values) 
-- the bakedP root is necessary to complete the style and bib entries
-- as well as image? 
-- first for completeness of metadata in yaml 
-- fails if required labels are not present
completeDocRep doughP bakedP fn (DocrepJSON y1 p1) = do
    let m0 = def ::MetaPage 
        mFiles = addFileMetaPage doughP bakedP fn  
        y2 = mergeLeftPref [toJSON mFiles, y1, toJSON m0]
        -- preference of files as computed 
        -- over what is set in md file
        -- over default 
    -- y2 <- completeMetaPage doughP bakedP fn y1
    -- let y3 = mergeLeftPref [toJSON y2, y1]
    putIOwords ["completeDocRep", "y2", showT y2]
    return (DocrepJSON y2 p1)

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
                    } deriving (Show,  Eq, Ord, Generic)  -- Read,

instance ToJSON IndexEntry
instance FromJSON IndexEntry






