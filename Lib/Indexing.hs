
------------------------------------------------------------------------------
--
-- Module      :   create an index for a directory

--

-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
--{-# LANGUAGE DuplicateRecordFields #-}

module Lib.Indexing
     where

import Uniform.Strings hiding ((<.>), (</>))
import Uniform.FileIO hiding ((<.>), (</>))
--import Uniform.FileStrings
--import Uniform.TypedFile

import Data.Aeson
import Data.Yaml  as Y
--import Lib.FileMgt
import Development.Shake.FilePath


makeIndexForDir :: Path Abs Dir -> ErrIO Text
-- make the index for the directory
-- place result in index.html in this directory

makeIndexForDir focus = do
    fs <- getDirContentNonHiddenFiles (toFilePath focus)
    is :: [IndexEntry] <- mapM (\f -> makeIndexEntry (makeAbsFile f)) fs
    putIOwords ["makeIndexForDir", "for ", showT focus, "\n", showT is ]
    let is1 = Y.encode is :: _
    let is2 =  (("menu"::Text) .= is1) -- :: KeyValue Pair
    let is3 = object is2
    let yaml1 = bb2t .   Y.encode  $ is2
    putIOwords ["makeIndexForDir", "yaml ", yaml1  ]

    return ""

data IndexEntry = IndexEntry {text :: Text  -- ^ naked filename
                              , link :: Text -- ^ the url relative to dough dir
                              } deriving (Generic, Eq, Ord, Show)
instance Zeros IndexEntry where zero = IndexEntry zero zero
instance FromJSON IndexEntry
instance ToJSON IndexEntry


makeIndexEntry :: Path Abs File -> ErrIO IndexEntry
makeIndexEntry fp = do
    let paths = reverse $ splitPath (toFilePath fp)
    let fn = head paths
    let dir = head . tail $ paths
    let fnn = takeBaseName fn



    return $ IndexEntry {text = s2t fnn, link = s2t $ "/" <> dir </> fnn <.> "html"}


getDirContentNonHiddenFiles :: FileOps fp => fp -> ErrIO [fp]
getDirContentNonHiddenFiles fp = do
    fps  <- getDirContentNonHidden fp
    filterM doesFileExist' fps

