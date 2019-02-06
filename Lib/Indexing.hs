
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

import Uniform.Strings
import Uniform.FileIO
import Uniform.FileStrings
import Uniform.TypedFile

import Data.Aeson

import Lib.FileMgt

makeIndexForDir :: Path Abs Dir -> ErrIO ()
-- make the index for the directory
-- place result in index.html in this directory

makeIndexForDir focus = do
    fs <- getDirContentNonHiddenFiles (toFilePath focus)
    is :: [IndexEntry] <- mapM (\f -> makeIndexEntry (makeAbsFile f)) fs

    return ()

data IndexEntry = IndexEntry {filename :: Text  -- ^ naked filename
                              , absURL :: Text -- ^ the url relative to dough dir
                              } deriving (Generic, Eq, Ord, Show)
instance Zeros IndexEntry where zero = IndexEntry zero zero
instance FromJSON IndexEntry
instance ToJSON IndexEntry


makeIndexEntry :: Path Abs File -> ErrIO IndexEntry
makeIndexEntry fp = do
    let fn = zero
    let absu = zero



    return $ IndexEntry {filename = fn, absURL = absu}


getDirContentNonHiddenFiles :: FileOps fp => fp -> ErrIO [fp]
getDirContentNonHiddenFiles fp = do
    fps  <- getDirContentNonHidden fp
    filterM doesFileExist' fps

