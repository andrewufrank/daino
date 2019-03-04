
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
import qualified Uniform.FileIO as FileIO
--import Uniform.FileStrings
--import Uniform.TypedFile

import Data.Aeson
import Data.Yaml  as Y
--import Lib.FileMgt
import Development.Shake.FilePath

--insertIndex :: Path Abs File -> ErrIO ()
---- insert the index into the index md


makeIndexForDir :: Path Abs Dir -> Path Abs File -> ErrIO MenuEntry
-- make the index for the directory
-- place result in index.html in this directory
-- the name of the index file is passed to exclude it
-- makes index only for md files in dough
-- needs more work for index? date ? abstract?
makeIndexForDir focus indexFn = do
    fs <- getDirContentNonHidden (toFilePath focus)
    let fs2 = filter (/= (toFilePath indexFn)) fs -- exclude index
    let fs3 = filter (FileIO.hasExtension ( "md")) fs2
    is :: [IndexEntry] <- mapM (\f -> makeIndexEntry (makeAbsFile f)) fs3
    let menu1 = MenuEntry {menu2 = is}
    putIOwords ["makeIndexForDir", "for ", showT focus, "\n", showT menu1 ]
    let yaml1 = bb2t .   Y.encode  $ menu1
    putIOwords ["makeIndexForDir", "yaml ", yaml1  ]

    return menu1

data MenuEntry = MenuEntry {menu2 :: [IndexEntry]} deriving (Generic, Eq, Ord, Show)
instance Zeros MenuEntry where zero = MenuEntry zero
instance FromJSON MenuEntry
instance ToJSON MenuEntry

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


--getDirContentNonHiddenFiles :: FileOps fp => fp -> ErrIO [fp]
--getDirContentNonHiddenFiles fp = do
--    fps  <- getDirContentNonHidden fp
--    filterM doesFileExist' fps

