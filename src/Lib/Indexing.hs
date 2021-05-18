----------------------------------------------------------------------
--
-- Module      :   create an index for a directory
---- | create an index for a directory
--  in two steps: collect all the date
--  with call to completeIndex
--  and
--  indexmake: convert collected data for printing (convertIndexEntries)
--  .
--  the data is stored in a file separately and managed by Shake
--  operates on metapage (or less? )
----------------------------------------------------------------------
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}

module Lib.Indexing where

import Uniform.Json  
import Uniform.PandocImports

import Foundational.Filetypes4sites
import Foundational.MetaPage
import UniformBase
import Wave.Md2doc
-- import Lib.CmdLineArgs (PubFlags (..))
-- import Foundational.Foundation



completeIndex :: NoticeLevel -> Path Abs Dir -> Path Abs Dir -> IndexEntry -> ErrIO IndexEntry
{- ^ the top call to form the index data into the MetaPage
later only the format for output must be fixed
-}
completeIndex debug doughP bakedP ix1 = do
    when (inform debug) $ putIOwords ["completeIndex", "start", showPretty ix1]
    -- x1 :: IndexEntry <- fromJSONerrio yam1
    -- let x1 = panyam pr
    unless (indexPage ix1) $ errorT ["completeIndex should only be called for indexPage True"]

    let fn = doughP </> (link ix1) :: Path Abs File
    -- changed to search in dough (but no extension yet)
    when (inform debug) $
        putIOwords
            [ "completeIndex"
            , "is indexpage"
            , "fn for search"
            , showT fn
            ]

    (dirs, files) <- getDirContent2dirs_files debug doughP bakedP fn
    when (inform debug) $ putIOwords ["completeIndex", "\n dirs", showT dirs, "\n files", showT files]
    let ix2 = ix1{dirEntries = dirs, fileEntries = files}
    when (inform debug) $ putIOwords ["completeIndex", "x2", showT ix2]
    return ix2

{- | get the contents of a directory, separated into dirs and files
 the directory is given by the index file
 which files to check: index.md (in dough) or index.docrep (in baked)
 currently checks index.docrep in dough (which are not existing)
 indexfile itself is removed and files which are not markdown
-}
getDirContent2dirs_files :: NoticeLevel -> Path Abs Dir -> Path Abs Dir -> Path Abs File -> ErrIO ([IndexEntry], [IndexEntry])
getDirContent2dirs_files debug doughP bakedP indexpageFn = do
    putIOwords ["getDirContent2dirs_files", showT indexpageFn]
    let pageFn = makeAbsDir $ getParentDir indexpageFn :: Path Abs Dir
    -- get the dir in which the index file is embedded
    dirs1 :: [Path Abs Dir] <- getDirectoryDirs' pageFn
    let dirs2 = filter ((("DNB" :: FilePath) /=) .   getNakedDir) dirs1
    files1 :: [Path Abs File] <- getDirContentFiles pageFn
    let files2 =
            filter (indexpageFn /=) -- should not exclude all index pages but has only this one in this dir?
                . filter (hasExtension extMD) -- extDocrep)
                $ files1
    ixfiles <- mapM (getFile2index debug doughP bakedP) files2
    let subindexDirs = map (\d -> d </> makeRelFile "index.md") dirs2
    -- "index.docrep" 
    ixdirs <- mapM (getFile2index debug doughP bakedP) subindexDirs

    return (catMaybes ixdirs, catMaybes ixfiles)

getFile2index :: NoticeLevel -> Path Abs Dir -> Path Abs Dir -> Path Abs File -> ErrIO (Maybe IndexEntry)
-- get a file and its index
-- collect data for indexentry (but not recursively, only this file)
-- the directories are represented by their index files
-- produce separately to preserve the two groups
getFile2index debug doughP bakedP fnin =
    do
        -- (Docrep y1 _) <- read8 fnin docrepFileType
        mdfile <- read8 fnin markdownFileType 
        (Docrep y1 _) <- readMarkdown2docrep debug doughP bakedP fnin mdfile
        -- needs the indexentry initialized
        let ix1 :: IndexEntry = dyIndexEntry y1
        return . Just $ ix1
