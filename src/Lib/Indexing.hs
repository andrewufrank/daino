----------------------------------------------------------------------
--
-- Module      :   create an index for a directory
---- | create an index for a directory
--  in two steps: collect all the date 
--  with call to addIndex2yam
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
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}

module Lib.Indexing where

import Uniform.Json ( ErrIO, fromJSONerrio ) 
import UniformBase
import Foundational.MetaPage 
import Foundational.Filetypes4sites
import Lib.CmdLineArgs (PubFlags (..))
import Foundational.Foundation 

initializeIndex ::    MetaPage -> IndexEntry 
-- initialize the index with the values from the metapage yaml 
initializeIndex    MetaPage{..} = ix1 
    where 
        ix1 = zero
                { fn =  makeAbsFile dyFn   
                , title  = dyTitle
                , link = makeRelFile dyLink
                , abstract  = dyAbstract
                , author    = dyAuthor
                , date       = fromMaybe (showT year2000)  dyDate
                , publish     = dyPublish
                , indexPage  = dyIndexPage 
                , dirEntries  = zero 
                , fileEntries = zero 
                    }

addIndex2yam ::  NoticeLevel -> Path Abs Dir -> IndexEntry  -> ErrIO IndexEntry
{- ^ the top call to form the index data into the MetaPage
later only the format for output must be fixed
-}
addIndex2yam debug  bakedP ix1 = do
    when (inform debug) $ putIOwords ["addIndex2yam", "start", showPretty ix1]
    -- x1 :: IndexEntry <- fromJSONerrio yam1
    -- let x1 = panyam pr
    if not (indexPage ix1)
        then return ix1
        else -- return dr 
        do
            when (inform debug) $ putIOwords ["addIndex2yam", "is indexpage"]
            let fn = bakedP </> (link ix1) :: Path Abs File
            (dirs, files) <- getDirContent2dirs_files debug fn
            when (inform debug) $ putIOwords ["addIndex2yam", "\n dirs", showT dirs, "\n files", showT files]
            let ix2 = ix1{dirEntries = dirs, fileEntries = files}
            when (inform debug) $ putIOwords ["addIndex2yam", "x2", showT ix2]
            return ix2

{- | get the contents of a directory, separated into dirs and files
 the directory is given by the index file
 which files to check: index.md (in dough) or index.docrep (in baked)
 currently checks index.docrep in dough (which are not existing)
 indexfile itself is removed and files which are not markdown
-}
getDirContent2dirs_files :: NoticeLevel -> Path Abs File -> ErrIO ([IndexEntry], [IndexEntry])
getDirContent2dirs_files debug indexpageFn = do
    putIOwords ["getDirContent2dirs_files", showT indexpageFn]
    let pageFn = makeAbsDir $ getParentDir indexpageFn :: Path Abs Dir
    -- get the dir in which the index file is embedded
    dirs1 :: [Path Abs Dir] <- getDirectoryDirs' pageFn
    files1 :: [Path Abs File] <- getDirContentFiles pageFn
    let files2 =
            filter (indexpageFn /=) -- should not exclude all index pages but has only this one in this dir?
                . filter (hasExtension extDocrep)
                $ files1
    ixfiles <- mapM (getFile2index debug) files2
    let subindexDirs = map (\d -> d </> makeRelFile "index.docrep") dirs1
    ixdirs <- mapM (getFile2index debug) subindexDirs

    return (catMaybes ixdirs, catMaybes ixfiles)

getFile2index :: NoticeLevel -> Path Abs File -> ErrIO (Maybe IndexEntry)
-- get a file and its index
-- collect data for indexentry (but not recursively, only this file)
-- the directories are represented by their index files
-- produce separately to preserve the two groups
getFile2index debug fnin =
    do
        (Docrep y1 _) <- read8 fnin docrepFileType
        let ix1 :: IndexEntry = dyIndexEntry y1
        return . Just $ ix1
     
