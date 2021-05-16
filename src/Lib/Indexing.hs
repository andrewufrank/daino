----------------------------------------------------------------------
--
-- Module      :   create an index for a directory
--
----------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports 
            -fno-warn-unused-imports #-}

module Lib.Indexing where

import Uniform.Json ( ErrIO, fromJSONerrio ) 
import UniformBase


import Foundational.MetaPage (
    IndexEntry,
    MetaPage (dyDirEntries, dyFileEntries, dyIndexPage, dyLink),
 )
import Foundational.Filetypes4sites
    ( docrepFileType, extDocrep, Docrep(Docrep), Panrep(panyam) ) 

import Lib.CmdLineArgs (PubFlags (..))
import Foundational.Foundation 

addIndex2yam :: Path Abs Dir -> NoticeLevel -> Panrep -> ErrIO Panrep
{- ^ the top call to form the index data into the MetaPage
later only the format for output must be fixed
-}
addIndex2yam bakedP debug pr = do
    when (inform debug) $ putIOwords ["addIndex2yam", "start", showT pr]
    -- x1 :: IndexEntry <- fromJSONerrio yam1
    let x1 = panyam pr
    when (inform debug) $ putIOwords ["addIndex2yam", "x1", showT x1]
    if not . dyIndexPage $ x1
        then return pr
        else -- return dr -- TODO include again
        do
            when (inform debug) $ putIOwords ["addIndex2yam", "is indexpage"]
            (dirs, files) <- getDirContent2dirs_files debug (bakedP </> dyLink x1)
            when (inform debug) $ putIOwords ["addIndex2yam", "\n dirs", showT dirs, "\n files", showT files]
            let x2 = x1{dyDirEntries = dirs, dyFileEntries = files}
            when (inform debug) $ putIOwords ["addIndex2yam", "x2", showT x2]
            return pr{panyam = x2}

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
    (do
        (Docrep y1 _) <- read8 fnin docrepFileType
        ix1 :: IndexEntry <- fromJSONerrio y1
        return . Just $ ix1)
    `catchError` 
        ( \e -> do
                    when (inform debug) $ putIOwords
                        [ "getFile2index error caught\n"
                        , "fn:"
                        , showT fnin
                        , "\n"
                        , showT e -- " showT msg])
                        ]
                    return Nothing
                    )
