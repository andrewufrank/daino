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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Lib.Indexing where

import Uniform.Json  
import Uniform.PandocImports
-- import Foundational.LayoutFlags
import Foundational.Filetypes4sites
import Foundational.MetaPage
import UniformBase
import Wave.Md2doc



completeIndex :: NoticeLevel -> Path Abs Dir -> Path Abs Dir -> IndexEntry -> ErrIO IndexEntry
{- ^ the top call to form the index data into the MetaPage
later only the format for output must be fixed
-}
completeIndex debug doughP bakedP ix1 = do
    when (inform debug) $ putIOwords ["completeIndex", "start", showPretty ix1]
    -- x1 :: IndexEntry <- fromJSONerrio yam1
    -- let x1 = panyam pr

    let fn = doughP </> (link ix1) :: Path Abs File
    -- changed to search in dough (but no extension yet)
    when (inform debug) $
        putIOwords
            [ "completeIndex"
            , "fn"
            , showT fn
            ]
    -- unless (isIndexPage fn) $ errorT ["completeIndex should only be called for indexPage True"]

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
    -- putIOwords ["getDirContent2dirs_files for", showPretty indexpageFn]
    let pageFn = makeAbsDir $ getParentDir indexpageFn :: Path Abs Dir
    -- get the dir in which the index file is embedded
    -- putIOwords ["getDirContent2dirs_files pageFn", showPretty pageFn]

    dirs1 :: [Path Abs Dir] <- getDirectoryDirs' pageFn
    let dirs2 = filter ( not . (isPrefixOf' ("DNB" :: FilePath) ) .   getNakedDir) dirs1
    let dirs3 = filter ( not . (isPrefixOf' "resources"
        -- (toFilePath resourcesDirName :: FilePath)
         ) .   getNakedDir) dirs2
    let dirs4 = filter ( not . (isPrefixOf' "templates"
        -- (toFilePath templatesDirName :: FilePath)
         ) .   getNakedDir) dirs3
    -- TODO may need extension (change to list of excluded)
    -- build from constants in foundation

    -- putIOwords ["\ngetDirContent2dirs_files excluded", s2t$ toFilePath templatesDirName , s2t$ toFilePath resourcesDirName]
    -- putIOwords ["\ngetDirContent2dirs_files dirs4", showPretty dirs4]

    files1 :: [Path Abs File] <- getDirContentFiles pageFn

    -- putIOwords ["getDirContent2dirs_files files1", showPretty files1]

    let files2 =
            filter (indexpageFn /=) -- should not exclude all index pages but has only this one in this dir?
                . filter (hasExtension extMD) -- extDocrep)
                $ files1
    -- putIOwords ["getDirContent2dirs files2", showPretty files2]
    ixfiles <- mapM (getFile2index debug doughP bakedP) files2
    -- putIOwords ["getDirContent2dirs ixfiles", showPretty ixfiles]

    let subindexDirs = map (\d -> d </> makeRelFile "index.md") dirs4
    -- "index.docrep" 
    -- putIOwords ["getDirContent2dirs subindexDirs", showPretty subindexDirs]
    ixdirs <- mapM (getFile2index debug doughP bakedP) subindexDirs

    -- putIOwords ["getDirContent2dirs xfiles", showPretty ixfiles, "\n ixdirs", showPretty ixdirs]

    return (catMaybes ixdirs, catMaybes ixfiles)



getFile2index :: NoticeLevel -> Path Abs Dir -> Path Abs Dir -> Path Abs File -> ErrIO (Maybe IndexEntry)
-- get a file and its index
-- collect data for indexentry (but not recursively, only this file)
-- the directories are represented by their index files
-- produce separately to preserve the two groups
getFile2index debug doughP bakedP fnin =
    do
        when (inform debug) $ putIOwords ["getFile2index fnin", showPretty fnin]

        -- (Docrep y1 _) <- read8 fnin docrepFileType
        mdfile <- read8 fnin markdownFileType 
        pd <- readMarkdown2 mdfile
        -- could perhaps "need" all ix as files?

        (Docrep y1 _) <- pandoc2docrep debug doughP bakedP fnin pd
        -- needs the indexentry initialized
        let ix1 :: IndexEntry = dyIndexEntry y1

        when (inform debug) $ putIOwords ["getFile2index ix1", showPretty ix1]

        return . Just $ ix1
