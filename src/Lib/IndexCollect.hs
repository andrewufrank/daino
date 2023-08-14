----------------------------------------------------------------------
--
-- Module      :   collect the files for the index for a directory
---- | create an index for a directory
--  in two steps: 
--  IndexCollect collect all files and dirs in the directory
-- where the index file is 
    -- old 
    --  with call to collectIndex
    --  and
    --  indexmake: convert collected data for printing (convertIndexEntries)
    --  .
    --  the data is stored in a file separately and managed by Shake
    --  operates on metapage (or less? )
----------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans
            -fno-warn-missing-signatures
            -fno-warn-missing-methods
            -fno-warn-duplicate-exports
            -fno-warn-unused-imports
            -fno-warn-unused-matches #-}

            
module Lib.IndexCollect where

-- import Uniform.Json  
import Uniform.Pandoc ( extMD )
import Uniform.Latex()
import Foundational.Filetypes4sites  
import Foundational.MetaPage
 
import Foundational.CmdLineFlags ( PubFlags )
import Foundational.SettingsPage
import UniformBase

import Wave.Md2doc
    -- ( includeBakeTest3docrep, readMarkdownFile2docrep ) 
-- import ShakeBake.Bake (readMarkdownFile2docrep)
import Foundational.SettingsPage  
import Data.List (sortOn)



collectIndex :: NoticeLevel -> PubFlags -> Settings -> Path Abs Dir ->   Path Abs Dir -> ErrIO IndexEntry2
{- ^ the top call to collect the index data into the MetaPage
-- files and dirs 
-- starts with dir of index
-}
collectIndex debug pubf sett4 doughP  fn = do
    when (inform debug) $ putIOwords ["collectIndex", "start", showPretty fn]

    -- let fn = doughP </> (link ix1) :: Path Abs File
    -- changed to search in dough (but no extension yet)

    -- when (inform debug) $  
    --     putIOwords
    --         [ "collectIndex", "fn", showT fn]
    -- unless (isIndexPage fn) $ errorT ["collectIndex should only be called for indexPage True"]

    (dirs, files) <- getDirContent2dirs_files debug pubf sett4 doughP  fn
    -- let
    --     dirs1 = sortField dirs 
    --     files1 = sortField files
    -- sort entries -- sort when produce output

    when (inform debug) $ putIOwords ["collectIndex", "\n dirs", showT dirs, "\n files", showT files]
    -- recurse for dirs 
    -- dirs2 <- mapM (collectIndex debug pubf sett4 doughP indexSortField) dirs1

    -- when (inform debug) $ putIOwords ["collectIndex recursion", "\n dirs2", showT dirs2]    

    let ix2 = zero{dirEntries = map toFilePath  dirs
                    , fileEntries = map toFilePath files}
    when (inform debug) $ putIOwords ["collectIndex", "x2", showT ix2]
    return ix2

--   where 
--     sortField = case (indexSortField) of 
--         Just "filename" -> sortOn link
--         Just "date"      -> sortOn date 
--         Just "reversedate" -> reverse . sortOn  date
--         _ -> sortOn link   -- what is best default? id?


{-  old
get the contents of a directory, separated into dirs and files
 the directory is given by the index file
 which files to check: index.md (in dough) or index.docrep (in baked)
 currently checks index.docrep in dough (which are not existing)
 indexfile itself is removed and files which are not markdown
-}
getDirContent2dirs_files :: NoticeLevel -> PubFlags -> Settings -> Path Abs Dir ->  Path Abs Dir -> ErrIO ([Path Abs Dir], [Path Abs File])
-- get the dirs and files, exclude based on filename
getDirContent2dirs_files debug pubf sett4 doughP   indexDir = do
    when (inform debug) $ putIOwords ["getDirContent2dirs_files for", showPretty indexDir]
    -- let pageFn = makeAbsDir $ getParentDir indexpageFn :: Path Abs Dir
    -- -- get the dir in which the index file is embedded
    -- when (inform debug) $ putIOwords ["getDirContent2dirs_files pageFn", showPretty pageFn]

    dirs1 :: [Path Abs Dir] <- getDirectoryDirs' indexDir
    let dirs2 =  filter (not . isInfixOf' (doNotBake (siteLayout sett4)). s2t . getNakedDir) dirs1
    let dirs3 = filter ( not . (isPrefixOf' resourcesName) .  getNakedDir) dirs2
    let dirs4 = filter ( not . (isPrefixOf'  templatesName) . getNakedDir) dirs3
    let dirs5 = filter ( not . (isPrefixOf' "." ) .   getNakedDir) dirs4
    -- TODO may need extension (change to list of excluded)
    -- build from constants in foundation

    when (inform debug) $ putIOwords ["\ngetDirContent2dirs_files dirs4", showPretty dirs5]
    let ixdirs = dirs5

    files1 :: [Path Abs File] <- getDirContentFiles indexDir

    when (inform debug) $ putIOwords ["getDirContent2dirs_files files1", showPretty files1]
 
    let files2 =
            filter (("index" /=) . getNakedFileName) -- should not exclude all index pages but has only this one in this dir?
                . filter (hasExtension extMD)  
                . filter (not . isInfixOf' (doNotBake (siteLayout sett4)) . s2t. toFilePath)
                $ files1
    when (inform debug) $ putIOwords ["getDirContent2dirs files2", showPretty files2]
    let ixfiles = files2
    -- ixfiles <- mapM (getFile2index debug pubf sett4 ) files2

    -- when (inform debug) $ putIOwords ["getDirContent2dirs ixfiles", showPretty ixfiles]

    -- let subindexDirs = map (\d -> d </> makeRelFile "index.md") dirs5
    
    -- when (inform debug) $  putIOwords ["getDirContent2dirs subindexDirs", showPretty subindexDirs]
    -- ixdirs <- mapM (getFile2index  debug pubf sett4 ) subindexDirs

    when (inform debug) $ putIOwords ["getDirContent2dirs xfiles", showPretty ixfiles, "\n ixdirs", showPretty ixdirs]

    -- return (catMaybes ixdirs, catMaybes ixfiles)
    return ( ixdirs,  ixfiles)



-- getFile2index :: NoticeLevel -> PubFlags -> Settings  -> Path Abs File -> ErrIO (Maybe IndexEntry2)
-- -- get a file and its index
-- -- collect data for indexentry (but not recursively, only this file)
-- -- the directories are represented by their index files
-- -- produce separately to preserve the two groups
-- -- the excluded (do not bake) files are filtered out before
-- getFile2index debug pubf sett4   fnin =
--     do
--         when (inform debug) $ putIOwords ["getFile2index fnin", showPretty fnin
--                 ,"\ndebug", showT debug]

--         -- mdfile <- read8 fnin markdownFileType 
--         -- pd <- readMarkdown2 mdfile
--         -- -- could perhaps "need" all ix as files?

--         -- let (Docrep y1 _) = pandoc2docrep doughP fnin pd
--         metaplus_y1 <- readMarkdownFile2docrep debug sett4  fnin 
--         -- needs the indexentry initialized
--         -- does include the DNB files, bombs with ff ligature
--         let incl = includeBakeTest3docrep pubf (metap metaplus_y1)
        
--         if incl then do
--             let ix1 :: IndexEntry2 =  indexEntry . extra $ metaplus_y1 -- dyIndexEntry y1
--             when (inform debug) $ putIOwords ["getFile2index ix1", showPretty ix1]
--             return . Just $ ix1
                        
--         else return Nothing 


