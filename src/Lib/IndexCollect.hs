----------------------------------------------------------------------
--
-- Module      :   create an index for a directory
---- | create an index for a directory
--  in two steps: 
--  IndexCollect collect all the date
--  with call to completeIndex
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
import Uniform.Latex
import Foundational.Filetypes4sites  
import Foundational.MetaPage
 
import Foundational.CmdLineFlags ( PubFlags )

import UniformBase

import Wave.Md2doc
    ( includeBakeTest3docrep, readMarkdownFile2docrep ) 
-- import ShakeBake.Bake (readMarkdownFile2docrep)
import Foundational.SettingsPage  
import Data.List (sortOn)



-- completeIndex :: NoticeLevel -> PubFlags -> Settings -> Path Abs Dir -> Maybe Text ->   IndexEntry -> ErrIO IndexEntry
-- {- ^ the top call to form the index data into the MetaPage
-- later only the format for output must be fixed
-- -}
-- completeIndex debug pubf sett4 doughP indexSortField ix1 = do
--     when (inform debug) $ putIOwords ["completeIndex", "start", showPretty ix1]

--     let fn = doughP </> (link ix1) :: Path Abs File
--     -- changed to search in dough (but no extension yet)

--     when (inform debug) $   -- to have indication where error is if pandoc error 
--         putIOwords
--             [ "completeIndex"
--             , "fn"
--             , showT fn
--             ]
--     -- unless (isIndexPage fn) $ errorT ["completeIndex should only be called for indexPage True"]

--     (dirs, files) <- getDirContent2dirs_files debug pubf sett4 doughP  fn
--     let
--         dirs1 = sortField dirs 
--         files1 = sortField files
--     -- sort entries 

--     when (inform debug) $ putIOwords ["completeIndex", "\n dirs", showT dirs, "\n files", showT files]
--     -- recurse for dirs 
--     dirs2 <- mapM (completeIndex debug pubf sett4 doughP indexSortField) dirs1


--     when (inform debug) $ putIOwords ["completeIndex recursion", "\n dirs2", showT dirs2]    

--     let ix2 = ix1{dirEntries = dirs2, fileEntries = files1}
--     when (inform debug) $ putIOwords ["completeIndex", "x2", showT ix2]
--     return ix2

--   where 
--     sortField = case (indexSortField) of 
--         Just "filename" -> sortOn link
--         Just "date"      -> sortOn date 
--         Just "reversedate" -> reverse . sortOn  date
--         _ -> sortOn link   -- what is best default? id?


-- {- | get the contents of a directory, separated into dirs and files
--  the directory is given by the index file
--  which files to check: index.md (in dough) or index.docrep (in baked)
--  currently checks index.docrep in dough (which are not existing)
--  indexfile itself is removed and files which are not markdown
-- -}
-- getDirContent2dirs_files :: NoticeLevel -> PubFlags -> Settings -> Path Abs Dir ->  Path Abs File -> ErrIO ([IndexEntry], [IndexEntry])
-- getDirContent2dirs_files debug pubf sett4 doughP   indexpageFn = do
--     when (inform debug) $ putIOwords ["getDirContent2dirs_files for", showPretty indexpageFn]
--     let pageFn = makeAbsDir $ getParentDir indexpageFn :: Path Abs Dir
--     -- get the dir in which the index file is embedded
--     when (inform debug) $ putIOwords ["getDirContent2dirs_files pageFn", showPretty pageFn]

--     dirs1 :: [Path Abs Dir] <- getDirectoryDirs' pageFn
--     let dirs2 =  filter (not . isInfixOf' (doNotBake (siteLayout sett4)). s2t . getNakedDir) dirs1
--     let dirs3 = filter ( not . (isPrefixOf' resourcesName) .  getNakedDir) dirs2
--     let dirs4 = filter ( not . (isPrefixOf'  templatesName) . getNakedDir) dirs3
--     let dirs5 = filter ( not . (isPrefixOf' "."
--          ) .   getNakedDir) dirs4
--     -- TODO may need extension (change to list of excluded)
--     -- build from constants in foundation

--     when (inform debug) $ putIOwords ["\ngetDirContent2dirs_files dirs4", showPretty dirs4]

--     files1 :: [Path Abs File] <- getDirContentFiles pageFn

--     when (inform debug) $ putIOwords ["getDirContent2dirs_files files1", showPretty files1]
 
--     let files2 =
--             filter (indexpageFn /=) -- should not exclude all index pages but has only this one in this dir?
--                 . filter (hasExtension extMD)  
--                 . filter (not . isInfixOf' (doNotBake (siteLayout sett4)) . s2t. toFilePath)
--                 $ files1
--     when (inform debug) $ putIOwords ["getDirContent2dirs files2", showPretty files2]
--     -- let hpname = blogAuthorToSuppress sett4
--     ixfiles <- mapM (getFile2index debug pubf sett4 ) files2

--     when (inform debug) $ putIOwords ["getDirContent2dirs ixfiles", showPretty ixfiles]

--     let subindexDirs = map (\d -> d </> makeRelFile "index.md") dirs5
    
--     when (inform debug) $  putIOwords ["getDirContent2dirs subindexDirs", showPretty subindexDirs]
--     ixdirs <- mapM (getFile2index  debug pubf sett4 ) subindexDirs

--     when (inform debug) $ putIOwords ["getDirContent2dirs xfiles", showPretty ixfiles, "\n ixdirs", showPretty ixdirs]

--     return (catMaybes ixdirs, catMaybes ixfiles)



-- getFile2index :: NoticeLevel -> PubFlags -> Settings  -> Path Abs File -> ErrIO (Maybe IndexEntry)
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
--         (Docrep y1 _) <- readMarkdownFile2docrep debug sett4  fnin 
--         -- needs the indexentry initialized
--         -- does include the DNB files, bombs with ff ligature
--         let incl = includeBakeTest3docrep pubf y1
        
--         if incl then do
--             let ix1 :: IndexEntry =  dyIndexEntry y1
--             when (inform debug) $ putIOwords ["getFile2index ix1", showPretty ix1]
--             return . Just $ ix1
                        
--         else return Nothing 


