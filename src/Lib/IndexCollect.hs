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
 
import Foundational.CmdLineFlags ( PubFlags )
import Foundational.SettingsPage
import UniformBase

import Wave.Md2doc
import Uniform.Shake

import Foundational.SettingsPage  
import Data.List (sortOn)



collectIndex :: NoticeLevel -> PubFlags -> Settings -> Path Abs Dir 
        ->   Path Abs Dir -> DainoValues -> ErrIO DainoValues
{- ^ the top call to collect the index data into the MetaPage
-- files and dirs 
-- starts with dir of index
-}
collectIndex debug pubf sett4 doughP fn dv1 = do
    when (inform debug) $ putIOwords ["collectIndex", "start", showPretty fn]


    (dirs, files) <- getDirContent2dirs_files debug pubf sett4 doughP  fn
 
    when (inform debug) $ putIOwords ["collectIndex", "\n dirs"
                    , showT dirs, "\n files", showT files]
    
    let dv2 = dv1{dirEntries = map (initializeIx2dir doughP) dirs
                    , fileEntries = map (initializeIx2file doughP) files}

    when (inform debug) $ putIOwords ["collectIndex", "dv2", showT dv2]
    return dv2

initializeIx2dir :: Path Abs Dir -> Path Abs Dir -> IndexEntry2 
-- the dough path to make the path relative
-- set the index file, not the directory itself 
initializeIx2dir doughP fp = zero{ixfn = toFilePath fp2
                    , link = toFilePath relfp}
        where 
            fp2 = addFileName fp (makeRelFile "index.md")
            relfp =  removeExtension . makeRelativeP doughP $ fp2  

initializeIx2file :: Path Abs Dir -> Path Abs File -> IndexEntry2 
-- the dough path to make the path relative
initializeIx2file doughP fp = zero{ixfn = toFilePath fp
                    , link = toFilePath relfp
                    }
        where 
            relfp =  removeExtension . makeRelativeP doughP $ fp  
{-  old
get the contents of a directory, separated into dirs and files
 the directory is given by the index dir file
 
-}
getDirContent2dirs_files :: NoticeLevel -> PubFlags -> Settings 
        -> Path Abs Dir ->  Path Abs Dir 
        -> ErrIO ([Path Abs Dir], [Path Abs File])
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

    when (inform debug) $ putIOwords ["getDirContent2dirs xfiles", showPretty ixfiles, "\n ixdirs", showPretty ixdirs]

    return ( ixdirs,  ixfiles)

