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
 
import Foundational.CmdLineFlags 
import Foundational.SettingsPage
import UniformBase

import Uniform.Shake
import Foundational.Filetypes4sites

-- import Data.List (sortOn)

getIndexFiles ::   [IndexEntry2] -> [Path Rel File]
-- collect the index files for files and dirs (/index)
getIndexFiles   ixs = is 
    where 
        is = map makeRelFile $ map ixfn ixs 

filterDNB :: Text -> [Path Abs Dir] -> [Path Abs Dir]
-- remove all directories which should not be processed 
-- into the blog etc. 
-- dnbString is typically: doNotBake (siteLayout sett4) 
-- does not check for publish 
--  (which is in the docrep construction)
filterDNB dnbString dirs1 = dirs5 
    where
        dirs2 =  filter (not . (isInfixOf' dnbString). s2t 
                . getNakedDir) dirs1
        dirs3 = filter ( not . (isPrefixOf' resourcesName) 
                .  getNakedDir) dirs2
        dirs4 = filter ( not . (isPrefixOf'  templatesName) 
                . getNakedDir) dirs3
        dirs5 = filter ( not . (isPrefixOf' "." ) 
                .   getNakedDir) dirs4

collectIndex :: NoticeLevel -> PubFlags -> Settings   
        ->   Path Abs Dir -> DainoValues -> ErrIO DainoValues
{- ^ the top call to collect the index data into the MetaPage
-- files and dirs 
-- starts with dir of index
-- searches in dough!
-}
collectIndex debug pubf sett4 fn dv1 = do
    putInform debug ["collectIndex 1", "start", showPretty fn]

    let layout = siteLayout sett4 
        doughP = doughDir layout -- the regular dough
        bakedP = bakedDir layout

    (dirs, files) :: ([Path Abs Dir], [Path Abs File]) <- getDirContent2dirs_files NoticeLevel0 pubf sett4 doughP  fn

    putInform NoticeLevel2 ["collectIndex 2 dough!", "\n dirs"
                    , showT dirs, "\n files", showT files]

    -- check for publishable: test docrep not zero 
    -- put needs 
    -- change to search in bakedP 

    let files1 = map (replaceDirectoryP doughP bakedP) files
    let dirs2 = dirs -- catMaybes $ map check2publishDirs dirs 
    files2m <- mapM check2publishFiles files1
    let files2 =  catMaybes files2m  

    -- let    mdfs = mdFiles pubf :: [Path Abs File]
    -- let dirs2 = catMaybes (map (check2publishDirs doughP mdfs) dirs)
    -- let files2 = catMaybes (map (check2publishFiles doughP mdfs) files)
        -- map ((\fp -> addFileName fp (makeRelFile "index"))  . 
                --   removeExtension . makeRelativeP doughP)   dirs 
                        -- :: [Path Rel File]
    let dv2 = dv1{dirEntries = map (initializeIx2dir (mdFiles pubf) doughP) dirs2
                    , fileEntries = map (initializeIx2file (mdFiles pubf) doughP) files2}

    putInform debug ["collectIndex 3"
        , "\ndv2 dirs ixfn", showT (map ixfn . dirEntries $ dv2)
        , "\ndv2 dirs link", showT (map link . dirEntries $ dv2)
        , "\ndv2 files ixfn", showT (map ixfn . fileEntries $ dv2)
        , "\ndv2 files link", showT (map link . fileEntries $ dv2)
        ]
    return dv2

check2publishDirs :: Path Abs File -> ErrIO (Maybe (Path Abs File))
-- -- a directory is published if the index file is set to publish 
    -- test simply if the docrep file exists 
check2publishDirs   ds = return $ Just ds -- if True then Just ds else Nothing 
--         where t = check2publish dough mdfs ((addFileName ds) (makeRelFile "index"))  
check2publishFiles ::Path Abs File -> ErrIO (Maybe (Path Abs File))
check2publishFiles   fs = check2publish fs 
                                      

check2publish ::  Path Abs File -> ErrIO (Maybe (Path Abs File))
check2publish inputFn = do 
    doc1 <- read8 inputFn docrepFileType
    return . Just $ inputFn 
    -- if (doc1 == zero) then return Nothing else return $ Just inputFn 

    -- where   t = True  -- TODO get old tests back -- fs2 `elem` mdfs
            -- fs2 = makeRelativeP doughP . removeExtension $ fs

initializeIx2dir :: [Path Abs File] -> Path Abs Dir -> Path Abs Dir -> IndexEntry2 
-- the dough path to make the path relative
-- set the index file, not the directory itself 
initializeIx2dir mdfs doughP fp = zero{ixfn =   relfp2
                    , link = toFilePath relfp}
        where 
            -- fp3 = filter (\fn -> elem fn mdfs) $ map toFilePath fp2
            relfp2 = (toFilePath relfp) </> ("index" :: FilePath) :: FilePath 
                -- addFileName fp (makeRelFile "index") :: [Path Rel File]
            relfp =   makeRelativeP doughP  fp  

initializeIx2file :: [Path Abs File] -> Path Abs Dir -> Path Abs File -> IndexEntry2 
-- the dough path to make the path relative
initializeIx2file mdfs doughP fp = zero{ixfn = toFilePath linkfp
                    , link = toFilePath linkfp
                    }
        where 
            -- fp = filter (\fn -> elem fn mdfs) relfp
            linkfp =  removeExtension relfp 
            relfp = makeRelativeP doughP $ fp  
{-  old
get the contents of a directory, separated into dirs and files
 the directory is given by the index dir file
 
-}
getDirContent2dirs_files :: NoticeLevel -> PubFlags -> Settings 
        -> Path Abs Dir ->  Path Abs Dir 
        -> ErrIO ([Path Abs Dir], [Path Abs File])
-- get the dirs and files, exclude based on filename
-- but not checked for inclusion 
getDirContent2dirs_files debug pubf sett4 doughP   indexDir = do
    putInform debug ["getDirContent2dirs_files for", showPretty indexDir]
    -- let pageFn = makeAbsDir $ getParentDir indexpageFn :: Path Abs Dir
    -- -- get the dir in which the index file is embedded
    -- putInform debug ["getDirContent2dirs_files pageFn", showPretty pageFn]

    dirs1 :: [Path Abs Dir] <- getDirectoryDirs' indexDir
    let dirs2 =  filter (not . isInfixOf' (doNotBake (siteLayout sett4)). s2t . getNakedDir) dirs1
    let dirs3 = filter ( not . (isPrefixOf' resourcesName) .  getNakedDir) dirs2
    let dirs4 = filter ( not . (isPrefixOf'  templatesName) . getNakedDir) dirs3
    let dirs5 = filter ( not . (isPrefixOf' "." ) .   getNakedDir) dirs4
    -- TODO may need extension (change to list of excluded)
    -- build from constants in foundation

    putInform debug ["\ngetDirContent2dirs_files dirs4", showPretty dirs5]
    let ixdirs = dirs5

    files1 :: [Path Abs File] <- getDirContentFiles indexDir

    putInform debug ["getDirContent2dirs_files files1", showPretty files1]
 
    let files2 =
            filter (("index" /=) . getNakedFileName) -- should not exclude all index pages but has only this one in this dir?
                . filter (hasExtension extMD)  
                . filter (not . isInfixOf' (doNotBake (siteLayout sett4)) . s2t. toFilePath)
                $ files1
    putInform debug ["getDirContent2dirs files2", showPretty files2]
    let ixfiles = files2

    putInform debug ["getDirContent2dirs xfiles", showPretty ixfiles, "\n ixdirs", showPretty ixdirs]

    return ( ixdirs,  ixfiles)

