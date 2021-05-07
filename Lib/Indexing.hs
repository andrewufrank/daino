----------------------------------------------------------------------
--
-- Module      :   create an index for a directory
--
----------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports 
            -fno-warn-unused-imports 
            #-}

module Lib.Indexing
    where
import UniformBase
import Uniform.Json 

import Uniform.Pandoc
    ( DocRep(DocRep),
      Panrep(Panrep, panyam),
      docRepFileType,
      extDocRep )                --  ( getAtKey )
import           Lib.CmdLineArgs                ( PubFlags(..) )
import           Lib.CheckInput
import           Lib.Foundation                 ( SiteLayout )

addIndex2yam :: Path Abs Dir -> Bool -> Panrep -> ErrIO Panrep
-- ^ the top call to form the index data into the DocYaml
--later only the format for output must be fixed 
addIndex2yam bakedP _  dr@(Panrep yam1 _) = do
    putIOwords ["addIndex2yam", "start", showT yam1]
    x1 :: IndexEntry <- fromJSONerrio yam1
    putIOwords ["addIndex2yam", "x1", showT x1]
    if not.indexPage $ x1
        then return dr
        else do
            putIOwords ["addIndex2yam", "is indexpage"]
            (dirs, files) <- getDirContent2dirs_files (bakedP </> link x1)
            putIOwords ["addIndex2yam", "\n dirs", showT dirs, "\n files" , showT files]
            let x2=x1{dirEntries = dirs, fileEntries = files}
            putIOwords ["addIndex2yam", "x2", showT x2]
            let x2j = toJSON x2
            putIOwords ["addIndex2yam", "x2j", showT x2j]
            let yam2 = mergeLeftPref [x2j, yam1]
            putIOwords ["addIndex2yam", "yam2", showT yam2]
            return dr{panyam=yam2}


-- | get the contents of a directory, separated into dirs and files 
-- the directory is given by the index file 
-- which files to check: index.md (in dough) or index.docrep (in baked)
-- currently checks index.docrep in dough (which are not existing)
-- indexfile itself is removed and files which are not markdown
getDirContent2dirs_files ::   Path Abs File  -> ErrIO ([IndexEntry], [IndexEntry])
getDirContent2dirs_files  indexpageFn = do
    putIOwords ["getDirContent2dir_files", showT indexpageFn]
    -- sucht in dough
    let pageFn = makeAbsDir $ getParentDir indexpageFn :: Path Abs Dir
    -- get the dir in which the index file is embedded
    dirs1 :: [Path Abs Dir] <-  getDirectoryDirs' pageFn
    files1 :: [Path Abs File] <-  getDirContentFiles pageFn
    let files2 = filter (indexpageFn /=)   -- should not exclude all index pages but has only this one in this dir? 
                        . filter (hasExtension extDocRep) $ files1
    ixfiles <- mapM getFile2index files2
    let subindexDirs = map (\d -> d </> makeRelFile "index.docrep") dirs1
    ixdirs <- mapM getFile2index subindexDirs

    return  (catMaybes ixdirs, catMaybes ixfiles)

getFile2index :: Path Abs File -> ErrIO (Maybe IndexEntry)
-- get a file and its index 
-- collect data for indexentry (but not recursively, only this file)
-- the directories are represented by their index files 
-- produce separately to preserve the two groups 
getFile2index fnin = do
        (DocRep y1 _) <- read8 fnin docRepFileType
        ix1 :: IndexEntry <- fromJSONerrio y1
        return . Just $ ix1
    `catchError`  (\e  -> do
            putIOwords ["getFile2index error caught\n", "fn:"
                , showT fnin, "\n", showT e ] -- " showT msg])
            return Nothing
            )

