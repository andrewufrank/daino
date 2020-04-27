------------------------------------------------------------------------------
--
-- Module      :   create an index for a directory
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DeriveGeneric #-}

module Lib.Indexing (module Lib.Indexing, getAtKey
            , module Lib.IndexMake 
            ) where

-- import           Uniform.Shake
import           Uniform.FileIO -- (getDirectoryDirs', getDirContentFiles )
-- import Uniform.Filenames (takeBaseName)
import           Uniform.Pandoc (getAtKey)
import Uniform.Strings (putIOline, putIOlineList)
-- import Uniform.Error 
-- DocValue(..)
-- , unDocValue
import           Lib.CmdLineArgs (PubFlags(..))
import           Lib.CheckInput (MetaRec(..), getTripleDoc
                               , PublicationState(..))
-- , readMeta2rec
-- , checkOneMdFile
import           Lib.Foundation (SiteLayout, doughDir)
import Lib.IndexMake (makeBothIndex, MenuEntry, IndexEntry
                , convert2index)

-- | get the conents, separated into dirs and files 
-- indexfile itself is removed 
getDirContent2metarec ::   MetaRec -> ErrIO ([Path Abs Dir], [Path Abs File])
getDirContent2metarec  metaRec = do 
    let indexpageFn =  makeAbsFile (fn metaRec)  :: Path Abs File
    let pageFn = makeAbsDir $ getParentDir indexpageFn :: Path Abs Dir
     
    dirs1 :: [Path Abs Dir] <-  getDirectoryDirs' pageFn
    files1 :: [Path Abs File] <-  getDirContentFiles pageFn
    return  (dirs1, filter (indexpageFn /=)  files1)

-- | produces the index as text 
makeIndex :: Bool           -- ^ debug
          -> SiteLayout     -- ^ layout 
          -> PubFlags       -- ^ flags 
          -> MetaRec        -- ^ the metarec of the current index.md
        --   -> Path Abs Dir
        --   -> Path Abs File
          -> ErrIO MenuEntry

makeIndex debug layout flags metaRec = do 
    metarecs <- makeIndex1 debug layout flags metaRec
    return . convert2index $ metarecs


-- | make the index text, 
-- will be moved into the page template with templating
-- return zero if not index page 
makeIndex1 :: Bool           -- ^ debug
          -> SiteLayout     -- ^ layout 
          -> PubFlags       -- ^ flags 
          -> MetaRec        -- ^ the metarec of the current index.md
        --   -> Path Abs Dir
        --   -> Path Abs File
          -> ErrIO (MetaRec, [MetaRec], [MetaRec])
            -- ^ of this index, the mds and the sub/index.md 
makeIndex1 debug layout flags metaRec    = do
    -- let doindex = indexPage metaRec
  -- let indexSort1 = indexSort metaRec :: SortArgs
    -- when debug $ 
    putIOwords ["makeIndex", "doindex", showT (indexPage metaRec)]
    if not (indexPage metaRec)
        then return zero 
        else do 
            (dirs, files) <- getDirContent2metarec metaRec
        
            -- -- let indexpageFn = makeAbsFile $ fn metaRec 
            -- -- let pageFn = makeAbsDir $ getParentDir indexpageFn :: Path Abs Dir
            -- -- fs2 :: [FilePath] <- getDirContentFiles (toFilePath pageFn)

            -- -- dirs2 :: [FilePath] <- getDirectoryDirs' (toFilePath pageFn) 
            when True $  do 
                -- putIOline  "makeIndexForDir 2 for" pageFn
                putIOline "index file" (fn metaRec) -- indexpageFn
                putIOline "sort"  (indexSort metaRec)
                putIOline "flags" flags
                putIOlineList "files found" (map show files)  -- is found
                putIOlineList "dirs found"  (map show dirs) 

            -- let fs4 =   filter (hasExtension ".md") $ files :: [Path Abs File]
            metaRecsThis :: [MetaRec]
                <- mapM (getMetaRec layout) files -- not filtered md yet!
            
            let subindex = map (\d -> d </> (makeRelFile "index.md")) dirs
            putIOline "subindex" (map show subindex)
            metaRecsSub :: [MetaRec] <- mapM (getMetaRec layout) subindex

            menu1 <- return (metaRec, metaRecsThis, metaRecsSub)
                -- the metarecs for the index in the subdirs 
            
            -- --   let fileIxsSorted =
            -- --         makeIndexEntries dough2 indexFn (indexSort metaRec) metaRecs
            -- --   --  sortFileEntries (indexSort metaRec)  fileIxs1
            -- --   let dirIxsSorted2 = makeIndexEntriesDirs (map makeAbsDir dirs)
            -- --   -- if not (null dirIxsSorted)
            -- --   --     then dirIxsSorted ++ [zero { title2 = "------" }]
            -- --   --     else []
            -- -- putIOwords ["makeIndexForDir 3 fs4",  showT  fs4,"/n"]
            -- -- putIOwords ["makeIndexForDir 3 metaRecs2", showT metaRecs2]        
            -- let menu1 = makeBothIndex
            --         (doughDir layout)
            --         indexpageFn 
            --         (indexSort metaRec)
            --         (filter (checkPubStateWithFlags flags . publicationState) metaRecs2)
            --         (map makeAbsDir dirs2)
            -- -- MenuEntry { menu2 = dirIxsSorted2 ++ fileIxsSorted }
            -- -- when debug $ 
            -- -- putIOwords ["makeIndexForDir 4", "index for dirs sorted ", showT  menu1]
            --         -- is empty
            -- -- let dirIxs = map formatOneDirIndexEntry (map makeAbsDir dirs) :: [IndexEntry]
            -- -- -- format the subdir entries
            -- -- let dirIxsSorted = sortWith title2 dirIxs
            -- --   when debug $ putIOwords ["makeIndexForDir 8", "menu1", showT menu1]
            return menu1

-- | find the metaRec to a path 
getMetaRec :: SiteLayout -> Path Abs File -> ErrIO MetaRec
getMetaRec layout mdfile = do
    (_, metaRec, report) <- getTripleDoc layout mdfile
    return metaRec
            
checkPubStateWithFlags :: PubFlags ->  PublicationState -> Bool

-- check wether the pubstate corresponds to the flag
checkPubStateWithFlags flags  PSpublish = publishFlag flags
checkPubStateWithFlags flags  PSdraft = draftFlag flags
checkPubStateWithFlags flags  PSold = oldFlag flags
checkPubStateWithFlags _  PSzero = False
-- checkPubStateWithFlags flags Nothing = publishFlag flags
    
    