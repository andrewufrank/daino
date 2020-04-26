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
import Lib.IndexMake (makeBothIndex, MenuEntry, IndexEntry)

makeIndex :: Bool
          -> SiteLayout
          -> PubFlags
          -> MetaRec
        --   -> Path Abs Dir
        --   -> Path Abs File
          -> ErrIO MenuEntry

-- | make the index text, will be moved into the page template with templating
-- return zero if not index page
makeIndex debug layout flags metaRec    = do
    -- let doindex = indexPage metaRec
  -- let indexSort1 = indexSort metaRec :: SortArgs
    -- when debug $ 
    putIOwords ["makeIndex", "doindex", showT (indexPage metaRec)]
    if not (indexPage metaRec)
       then return zero 
       else do
            let indexpageFn = makeAbsFile $ fn metaRec 
            let pageFn = makeAbsDir $ getParentDir indexpageFn :: Path Abs Dir
            fs2 :: [FilePath] <- getDirContentFiles (toFilePath pageFn)
            dirs2 :: [FilePath] <- getDirectoryDirs' (toFilePath pageFn) 
            when False $  do 
                putIOline  "makeIndexForDir 2 for" pageFn
                putIOline "index file" (fn metaRec) -- indexpageFn
                putIOline "sort"  (indexSort metaRec)
                putIOline "flags" flags
                putIOlineList "files found" fs2  -- is found
                putIOlineList "dirs found"  dirs2
            let fs4 = filter (indexpageFn /=) . map makeAbsFile 
                        . filter (hasExtension "md") $ fs2 :: [Path Abs File]
            metaRecs2 :: [MetaRec]
                <- mapM (getMetaRec layout) fs4 -- noch ok
            
            --   let fileIxsSorted =
            --         makeIndexEntries dough2 indexFn (indexSort metaRec) metaRecs
            --   --  sortFileEntries (indexSort metaRec)  fileIxs1
            --   let dirIxsSorted2 = makeIndexEntriesDirs (map makeAbsDir dirs)
            --   -- if not (null dirIxsSorted)
            --   --     then dirIxsSorted ++ [zero { title2 = "------" }]
            --   --     else []
            -- putIOwords ["makeIndexForDir 3 fs4",  showT  fs4,"/n"]
            -- putIOwords ["makeIndexForDir 3 metaRecs2", showT metaRecs2]        
            let menu1 = makeBothIndex
                    (doughDir layout)
                    indexpageFn 
                    (indexSort metaRec)
                    (filter (checkPubStateWithFlags flags . publicationState) metaRecs2)
                    (map makeAbsDir dirs2)
            -- MenuEntry { menu2 = dirIxsSorted2 ++ fileIxsSorted }
            -- when debug $ 
            -- putIOwords ["makeIndexForDir 4", "index for dirs sorted ", showT  menu1]
                    -- is empty
            -- let dirIxs = map formatOneDirIndexEntry (map makeAbsDir dirs) :: [IndexEntry]
            -- -- format the subdir entries
            -- let dirIxsSorted = sortWith title2 dirIxs
            --   when debug $ putIOwords ["makeIndexForDir 8", "menu1", showT menu1]
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
    
    