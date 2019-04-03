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
{-# LANGUAGE DeriveGeneric #-}

module Lib.Indexing (module Lib.Indexing, getAtKey
            , module Lib.IndexMake 
            ) where

import           Uniform.Shake
import           Uniform.FileIO (getDirectoryDirs', getDirContentFiles )
import           Uniform.Pandoc (getAtKey)
import Uniform.Strings (putIOline, putIOlineList)
-- DocValue(..)
-- , unDocValue
import           Lib.CmdLineArgs (PubFlags(..))
import           Lib.CheckInput (MetaRec(..), checkOneMdFile
                               , PublicationState(..))
-- , readMeta2rec
-- , checkOneMdFile
import           Lib.Foundation (SiteLayout)
import Lib.IndexMake (makeBothIndex, MenuEntry, IndexEntry)
makeIndex :: Bool
          -> SiteLayout
          -> PubFlags
          -> MetaRec
          -> Path Abs Dir
          -> Path Abs File
          -> ErrIO MenuEntry

-- | make the index text, will be moved into the page template with templating
-- return zero if not index page
makeIndex debug layout flags metaRec dough2 indexpageFn  = do
    let doindex = fromMaybe False $ indexPage metaRec
  -- let indexSort1 = indexSort metaRec :: SortArgs
    when debug $ putIOwords ["makeIndex", "doindex", showT doindex]
    if not doindex
       then return zero 
       else do
            let pageFn = makeAbsDir $ getParentDir indexpageFn :: Path Abs Dir
            fs2 :: [FilePath] <- getDirContentFiles (toFilePath pageFn)
            dirs2 :: [FilePath] <- getDirectoryDirs' (toFilePath pageFn) 
            when debug $ do 
                putIOline  "makeIndexForDir 2 for" pageFn
                putIOline "index file" indexpageFn
                putIOline "sort"  (indexSort metaRec)
                putIOline "flags" flags
                putIOlineList "files found" fs2
                putIOlineList "dirs found"  dirs2
                    
            let fs4 = filter (indexpageFn /=) . map makeAbsFile $ fs2 :: [Path Abs File]
            metaRecs2 :: [(Path Abs File, MetaRec)]
                <- mapM (getMetaRecs layout) fs4
            
            --   let fileIxsSorted =
            --         makeIndexEntries dough2 indexFn (indexSort metaRec) metaRecs
            --   --  sortFileEntries (indexSort metaRec)  fileIxs1
            --   let dirIxsSorted2 = makeIndexEntriesDirs (map makeAbsDir dirs)
            --   -- if not (null dirIxsSorted)
            --   --     then dirIxsSorted ++ [zero { title2 = "------" }]
            --   --     else []
            let menu1 = makeBothIndex
                    dough2
                    indexpageFn
                    (indexSort metaRec)
                    (filter (checkPubStateWithFlags flags . publicationState . snd) metaRecs2)
                    (map makeAbsDir dirs2)
            -- MenuEntry { menu2 = dirIxsSorted2 ++ fileIxsSorted }
            when debug
                $ putIOwords ["makeIndexForDir 4", "index for dirs sorted ", showT $ menu1]
            -- let dirIxs = map formatOneDirIndexEntry (map makeAbsDir dirs) :: [IndexEntry]
            -- -- format the subdir entries
            -- let dirIxsSorted = sortWith title2 dirIxs
            --   when debug $ putIOwords ["makeIndexForDir 8", "menu1", showT menu1]
            return menu1


getMetaRecs :: SiteLayout -> Path Abs File -> ErrIO (Path Abs File, MetaRec)
getMetaRecs layout mdfile = do
    (_, metaRec, report) <- checkOneMdFile layout mdfile
    return (mdfile, metaRec)
            
checkPubStateWithFlags :: PubFlags -> Maybe PublicationState -> Bool

-- check wether the pubstate corresponds to the flag
checkPubStateWithFlags flags (Just PSpublish) = publishFlag flags
checkPubStateWithFlags flags (Just PSdraft) = draftFlag flags
checkPubStateWithFlags flags (Just PSold) = oldFlag flags
checkPubStateWithFlags _ (Just PSzero) = False
checkPubStateWithFlags flags Nothing = publishFlag flags
    
    