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

import           Uniform.FileIO -- (getDirectoryDirs', getDirContentFiles )
import           Uniform.Pandoc (getAtKey)
import Uniform.Strings (putIOline, putIOlineList)
import           Lib.CmdLineArgs (PubFlags(..))
import           Lib.CheckInput (MetaRec(..), getTripleDoc
                               , PublicationState(..))
import           Lib.Foundation (SiteLayout)
import Lib.IndexMake (MenuEntry, IndexEntry
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
            when debug  $  do 
                -- putIOline  "makeIndexForDir 2 for" pageFn
                putIOline "index file" (fn metaRec) -- indexpageFn
                putIOline "sort"  (indexSort metaRec)
                putIOline "flags" flags
                putIOlineList "files found" (map show files)  -- is found
                putIOlineList "dirs found"  (map show dirs) 

            let files2 =   filter (hasExtension . makeExtension $".md") 
                                    $ files :: [Path Abs File]
            metaRecsThis :: [MetaRec]
                <- mapM (getMetaRec layout) files2 -- not filtered md yet!
            
            let subindex = map (\d -> d </> (makeRelFile "index.md")) dirs
            when debug $ putIOline "subindex" (map show subindex)
            metaRecsSub :: [MetaRec] <- mapM (getMetaRec layout) subindex

            menu1 <- return (metaRec, metaRecsThis, metaRecsSub)
                -- the metarecs for the index in the subdirs 
            return menu1

-- | find the metaRec to a path 
getMetaRec :: SiteLayout -> Path Abs File -> ErrIO MetaRec
getMetaRec layout mdfile = do
    (_, metaRec, report) <- getTripleDoc layout mdfile
    unless (null' report) $ 
        putIOwords ["/n/n Problem with reading MetaRec ", showT mdfile
                    , "/n", report,
                    "/n---------------------------------/n"]
    return metaRec
            
checkPubStateWithFlags :: PubFlags ->  PublicationState -> Bool
-- TODO check pubstate!
-- check wether the pubstate corresponds to the flag
checkPubStateWithFlags flags  PSpublish = publishFlag flags
checkPubStateWithFlags flags  PSdraft = draftFlag flags
checkPubStateWithFlags flags  PSold = oldFlag flags
checkPubStateWithFlags _  PSzero = False
-- checkPubStateWithFlags flags Nothing = publishFlag flags
    
    