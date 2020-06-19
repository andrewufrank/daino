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
{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports 
            -fno-warn-unused-imports 
            #-}
            
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

            let files2 =   filter (hasExtension . makeExtension $"md") 
                                    $ files :: [Path Abs File]
            metaRecsThis :: [MetaRec]
                <- mapM (getMetaRec debug layout) files2 -- not filtered md yet!
            when debug $ putIOline "metaRecsThis 1" metaRecsThis

            let subindexDirs = map (\d -> d </> (makeRelFile "index.md")) dirs
            -- only the index files existing 
            when debug $ putIOline "subindexDirs 1" subindexDirs
            subindexDirs2 <- filterM (doesFileExist' ) subindexDirs 
            -- only the dirs with an index file
            metaRecsSub :: [MetaRec] <- mapM (getMetaRec debug layout) subindexDirs2
            
            when debug $ putIOline "subindex 2 - metarecSub\n"   metaRecsSub 

            menu1 <- return (metaRec, metaRecsThis, metaRecsSub)
                -- the metarecs for the index in the subdirs 
            when debug $ putIOwords ["subindex", "end"]
            return menu1

-- | find the metaRec to a path and report on errors in input data  
getMetaRec :: Bool -> SiteLayout -> Path Abs File -> ErrIO MetaRec
getMetaRec debug layout mdfile = do
    when debug $ putIOwords ["getMetaRec mdfile", showT mdfile]

    (_, metaRec, report1) <- getTripleDoc layout mdfile
    let reportX :: Text 
        reportX = if  isNothing report1 then ""
         else -- (concat'["", report1, "X"] :: Text)
                concatT ["Problem with reading MetaRec for ", showT . relURL $ metaRec
                    ,   fromJustNote "getMetaRecr wrerwdd" report1,
                    "\n ---------------------------------"]  
    putIOwords [reportX]
    return metaRec
            
checkPubStateWithFlags :: PubFlags ->  PublicationState -> Bool
-- TODO check pubstate!
-- check wether the pubstate corresponds to the flag
checkPubStateWithFlags flags  PSpublish = publishFlag flags
checkPubStateWithFlags flags  PSdraft = draftFlag flags
checkPubStateWithFlags flags  PSold = oldFlag flags
checkPubStateWithFlags _  PSzero = False
-- checkPubStateWithFlags flags Nothing = publishFlag flags
    
    