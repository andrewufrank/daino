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

module Lib.IndexMake (module Lib.IndexMake) where

import           Uniform.Shake
import           GHC.Exts (sortWith)
import           Uniform.Json
import           Uniform.Json (FromJSON(..))
import           Uniform.Time (year2000)
import Uniform.Filenames (makeAbsFile)
import           Lib.CheckInput (MetaRec(..)
                               , SortArgs(..)
                               , makeRelPath
                               )

makeBothIndex :: Path Abs Dir -> Path Abs File -> SortArgs 
      -> [MetaRec] -> [Path Abs Dir] -> MenuEntry
makeBothIndex dough2 indexFn sortFlag metaRecs dirs =
  MenuEntry { menu2 = dirIxsSorted2 ++ fileIxsSorted }
  where
    fileIxsSorted = makeIndexEntries dough2 indexFn sortFlag metaRecs

    dirIxsSorted2 = makeIndexEntriesDirs dough2 dirs

    ------------ F O R   D I R 
makeIndexEntriesDirs :: Path Abs Dir -> [Path Abs Dir] -> [IndexEntry]
makeIndexEntriesDirs dough dirs =
  if not (null dirIxsSorted)
  then dirIxsSorted ++ [zero { title2 = "------" }]
  else []
  where
    dirIxsSorted = sortWith title2 dirIxs

    dirIxs = map (formatOneDirIndexEntry dough) dirs :: [IndexEntry]

-- filterIndexForFiles :: Path Abs File -> [FilePath] -> [Path Abs File]
-- filterIndexForFiles indexFn fs = fs4
--   where
--     fs2 = filter (/= toFilePath indexFn) fs -- exclude index

--     fs3 = filter (hasExtension "md") fs2

--     fs4 = map makeAbsFile fs3

formatOneDirIndexEntry :: Path Abs Dir -> Path Abs Dir -> IndexEntry

-- format an entry for a subdir
-- fn is name of dir, the link should to to ../index.html 
formatOneDirIndexEntry dough2 fn = zero
  { text2 = s2t (getNakedDir fn :: FilePath)
  , link2 = s2t $ makeRelPath dough2 (fn </> (makeRelFile "index.html") :: Path Abs File)
  -- should add to the index file (to be found by search for index set in metaRec)

  , title2 = baseName1 <> " (subdirectory)"
  }
  where
    baseName1 = s2t (getNakedDir fn :: FilePath)

    -- s2t . takeBaseName . toFilePath $ fn
    -- linkName = makeRelLink2  "index.md" 
    -- dough2 (fn </> (makeRelFile "index.mt"))


---------------- F O R    F I L E S 

sortFileEntries :: SortArgs -> [Maybe IndexEntry] -> [IndexEntry]
sortFileEntries sortArg fileIxsMs = case sortArg of
  SAtitle       -> sortWith title2 fileIxs
  SAdate        -> sortWith date2 fileIxs
  SAreverseDate -> reverse $ sortWith date2 fileIxs
  SAzero        -> fileIxs --    errorT ["makeIndexForDir fileIxsSorted", showT SAzero]
  where
    fileIxs = catMaybes fileIxsMs

makeIndexEntries :: Path Abs Dir
                 -> Path Abs File
                 -> SortArgs
                 -> [MetaRec]
                 -> [IndexEntry]

-- reduce the index entries 
makeIndexEntries dough indexFile sortArg pms =
  sortFileEntries sortArg . map (makeOneIndexEntry dough indexFile) $ pms

makeOneIndexEntry :: Path Abs Dir
                  -> Path Abs File
                  -> MetaRec
                  -> Maybe IndexEntry
makeOneIndexEntry dough2 indexFile (metaRec) =
  if hasExtension (makeExtensionT "md") fn1 || fn1 /= indexFile
  then Just $ getOneIndexEntryPure metaRec 
  else Nothing
  where
    -- linkName = makeRelLink2 $ toFilePath fn1 -- dough2 fn1
    fn1 = makeAbsFile $ fn metaRec

getOneIndexEntryPure :: MetaRec  -> IndexEntry
-- | the pure code to compute an IndexEntry
-- Text should be "/Blog/postTufteStyled.html"
getOneIndexEntryPure metaRec  = IndexEntry
  { text2 = s2t . takeBaseName . fn $ metaRec 
  , link2 = s2t $ setExtension "html". removeExtension . relURL $ metaRec 
  , abstract2 = abstract metaRec
  , title2 = if isZero (title metaRec :: Text ) 
        then  (s2t . takeBaseName . fn $ metaRec )  
        else title metaRec
  , author2 =  author metaRec
  , date2 =   showT $ date metaRec
  , publish2 = shownice $ publicationState metaRec
  }


      ------  S U P P O R T 

      
newtype MenuEntry = MenuEntry { menu2 :: [IndexEntry] }
  deriving (Generic, Eq, Ord, Show)

instance NiceStrings MenuEntry

instance Zeros MenuEntry where
  zero = MenuEntry zero

instance FromJSON MenuEntry

instance ToJSON MenuEntry

data IndexEntry =
  IndexEntry
    { text2 :: Text  -- ^ naked filename -- not shown
    , link2 :: Text -- ^ the url relative to dough dir
    , title2 :: Text -- ^ the title as shown
    , abstract2 :: Text
    , author2 :: Text
    , date2 :: Text -- UTCTime -- read the time early one to find errors
    , publish2 :: Text
    }
  deriving (Generic, Eq, Ord, Show, Read)

instance Zeros IndexEntry where
  zero = IndexEntry zero zero zero zero zero zero zero

--instance FromJSON IndexEntry
instance ToJSON IndexEntry

instance FromJSON IndexEntry where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }


