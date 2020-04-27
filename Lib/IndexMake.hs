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

import           GHC.Exts (sortWith)
import           Uniform.Json
import Uniform.Filenames
import           Lib.CheckInput (MetaRec(..)
                               , SortArgs(..)
                               , makeRelPath
                               )

-- | convert the metarecs and put some divider between
-- TODO  - avoid dividers if list empty
convert2index :: (MetaRec, [MetaRec], [MetaRec])
                        -> MenuEntry
convert2index  (this, content, subix) = MenuEntry {menu2 = 
            [a]   
            ++ [zero{title2= "--- subdir ---"}] ++ c 
            ++ [zero{title2= "--- content ---"}] ++ b 
            }

    where 
        a = getOneIndexEntryPure this
        b = map getOneIndexEntryPure content 
        c = map getOneIndexEntryPure subix 



getOneIndexEntryPure :: MetaRec  -> IndexEntry
-- | the pure code to compute an IndexEntry
-- Text should be "/Blog/postTufteStyled.html"
getOneIndexEntryPure metaRec  = IndexEntry
  { text2 = s2t . takeBaseName' . fn $ metaRec 
  , link2 = s2t $ setExtension "html". removeExtension . relURL $ metaRec 
  , abstract2 = abstract metaRec
  , title2 = if isZero (title metaRec :: Text ) 
        then  s2t . takeBaseName' . fn $ metaRec  
        else title metaRec
  , author2 =  author metaRec
  , date2 =   showT $ date metaRec
  , publish2 = shownice $ publicationState metaRec
  , isIndex = indexPage metaRec
  }


      ------  S U P P O R T 

      
newtype MenuEntry = MenuEntry { menu2 :: [IndexEntry] }
  deriving (Generic, Eq, Ord, Show)

instance NiceStrings MenuEntry where 
    shownice = showNice

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
    , isIndex :: Bool -- mark for index entries 
    }
  deriving (Generic, Eq, Ord, Show, Read)

instance Zeros IndexEntry where
  zero = IndexEntry zero zero zero zero zero zero zero False

--instance FromJSON IndexEntry
instance ToJSON IndexEntry

instance FromJSON IndexEntry where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }


