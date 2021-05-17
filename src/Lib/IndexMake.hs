{-# LANGUAGE DeriveGeneric         #-}
---------------------------------------------------------------------
--
-- Module      :   
-- | create an index for a directory
--  in two steps: 
--  indexing: collect all the date 
--  with call to addIndex2yam
--  and
--  convert collected data for printing (convertIndexEntries)
--  . 
--  the data is stored in a file separately and managed by Shake
--  operates on metapage (or less? )
----------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Lib.IndexMake (module Lib.IndexMake) where

-- import           Foundational.Filetypes4sites
import           Foundational.MetaPage
import           Uniform.Json
import           UniformBase
import Uniform2.HTMLout


convertIndexEntries :: MetaPage -> ErrIO MetaPage
-- ^ take the index entries and convert their
-- form and push them back into the json
convertIndexEntries y =
  do
  --   yentry :: IndexEntry <- fromJSONerrio y
    let yentry = zero -- was y ??
    putIOwords ["convertIndexEntries", "start yentry", showT yentry]
    let dirs = undef "adsfwer" -- dyDirEntries y
    let fils = undef "asdfasd" -- fileEntries dyFileEntries MetaPage

    -- dirs <- fromJustNote "convertIndexEntries werdsdx" $ getAtKey y "DirEntries"
    -- files <- fromJustNote "convertIndexEntries 34634" $ getAtKey y "FileEntries"
    -- meta :: IndexEntry  <- fromJSONerrio y

    -- metadirs <- mapM fromJSONerrio dirs
    -- metafiles <- mapM fromJSONerrio files

    let menu1 = convert2index (yentry, dirs, fils)
    putIOwords ["convertIndexEntries", "menu2", showT menu1]
    -- let y2 = putAtKey2 "menu" menu1 y
    let y2 = mergeLeftPref [toJSON menu1, toJSON y]
    y4 :: MetaPage <- fromJSONerrio y2   
    return y4

-- | convert the metarecs and put some divider between
-- TODO  - avoid dividers if list empty
convert2index ::
  (IndexEntry, [IndexEntry], [IndexEntry]) ->
  MenuEntry
convert2index (this, content, subix) =
  MenuEntry
    { menu2 =
        [a]
          ++ ( if null c
                 then zero
                 else zero {title2 = "--- subdir ---"} : c
             )
          ++ ( if null b
                 then zero
                 else zero {title2 = "--- content ---"} : b
             )
    }
  where
    a = getOneIndexEntryPure this
    b = map getOneIndexEntryPure content -- braucht wohl filter
    c = map getOneIndexEntryPure subix

data Index4html = Index4html
  { -- fn :: Path Abs File   -- ^ naked filename -- not shown
    text2      :: Text, -- the filename - not shown? ?

    -- | the url relative to dough dir
    link2      :: Text,
    -- | the title as shown
    title2     :: Text,
    abstract2  :: Text,
    author2    :: Text,
    date2      :: Text, -- UTCTime -- read the time early one to find errors
    publish2   :: Text,
    indexPage2 :: Bool -- mark for index entries
  }
  deriving (Generic, Eq, Ord, Show, Read)

instance Zeros Index4html where
  zero = Index4html zero zero zero zero zero zero zero False

instance FromJSON Index4html

-- parseJSON = genericParseJSON h4Options
instance ToJSON Index4html

-- toJSON = genericToJSON h4Options
-- h4Options =
--     defaultOptions
--         {fieldLabelModifier =   drop 2 }

getOneIndexEntryPure :: IndexEntry -> Index4html

-- | the pure code to compute an IndexEntry
-- Text should be "/Blog/postTufteStyled.html"
getOneIndexEntryPure metaRec =
  Index4html
    { text2 = s2t . takeBaseName' . toFilePath  . fn $ metaRec,
      link2 =  s2t . toFilePath $ 
          setExtension extHTML . removeExtension
            . link
            $ metaRec,
      abstract2 = abstract metaRec,
      title2 =
        if isZero (title metaRec :: Text)
          then s2t . takeBaseName' . toFilePath  .  fn $ metaRec
          else title metaRec,
      author2 = author metaRec,
      date2 = showT $ date metaRec,
      publish2 = shownice $ publish metaRec,
      indexPage2 = indexPage metaRec
    }

--       ------  S U P P O R T

newtype MenuEntry = MenuEntry {menu2 :: [Index4html]}
  -- menu2 is referenced in the template
  deriving (Generic, Eq, Ord, Show)

-- instance NiceStrings MenuEntry where
--     shownice = showNice

instance Zeros MenuEntry where
  zero = MenuEntry zero

instance FromJSON MenuEntry

-- parseJSON = genericParseJSON h4Options

instance ToJSON MenuEntry

--  toJSON = genericToJSON h4Options
