{-# LANGUAGE DeriveGeneric         #-}
---------------------------------------------------------------------
--
-- Module      :   
-- | create an index for a directory
--  in two steps: 
--  IndexCollect: collect all the date 
--  with call to addIndex2yam
--  and
--  indexMake: convert collected data for printing (convertIndexEntries)
--  . 
--  the data is stored in a file separately and managed by Shake
--  operates on metapage (or less? )
-- this could all be pure code?
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
import Data.List (sortOn)
-- import Data.Ord (comparing)

convertIndexEntries :: NoticeLevel ->  Text -> IndexEntry -> ErrIO MenuEntry
-- ^ take the index entries and convert their
-- form and push them back into the json
-- converts to values for printing if indexpage else null
-- date today is passed to feed in pages 
-- is pure except for today!
convertIndexEntries debug indexSortField  ixe1 =
  do
    when (inform debug) $ putIOwords ["convertIndexEntries", "start ixe1", showT ixe1]
    let fn = makeAbsFile $ ixfn ixe1
    when (inform debug) $ putIOwords ["convertIndexEntries", "fn", showT fn]
    -- today1 :: UTCTime <- getCurrentTimeUTC
    -- to avoid the changes in testing leading to failures
    -- let today1 = "2021-01-01"::UTCTime  -- for testing 
    menu4 <- if isIndexPage fn
        then do
            let fils = fileEntries ixe1 -- fileEntries dyFileEntries MetaPage
            let dirs = dirEntries ixe1 -- dyDirEntries y

            let menu1 = convert2index indexSortField (ixe1, fils, dirs)
            let menu3 = menu1{today2 = "2021-01-01"} --showT today1}
            when (inform debug) $ putIOwords ["convertIndexEntries", "menu1", showT menu3]
            return menu3
        else return zero
    return menu4

-- | convert the indexEntry1s and put some divider between
-- TODO  - avoid dividers if list empty
convert2index :: Text -> 
  (IndexEntry, [IndexEntry], [IndexEntry]) ->
  MenuEntry
convert2index indexSortField (this, fils, dirs) =
    MenuEntry
        { menu2subdir = sortOn sortField . getIndexEntryPure $ dirs
        , menu2files  = sortOn sortField . getIndexEntryPure . indexFilter $ fils
        , today2 = zero -- is set above
        }
 where 
    sortField = case (indexSortField) of 
        "filename" -> text2
        "date"      -> date2
        _ -> date2   -- as a default
        -- how to deal with reverse sorts??

indexFilter :: [IndexEntry] -> [IndexEntry]
indexFilter ixs = ixs -- filter ((Just "true" ==) . publish ) ixs
    -- does not work because publish is not set


-- | the lines for the index 
-- TODO make a variant for the breaing marks
data Index4html = Index4html
  { -- fn :: Path Abs File   -- ^ naked filename -- not shown
    text2      :: Text, -- the filename with no extension as title 

    -- | the url relative web root
    link2      :: Text,
    -- | the title as shown
    title2     :: Text,
    abstract2  :: Text,
    author2    :: Text,
    date2      :: Text, -- UTCTime -- read the time early one to find errors
    publish2   :: Text  -- not yet used 
                -- add language ?
    -- indexPage2 :: Bool -- mark for index entries
    }
  deriving (Generic, Eq, Ord, Show, Read)

instance Zeros Index4html where
  zero = Index4html zero zero zero zero zero zero zero

instance FromJSON Index4html

-- parseJSON = genericParseJSON h4Options
instance ToJSON Index4html


getIndexEntryPure :: [IndexEntry] -> [Index4html]
getIndexEntryPure ixe2 = map getOneIndexEntryPure  ixe2
                    -- mapMaybe (\i -> if (Just "true" == publish i)
                    --               then Just $ getOneIndexEntryPure i
                    --               else error "xsdwer" ) ixe2

getOneIndexEntryPure :: IndexEntry -> Index4html

-- | the pure code to compute an IndexEntry
-- Text should be "/Blog/postTufteStyled.html"
getOneIndexEntryPure indexEntry1 =
  Index4html
    { text2 = s2t . takeBaseName'   . ixfn $ indexEntry1,
      link2 =  s2t . -- s2t . toFilePath $ 
          setExtension "html" . removeExtension
          -- TODO use extHTML
            . link
            $ indexEntry1,
      abstract2 = abstract indexEntry1,
      title2 =
        if isZero (title indexEntry1 :: Text)
          then s2t . takeBaseName'   .  ixfn $ indexEntry1
          else title indexEntry1,
      author2 = author indexEntry1,
      date2 = date indexEntry1,
      publish2 = shownice $ publish indexEntry1
    --   indexPage2 = indexPage indexEntry1
    }

--       ------  S U P P O R T

data MenuEntry = MenuEntry {menu2subdir :: [Index4html]
                            , menu2files :: [Index4html]
                                , today2 :: Text }
  -- menu2 is referenced in the template
  deriving (Generic, Eq, Ord, Show, Read)

-- instance NiceStrings MenuEntry where
--     shownice = showNice

instance Zeros MenuEntry where
  zero = MenuEntry zero zero zero

instance FromJSON MenuEntry

-- parseJSON = genericParseJSON h4Options

instance ToJSON MenuEntry

--  toJSON = genericToJSON h4Options
