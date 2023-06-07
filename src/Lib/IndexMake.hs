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
import Uniform.Json ( FromJSON, ToJSON, ErrIO ) 
import Uniform.Latex 
import           UniformBase
import Data.List (sortOn)
-- import Wave.Md2doc (includeBakeTest3docrep)

convertIndexEntries :: NoticeLevel ->  [Text] -> Text -> IndexEntry -> ErrIO MenuEntry
-- ^ take the index entries and convert their
-- form and push them back into the json
-- converts to values for printing if indexpage else null
-- date today is passed to feed in pages 
-- the authors which should be oppressed are passed 
-- is pure except for today! TODO today is not used 
-- use the date from the siteHeader? TODO
convertIndexEntries debug hpAuthor indexSortField  ixe1 =
  do
    when (inform debug) $ putIOwords ["convertIndexEntries", "start ixe1", showT ixe1
        , "\n\thpAuthor", showT hpAuthor]
    let fn = makeAbsFile $ ixfn ixe1
    when (inform debug) $ putIOwords ["convertIndexEntries", "fn", showT fn]
    menu4 <- if isIndexPage fn
        then do
            let fils = fileEntries ixe1  
            let dirs = dirEntries ixe1  

            let menu1 = convert2index hpAuthor (indexSortField ) (ixe1, fils, dirs)
            -- let menu3 = menu1{today3 = "2021-01-01"}  
            -- to avoid the changes in testing leading to failures
            today1 :: UTCTime <- getCurrentTimeUTC
            let menu3 = menu1 -- {today3 = showT today1}
            when (inform debug) $ putIOwords ["convertIndexEntries", "menu3", showT menu3]
            return menu3
        else return zero
    return menu4

-- | convert the indexEntry1s and put some divider between
-- TODO  - avoid dividers if list empty
convert2index :: [Text] -> Text -> 
  (IndexEntry, [IndexEntry], [IndexEntry]) ->
  MenuEntry
convert2index hpAuthor indexSortField (this, fils, dirs) =
    MenuEntry
        { menu2subdir = sortField . (getIndexEntryPure hpAuthor) $ dirs
        , menu2files  = sortField . (getIndexEntryPure hpAuthor) $ fils
        -- . indexFilter - if needed then use includeBakeTest3docrep from md2doc 
        -- , today3 = zero -- is set afterwards
        }
 where   -- done in collection now?
    sortField = case (indexSortField) of 
        "filename" -> sortOn text2
        "date"      -> sortOn date2
        "reversedate" -> reverse . sortOn  date2
        _ -> sortOn text2  -- what is best default? id?

-- indexFilter :: [IndexEntry] -> [IndexEntry]
-- indexFilter ixs = ixs -- filter ((Just "true" ==) . publish ) ixs
--     -- does not work because publish is not set
--     -- todo remove 


-- | the lines for the index 
-- TODO make a variant for the breaing marks
data Index4html = Index4html
  { -- fn :: Path Abs File   -- ^ naked filename -- not shown
    text2      :: Text, -- the filename with no extension as title 

    -- | the url relative web root
    link2      :: Text,  -- ^ link to html 
    pdf2        :: Text, -- ^ link to pdf 
    -- | the title as shown
    title2     :: Text,
    abstract2  :: Text,
    author2    :: Text,
    date2      :: Text -- UTCTime -- read the time early one to find errors
    -- publish2   :: Text  -- not yet used 
                -- add language ?
    -- indexPage2 :: Bool -- mark for index entries
    }
  deriving (Generic, Eq, Ord, Show, Read)

instance Zeros Index4html where
  zero = Index4html zero zero zero zero zero zero zero 
instance FromJSON Index4html

-- parseJSON = genericParseJSON h4Options
instance ToJSON Index4html


getIndexEntryPure :: [Text]-> [IndexEntry] -> [Index4html]
-- pass the author names which should be oppressed in indices
getIndexEntryPure hpAuthor ixe2 = map (getOneIndexEntryPure hpAuthor)  ixe2
                    -- mapMaybe (\i -> if (Just "true" == publish i)
                    --               then Just $ getOneIndexEntryPure i
                    --               else error "xsdwer" ) ixe2

getOneIndexEntryPure :: [Text] -> IndexEntry -> Index4html

-- | the pure code to compute an IndexEntry
-- Text should be "/Blog/postTufteStyled.html"
getOneIndexEntryPure hpAuthor indexEntry1 =
  Index4html
    { text2 = s2t . takeBaseName'   . ixfn $ indexEntry1
    , link2 =  convertLink2html indexEntry1
    , pdf2 =  convertLink2pdf  indexEntry1 
    , abstract2 = abstract indexEntry1
    , title2 =
        if isZero (title indexEntry1 :: Text)
          then s2t . takeBaseName'   .  ixfn $ indexEntry1
          else title indexEntry1
    , author2 = blankAuthorName hpAuthor (author indexEntry1)
    , date2 = date indexEntry1
    -- , publish2 = shownice $ publish indexEntry1
    --   indexPage2 = indexPage indexEntry1
    }

blankAuthorName :: [Text] -> Text -> Text 
-- suppress/oppress author name, if the author name is the same as one in the first arg (AUF, Andrew U..) then set it to empty else copy 
-- idea is to avoid to have each page say the obvious "author XX"
blankAuthorName names current = 
    if current `elem` names 
        then zero 
        else current 

-- hpAuthor :: [Text]
-- the list of authornames which are blanked
-- should be the author of the blog
-- hpAuthor = ["AUF", "Andrew U. Frank"]

--       ------  S U P P O R T
-- the MenuEntry is one entry in the menu list 
data MenuEntry = MenuEntry {menu2subdir :: [Index4html]
                            , menu2files :: [Index4html]
    -- , today3 :: Text 
                            }
  -- menu2 is referenced in the template
  deriving (Generic, Eq, Ord, Show, Read)

-- instance NiceStrings MenuEntry where
--     shownice = showNice

instance Zeros MenuEntry where
  zero = MenuEntry zero zero 

instance FromJSON MenuEntry

-- parseJSON = genericParseJSON h4Options

instance ToJSON MenuEntry

--  toJSON = genericToJSON h4Options
