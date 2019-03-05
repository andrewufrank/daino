
------------------------------------------------------------------------------
--
-- Module      :   dealing with file reading and writing
--              including the where things are (below site)

-- file type: md for markdown, html for templates, html for output

-- text types MD, Template, HTMLout

-- the site dir (and other similar) could be read in as yaml
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lib.YamlBlocks where

import Uniform.Zero hiding (Meta, at)
import Uniform.Filenames hiding (Meta, at)
import Uniform.FileStrings hiding (Meta, at)
import Uniform.TypedFile
import Data.Aeson (Value, ToJSON (..), Value (..), decode)
import Data.Aeson.Encode.Pretty (encodePretty)
--import  Path.IO (ensureDir)
import Text.Pandoc as Pandoc
import Text.Pandoc.Highlighting (tango)
import Text.Pandoc.Shared (stringify)
import Text.CSL.Pandoc (processCites')
import Lib.FileMgt -- (MarkdownText(..), unMT, HTMLout(..), unHTMLout
--            , unDocValue, DocValue (..) )
import Control.Lens ((^?), (?~), (&), at)
import Data.Aeson
import Data.Aeson.Lens
import  Data.Yaml.Union


getMeta :: Pandoc -> Meta
getMeta (Pandoc m _) = m

putMeta :: Meta -> Pandoc -> Pandoc
putMeta m1 (Pandoc _ p0) = Pandoc m1 p0

class AtKey vk v where
    getMaybeStringAtKey :: vk -> Text -> Maybe v
    putStringAtKey :: Text -> v -> vk -> vk

instance AtKey Value Text where
    getMaybeStringAtKey meta2 k2 =   meta2 ^? key k2 . _String

    putStringAtKey  k2 txt meta2 = meta2 & _Object . at k2 ?~ String  txt
--        (unHTMLout text2)

--instance AsValue Meta
--instance AsPrimitive Meta
--instance AsNumber Meta


--instance AtKey Meta Text where
--    getMaybeStringAtKey meta2 k2 =   meta2 ^? key k2 . _String
--
--    putStringAtKey meta2 k2 txt = meta2 & _Object . at k2 ?~ String  txt

instance AtKey DocValue  Text where
    getMaybeStringAtKey meta2 k2 = getMaybeStringAtKey (unDocValue meta2) k2

    putStringAtKey  k2 txt meta2= DocValue $ (unDocValue meta2) & _Object . at k2 ?~ String txt

instance AtKey DocValue Bool  where
    getMaybeStringAtKey meta2 k2 =  (unDocValue meta2) ^? key k2 . _Bool

    putStringAtKey  k2 b meta2 = DocValue $  (unDocValue meta2) & _Object . at k2 ?~ Bool b




-- | Flatten a Pandoc 'Meta' into a well-structured JSON object, rendering Pandoc
-- text objects into plain strings along the way.
flattenMeta :: Meta -> Value
flattenMeta (Meta meta) = toJSON $ fmap go meta
 where
  go :: MetaValue -> Value
  go (MetaMap     m) = toJSON $ fmap go m
  go (MetaList    m) = toJSONList $ fmap go m
  go (MetaBool    m) = toJSON m
  go (MetaString  m) = toJSON m
  go (MetaInlines m) = toJSON $ stringify m
  go (MetaBlocks  m) = toJSON $ stringify m

