{-# LANGUAGE DeriveGeneric #-}
------------------------------------------------------
-- Module DocRep  (which is pandoc and metarec
-- originally copied from Slick  because it concentrates all funny pandoc stuff here (including the
-- writing of the json
----------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -fno-warn-dodgy-exports #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans
 -fno-warn-missing-signatures
 -fno-warn-missing-methods 
-fno-warn-duplicate-exports
-fno-warn-unused-imports 
-fno-warn-unused-matches #-}

module Lib.Pandoc
  ( module Lib.Pandoc,
    getMeta,
    getAtKey,
    Pandoc (..),
    flattenMeta,
    readMarkdown2,
    HTMLout (..),
    htmloutFileType,
    -- , MenuEntry
  )
where

-- hiding (Meta(..))

import Lib.CmdLineArgs (PubFlags (..))
import Lib.Foundation (SiteLayout (..))
import Uniform.BibTex ()
import Uniform.Convenience.DataVarious
import Uniform.HTMLout (HTMLout (..), htmloutFileType)
import Uniform.Markdown (readMarkdown2)
import Uniform.Pandoc
  
import UniformBase
import Uniform.Json 

   

-------------------
docRepNeeds2 :: Path Abs File -> ErrIO [FilePath]
-- get the needs that are visible in the file
-- fix the path? TODO
docRepNeeds2 fnx = do
  dr1 <- read8 fnx docRepFileType
  let n1 = docRepNeeds dr1
  return n1

docRepNeeds :: DocRep -> [FilePath]
-- ^ collect the needs (bib, images, css?)
docRepNeeds (DocRep y1 p1) = map t2s . catMaybes $ [imgs, bibs]
  where
    imgs = getAtKey y1 "image" :: Maybe Text
    bibs = getAtKey y1 "bibliography" :: Maybe Text

-- TODO should list be images?

data BottomLines = BottomLines
  { ssgversion :: Text,
    -- | the data when converted(baked)
    today :: Text,
    filename :: Text
  }
  deriving (Generic, Read, Show, Eq, Ord)

instance ToJSON BottomLines
