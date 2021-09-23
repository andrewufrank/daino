---------------------------------------------------------------------
--
-- Module      :  Uniform.Doc2html
---------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans
            -fno-warn-missing-signatures
            -fno-warn-missing-methods
            -fno-warn-duplicate-exports
            -fno-warn-unused-imports
            -fno-warn-unused-matches #-}

{- | the representation with indices
 ready for processing to HTML or to TexSnip -> Tex -> Pdf
-}
module Wave.Doc2html (
    module Wave.Doc2html,
) where

import Data.Default
import Foundational.Filetypes4sites
import Foundational.LayoutFlags
import Foundational.MetaPage
import GHC.Generics (Generic)
import Lib.IndexMake
import Lib.IndexCollect
import Lib.Templating
import Uniform.Json
import Uniform.Pandoc
import Uniform2.HTMLout
import UniformBase
import Data.Maybe (fromMaybe)

-- import Text.Pandoc.Definition

------------------------------------------------docrep -> panrep

{- ^ transform a docrep to a panrep (which is the pandoc rep)
 does process the references
 and will do index, but this goes to ssg
-}

docrep2panrep debug layout (Docrep y1 p1) = do
    let bakedP = bakedDir layout
    let doughP = doughDir layout
    let pr =
            Panrep
                { panyam = y1
                , panpan = p1
                }

    if isIndexPage (makeAbsFile . dyFn . panyam $ pr )
        then do
    -- if dyIndexPage . panyam $ pr
            let m1 = panyam pr
            let ix1 =dyIndexEntry  m1
            ix2 <- completeIndex debug doughP bakedP ix1
            -- todo put ix2 into pr
            let m2 = m1{dyIndexEntry = ix2}
            return pr{panyam = m2}
        else
            return pr

-- ------------------------------------ panrep2html
-- panrep2html :: Panrep -> ErrIO HTMLout
-- implements the bake
panrep2html :: NoticeLevel -> Path Abs File -> Settings -> Panrep -> ErrIO HTMLout
panrep2html debug masterfn staticMenu (Panrep m1 p1) = do
        vals <- panrep2vals  debug staticMenu (Panrep m1 p1)
        p :: HTMLout <- panrep2html2 debug masterfn vals
        return p

panrep2vals ::  NoticeLevel -> Settings -> Panrep -> ErrIO [Value]
panrep2vals debug staticMenu (Panrep m1 p1) = do
    let ixe1 = dyIndexEntry m1
    let indexSortField = Data.Maybe.fromMaybe "" (dyIndexSort m1)
    menu4 :: MenuEntry <- convertIndexEntries  debug indexSortField ixe1
    html <- writeHtml5String2 p1
    -- in uniform.Pandoc (dort noch mehr moeglicherweise duplicated)
    let p2 = ContentHtml html
    when (inform debug) $ putIOwords ["panrep2vals", "m1", showPretty m1]
    when (inform debug) $ putIOwords ["panrep2vals", "staticmenu", showPretty staticMenu]
    when (inform debug) $putIOwords ["panrep2vals", "menu4", showPretty menu4]
    when (inform debug) $putIOwords ["panrep2vals", "p2", showPretty p2]
    let vals = [toJSON staticMenu, toJSON m1, toJSON menu4, toJSON p2]
    -- order matters left preference?
    when (inform debug) $putIOwords ["panrep2vals", "vals", showPretty vals]
    return vals

panrep2html2 :: NoticeLevel -- ^ 
  -> Path Abs File -- ^ 
  -> [Value] -- ^ 
  -> ErrIO HTMLout
panrep2html2 debug masterfn vals = do
    p :: HTMLout <- putValinMaster debug vals masterfn
    when (True) $ putIOwords ["\n panrep2html done"]
    return p

newtype ContentHtml = ContentHtml {content :: Text} deriving (Show, Generic)
-- | the record which contains the blog text in html format 
-- mit id,h1, h2,.. span und p tags 
instance ToJSON ContentHtml
