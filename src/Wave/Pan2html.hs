---------------------------------------------------------------------
--
-- Module      :  Uniform.Pan2html
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
module Wave.Pan2html (
    module Wave.Pan2html,
) where

import Data.Default
import Foundational.Filetypes4sites
import Foundational.Foundation
import Foundational.MetaPage
import GHC.Generics (Generic)
import Lib.IndexMake
import Lib.Indexing
import Lib.Templating
import Uniform.Json
import Uniform.Pandoc
import Uniform2.HTMLout
import UniformBase

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
    --
    if dyIndexPage . panyam $ pr
        then do
            let m1 = panyam pr
            let ix1 = initializeIndex m1
            ix2 <- completeIndex debug doughP bakedP ix1
            -- todo put ix2 into pr
            let m2 = m1{dyIndexEntry = ix2}
            return pr{panyam = m2}
        else return pr

-- ------------------------------------ panrep2html
-- panrep2html :: Panrep -> ErrIO HTMLout
-- implements the bake
panrep2html :: NoticeLevel -> SiteLayout -> Panrep -> ErrIO HTMLout
panrep2html debug layout (Panrep m1 p1) = do
    let ixe1 = dyIndexEntry m1
    menu4 :: MenuEntry <- convertIndexEntries ixe1 -- move to
    thtml <- writeHtml5String2 p1
    let p2 = Content thtml
    let cts = [toJSON m1, toJSON menu4, toJSON p2]

    p :: HTMLout <- putValinMaster debug cts (templatesDir layout)
    when (informNone debug) $ putIOwords ["\n panrep2html done"]
    return p

newtype Content = Content {content :: Text} deriving (Show, Generic)
instance ToJSON Content
