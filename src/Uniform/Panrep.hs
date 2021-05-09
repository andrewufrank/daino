---------------------------------------------------------------------------
--
-- Module      :  Uniform.Panrep
-----------------------------------------------------------------------------
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
module Uniform.Panrep (
    module Uniform.Panrep,
) where

import Data.Default
import GHC.Generics (Generic)
import Lib.Foundation
import Lib.IndexMake
import Lib.Indexing (addIndex2yam)
import Lib.MetaPage
import Lib.Templating
import Text.CSL as Pars (Reference, readBiblioFile, readCSLFile)
import Text.CSL.Pandoc as Bib (processCites)
import qualified Text.Pandoc as Pandoc
import Uniform.Filetypes4sites
import Uniform.HTMLout (
    HTMLout (HTMLout),
    html5Options,
    htmloutFileType,
    writeHtml5String,
 )
import Uniform.Json
import Uniform.Pandoc
import Uniform.PandocImports
import UniformBase

------------------------------------------------docrep -> panrep

{- ^ transform a docrep to a panrep (which is the pandoc rep)
 does process the references
 and will do index, but this goes to ssg
-}

docrep2panrep debug layout (Docrep y1 p1) = do
    let bakedP = bakedDir layout
    let pr =
            Panrep
                { panyam = fromJustNote "docRepJSON2docrep not a value" . fromJSONValue $ y1
                , panpan = p1
                }
    p2 <- addIndex2yam bakedP debug pr
    return p2

-- do
-- --   (DocrepJSON y2 p2) <- addRefs False dr1 -- was already done in  bakeOneMD2docrep
-- return
--  $ Panrep y1 p1

-- ------------------------------------ panrep2html
-- panrep2html :: Panrep -> ErrIO HTMLout
-- implements the bake
panrep2html debug layout dr1 = do
    let templateP = templatesDir layout
    dr4 <- convertIndexEntries dr1 -- move to
    p :: HTMLout <- putValinMaster False dr4 templateP
    return p

-- -- where does this belong?

-- panrep22html :: Panrep -> ErrIO HTMLout
-- -- ^ transform a docrep to a html file
-- -- needs teh processing of the references with citeproc
-- panrep2html pr1@(Panrep y1 p1) = do
--   -- dr2 <- addRefs pr1
--   h1 <- unPandocM $ writeHtml5String html5Options p1
--   return . HTMLout $ h1
