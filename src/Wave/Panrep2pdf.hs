---------------------------------------------------------------------
--
-- Module      :  Uniform.Panrep2pdf
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
module Wave.Panrep2pdf (
    module Wave.Panrep2pdf,
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

import Uniform.PandocImports


-- ------------------------------------ panrep2texsnip
-- panrep2html :: Panrep -> ErrIO HTMLout
-- implements the bake
-- TODO simplify, just an op on second part
panrep2texsnip :: NoticeLevel -> Panrep -> ErrIO TexSnip
panrep2texsnip debug (Panrep y p) = do
    when (informall debug) $ putIOwords ["\n panrep2texsnip start"]
    res1 <- writeTexSnip2 p
    let res = (TexSnip y res1)
    when (informall debug) $ putIOwords ["\n panrep2texsnip done"]
    return res

