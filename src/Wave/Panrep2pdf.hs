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

import Uniform.PandocImports
import Uniform.Latex


-- ------------------------------------ panrep2texsnip
-- implements the bake
-- TODO simplify, just an op on second part
-- is this just a single language snip
-- and a panrep can produce multiple? 
-- how to deal with shake? 
panrep2texsnip :: NoticeLevel -> Panrep -> ErrIO TexSnip
panrep2texsnip debug (Panrep y p) = do
    when (inform debug) $ putIOwords ["\n panrep2texsnip start"]
    res1 <- writeTexSnip2 p
    let res = (TexSnip y res1)
    when (inform debug) $ putIOwords ["\n panrep2texsnip done"]
    return res

-- ------------------------------------ texsnip2tex
-- implements the bake
-- TODO simplify, just an op on second part
-- is this just a single language snip
-- and a panrep can produce multiple? 
-- how to deal with shake? 
texsnip2tex :: NoticeLevel -> TexSnip -> ErrIO Latex
texsnip2tex debug p = do
    when (inform debug) $ putIOwords ["\n texsnip2tex start"]
    let snips2 =  [p]
    let res2 = Latex $ tex2latex zero (map unTexSnip snips2)
    when (inform debug) $ putIOwords ["\n texsnip2tex done"]
    return res2

-- ------------------------------------ tex2pdf
-- implements the bake
-- TODO simplify, operations are with files (not texts)
-- refdir must be set to current 
tex2pdf :: NoticeLevel -> Path Abs File ->  Path Abs File ->   ErrIO ()
tex2pdf debug fn fnres  =  do
    when (inform debug) $ putIOwords ["\n tex2pdf start"]
    let refDir =
            makeAbsDir . getParentDir . toFilePath $ fn :: Path Abs Dir
    writePDF2 debug fn fnres refDir
    when (inform debug) $ putIOwords ["\n tex2pdf done"]
    return ()
