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

{- |  ready for processing to HTML or to TexSnip -> Tex -> Pdf
-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Wave.Panrep2pdf (
    module Wave.Panrep2pdf,
) where

import Foundational.Filetypes4sites
import Foundational.MetaPage
import GHC.Generics (Generic)
import Uniform.Pandoc ( writeTexSnip2 )
import UniformBase

import Uniform.Latex ( tex2latex, writePDF2, LatexParam (..))  


-- ------------------------------------ panrep2texsnip

panrep2texsnip :: NoticeLevel -> Panrep -> ErrIO TexSnip
panrep2texsnip debug (Panrep y p) = do
    when (inform debug) $ putIOwords ["\n panrep2texsnip start"]
    res1 <- writeTexSnip2 p
    let res = (TexSnip y res1)
    when (inform debug) $ putIOwords ["\n panrep2texsnip done"]
    return res


text2absFile :: Path Abs Dir -> Text -> Path Abs File 
text2absFile doughP t = doughP </> makeRelFile (t2s t)

texsnip2tex :: NoticeLevel ->  Path Abs Dir -> TexSnip ->  ErrIO Latex
-- the (lead) snip which comes from the md which gives the name to the resulting tex and pdf 
-- and ist metadata are included (taken from the snip)
-- it may include other filenames, the snips of these
-- are then included in the pdf built. 
texsnip2tex  debug doughP p = do
    when (inform debug) $ putIOwords ["\n texsnip2tex start"]
    -- let snips2 =  [p]
    let latexparam = LatexParam 
            { latTitle = dyTitle (snipyam p) 
            , latAuthor = dyAuthor (snipyam p)
            , latAbstract = dyAbstract (snipyam p)
            , latBibliographyP = maybe "" (s2t . toFilePath . text2absFile doughP)
                -- fmap (text2absfile doughP) 
                (dyBibliography $ snipyam p)
            -- make this an abs file name 
            , latReferences = maybe "" (shownice ) $ dyReferences (snipyam p)
            , latStyle    = dyStyleBiber (snipyam  p)
                --  maybe "authoryear" id $ dyStyleBiber (snipyam p)
            , latBook = False  -- will be used for books
            , latContent = dyContentFiles (snipyam p)
        }
 
    let res2 = Latex $ tex2latex latexparam (  unTexSnip p)
   
    -- tex file must be full, ordinary latex content

    when (inform debug) $ putIOwords ["\n texsnip2tex done"]
    return res2

-- ------------------------------------ tex2pdf


-- refdir must be set to the dir where searches for 
-- biblio etc start - seems not correct
-- the refdir is where the intermediate files are put
-- this is fnres - just the doughPath
tex2pdf :: NoticeLevel -> Path Abs File ->  Path Abs File ->  Path Abs Dir ->  ErrIO ()
tex2pdf debug fn fnres doughP  =  do
    when (inform debug) $ putIOwords ["\n tex2pdf start for", showT fn]
    let refDir =
            makeAbsDir . getParentDir . toFilePath $ fn :: Path Abs Dir
    -- refDir must be the place where biblio is place (or searched from - best ) - i.e. the root for dough 
    writePDF2 debug  fn fnres refDir
    when (inform debug) $ putIOwords ["\n tex2pdf done"]
    return ()
