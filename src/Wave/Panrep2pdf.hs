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
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
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
import Uniform.Latex ( tex2latex, writePDF2, LatexParam (..))  -- used for 


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

-- ata LatexParam = LatexParam
-- { latTitle ::  Text  
--     , latAuthor :: Text 
--     , latAbstract ::  Text
--     , latBibliographyP :: Text  -- the bibliio file 
--             -- problem with multiple files? 
--     , latStyle :: Text
--             -- is not used 
--     , latBook :: Bool  -- is this a long text for a book/booklet
--     , latContent :: [Text] -- ^ a list of the .md files which are collected into a multi-md pdf
--     -- }
--     deriving (Eq, Ord, Read, Show, Generic)

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
            , latStyle    = "authoryear"
                --  maybe "authoryear" id $ dyStyleBiber (snipyam p)
            , latBook = False  -- will be used for books
            , latContent = dyContentFiles (snipyam p)
        }
 
    let res2 = Latex $ tex2latex latexparam (  unTexSnip p)
    -- TODO add here the LatexParam (title, abstract, content, biblio, style)
    -- extracted from first param of texsnip
    -- which texsnip if there are multiple?
        -- allow multiple biblios?

    -- tex file must be full, ordinary latex content

    when (inform debug) $ putIOwords ["\n texsnip2tex done"]
    return res2

-- ------------------------------------ tex2pdf
-- implements the bake to convert tex to pdf 
-- input tex must be an ordinary latx file 
-- operations are with files (not texts)

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
