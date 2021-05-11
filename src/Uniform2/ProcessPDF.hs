--------------------------------------------------------------------------
--
-- Module      :  Uniform.ProcessPDF
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DoAndIfThenElse #-}
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

{- | convert the tex collection of snips to a latex file
 process it to a pdf
-}
module Uniform2.ProcessPDF (
    module Uniform2.ProcessPDF,
) where

import Uniform2.Filetypes4sites
import Uniform.Json
import Uniform.Latex
import Uniform.PandocImports
import UniformBase

panrep2texsnip :: Panrep -> ErrIO TexSnip
panrep2texsnip (Panrep y p) = do
    res1 <- writeTexSnip2 p
    return (TexSnip y res1)

tex2latex2 :: LatexParam -> [TexSnip] -> Latex
tex2latex2 latpar snips =
    Latex $ tex2latex latpar (map unTexSnip snips)

writePDF1 debug fn fnres refDir = writePDF2 debug fn fnres refDir

-- fn must have extension tex
-- result extension?

--     \bibliographystyle{plainnat}

-- %achtung keine blanks in liste!
-- \bibliography{/home/frank/Workspace8/ssg/docs/site/baked/resources/BibTexLatex.bib}

-- writeTexSnip2text ::   Pandoc -> ErrIO Text
-- -- write a latex file from a pandoc doc
-- seems not to give a full tex and thus not processing
-- writeTexSnip2text  pandocRes = do
--     p <- unPandocM $ writeLaTeX latexOptions pandocRes
--     return  p
---------- write PDF with Lualatex
-- the process uses files - is this a preformance issue?
