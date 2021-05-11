--------------------------------------------------------------------------
--
-- Module      :  Uniform.ProcessPDF
---------------------------------------------------------------------------
-- {-# LANGUAGE BangPatterns                   #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports 
            -fno-warn-unused-imports 
            -fno-warn-unused-matches #-}

module Uniform2.ProcessPDF (
    module Uniform2.ProcessPDF,
    --   , Pandoc(..)
    --   , module Uniform.Error   -- or at least ErrIO
    --   , write8
    --   , TypedFile5
    --   , TypedFiles5
    --   , TypedFiles7
    --   , read8
    --   , module Uniform.Json
) where

import Uniform2.Filetypes4sites

-- import Uniform.Pandoc  -- cycle?

-- import           Uniform.FileIO

-- import Uniform.Json

-- import qualified System.Exit as Sys
-- import qualified System.Process as Sys
import Uniform.Json
import Uniform.Latex
import Uniform.PandocImports
-- import Uniform2.Docrep
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


