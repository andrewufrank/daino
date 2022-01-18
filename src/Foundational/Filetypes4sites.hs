{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

---------------------------------------------------------------------
--
-- Module      : all the filetype used for SSG
----------------------------------------------------------------------

{- | SSG uses sequences of transformations between data structures (types)
 MD -> Docrep -> Panrep -> TexSnip -> Tex -> PDF
                 Pandrep -> HTML
 Each result is written as a typed file with a specific extension
-}
module Foundational.Filetypes4sites (
    module Foundational.Filetypes4sites,
) where

import Uniform.Json (FromJSON, ToJSON, Value)
import Uniform.PandocImports ( Pandoc )
import UniformBase
import Foundational.MetaPage ( MetaPage, extPDF )


--------------------------------------------typed file Docrep

{- | representation of a document
 the yam part contains the json formated yaml metadata
 which is extensible
 Attention the Pandoc is Pandoc (Meta (Map Text MetaValue) [Block]
 means that title etc is duplicated in the Meta part.
 I keep the pandoc structure (Pandoc Meta [Block] - Text.Pandoc.Definition
 because it is possible to convert the Meta from Pandoc to JSON
 with flattenMeta (in PandocImports)
 but I do not see an easy way to convert back
 - Where would this be required? 
 - probably for the index construction? 
-}
data Docrep = Docrep {meta1 :: MetaPage, pan1 :: Pandoc} -- a json value
    deriving (Show, Read, Eq, Generic, Zeros)

instance FromJSON Docrep
instance ToJSON Docrep

extDocrep :: Extension
extDocrep = Extension "docrep"

-- instance NiceStrings Docrep where
--   shownice = showNice . unDocrep

docrepFileType :: TypedFile5 Text Docrep
docrepFileType =
    TypedFile5{tpext5 = extDocrep} :: TypedFile5 Text Docrep

instance TypedFiles7 Text Docrep where
    wrap7 = readNote "Docrep wrap7 sfasdwe" . t2s
    unwrap7 = showT

-------------------- fileType Panrep ----------

extPanrep :: Extension
extPanrep = Extension "panrep"

-- | a file containing what pandoc internally works on
-- plus the complete set of the metadata
panrepFileType :: TypedFile5 Text Panrep
panrepFileType =
    TypedFile5{tpext5 = extPanrep} :: TypedFile5 Text Panrep

data Panrep = Panrep {panyam :: MetaPage, panpan :: Pandoc}
    deriving (Eq, Show, Read)

instance Zeros Panrep where zero = Panrep zero zero

instance TypedFiles7 Text Panrep where
    -- handling Pandoc and read them into PandocText
    wrap7 = readNote "wrap7 for pandoc 223d" . t2s
    unwrap7 = showT

--- variant 1 panrep 
extPanrep1 :: Extension
extPanrep1 = Extension "panrep1"

panrep1FileType :: TypedFile5 Text Panrep1
panrep1FileType =
    TypedFile5{tpext5 = extPanrep1} :: TypedFile5 Text Panrep1

newtype Panrep1 = Panrep1 {unPanrep1 :: Panrep}
-- data Panrep = Panrep {panyam :: MetaPage, panpan :: Pandoc}
    deriving (Eq, Show, Read)

-- instance Zeros Panrep where zero = Panrep zero zero

instance TypedFiles7 Text Panrep1 where
    wrap7 = readNote "wrap7 for pandocrep1" . t2s
    unwrap7 = showT
--------------------  TexSnip

extTexSnip :: UniformBase.Extension
extTexSnip = Extension "texsnip"

{- | a wrapper around TexSnip
 snipyam is not used
a tex snip is a piece of latex code, but not a full compilable
latex which results in a pdf
-}
data TexSnip = TexSnip {snipyam :: MetaPage, unTexSnip :: Text}
    deriving (Show, Read, Eq)

-- unTexSnip (TexSnip a) = a   --needed for other ops

instance Zeros TexSnip where
    zero = TexSnip zero zero

texSnipFileType :: TypedFile5 Text TexSnip
texSnipFileType =
    TypedFile5{tpext5 = extTexSnip} :: TypedFile5 Text TexSnip

instance TypedFiles7 Text TexSnip where
    -- handling TexSnip and read them into TexSnipText
    -- the file on disk is readable for texstudio

    wrap7 = readNote "wrap7 for TexSnip dwe11d" . t2s
    unwrap7 = showT

----------------  Tex

extTex :: Extension
extTex = Extension "tex"

texFileType :: TypedFile5 Text Latex
texFileType = TypedFile5{tpext5 = extTex} :: TypedFile5 Text Latex

instance TypedFiles7 Text Latex where
    wrap7 = Latex
    unwrap7 = unLatex

-- | this is a full file, not just a snippet
newtype Latex = Latex {unLatex :: Text}
    deriving (Eq, Ord, Read, Show)

instance Zeros Latex where
    zero = Latex zero

---------------------------------------------- PDF
-- extension in metapage

pdfFileType :: TypedFile5 Text PDFfile
pdfFileType = TypedFile5{tpext5 = extPDF} :: TypedFile5 Text PDFfile

-- | a file in PDF format
newtype PDFfile = PDFfile {unpdffile :: Text}
    deriving (Eq, Ord, Read, Show)

instance Zeros PDFfile where
    zero = PDFfile zero

instance TypedFiles7 Text PDFfile where
    wrap7 = PDFfile
    unwrap7 = unpdffile

-------------------- fileType ---------- CSL
-- extCSL = Extension "csl"
-- cslFileType = TypedFile5 {tpext5 = extCSL} :: TypedFile5 Text Style

-- instance TypedFiles7 Text Style where
--     wrap7 = id
--     unwrap7 = id
--------------------------------- Bib
-- extBib = Extension "bib"
-- bibFileType = TypedFile5 {tpext5 = extBib}

-- instance TypedFiles7 Text