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

-- | SSG uses sequences of transformations between data structures (types)
-- MD -> DocrepJSON -> Panrep -> TexSnip -> Tex -> PDF
--                 Pandrep -> HTML
-- Each result is written as a typed file with a specific extension
module Uniform.Filetypes4sites
  ( module Uniform.Filetypes4sites,
  )
where

import Uniform.Json (FromJSON, ToJSON, Value)
import Uniform.PandocImports
import UniformBase

----------------------------- -------------------------Markdown

extMD :: Extension
extMD = Extension "md"

newtype MarkdownText = MarkdownText Text
  deriving (Show, Read, Eq, Ord)

-- a wrapper around Markdonw text
unMT :: MarkdownText -> Text
unMT (MarkdownText a) = a --needed for other ops

instance Zeros MarkdownText where
  zero = MarkdownText zero

markdownFileType :: TypedFile5 Text MarkdownText
markdownFileType =
  TypedFile5 {tpext5 = extMD} :: TypedFile5 Text MarkdownText

instance TypedFiles7 Text MarkdownText where
  -- handling Markdown and read them into MarkdownText
  wrap7 = MarkdownText
  unwrap7 (MarkdownText a) = a

-- readMarkdown, readMd2meta in Uniform.Markdown

--------------------------------------------typed file DocrepJSON

-- | representation of a document
-- the yam part contains the json formated yaml metadata
-- which is extensible
-- Attention the Pandoc is Pandoc (Meta (Map Text MetaValue) [Block]
-- means that title etc is duplicated in the Meta part.
-- I keep the pandoc structure (Pandoc Meta [Block] - Text.Pandoc.Definition
-- because it is possible to convert the Meta from Pandoc to JSON
-- with flattenMeta (in PandocImports)
-- but I do not see an easy way to convert back

-- data DocrepJSON = DocrepJSON {yam :: Value, blocks :: [Block]} -- a json value
data DocrepJSON = DocrepJSON {yam :: Value, pan :: Pandoc} -- a json value
  deriving (Show, Read, Eq, Generic, Zeros)

-- instance Zeros DocrepJSON where zero = DocrepJSON zero zero

instance FromJSON DocrepJSON
instance ToJSON DocrepJSON

extDocrep :: Extension
extDocrep = Extension "docrep"

-- instance NiceStrings DocrepJSON where
--   shownice = showNice . unDocrep

docrepFileType :: TypedFile5 Text DocrepJSON
docrepFileType =
  TypedFile5 {tpext5 = extDocrep} :: TypedFile5 Text DocrepJSON

instance TypedFiles7 Text DocrepJSON where
  wrap7 = readNote "DocrepJSON wrap7 sfasdwe" . t2s
  unwrap7 = showT

-------------------- fileType Panrep ----------
-- a file containing what pandoc internally works on
-- plus the complete set of the metadata

extPanrep :: Extension
extPanrep = Extension "panrep"

panrepFileType :: TypedFile5 Text Panrep
panrepFileType =
  TypedFile5 {tpext5 = extPanrep} :: TypedFile5 Text Panrep

data Panrep = Panrep {panyam :: Value, panpan :: Pandoc}
  deriving (Eq, Show, Read)

instance Zeros Panrep where zero = Panrep zero zero

instance TypedFiles7 Text Panrep where
  -- handling Pandoc and read them into PandocText
  wrap7 = readNote "wrap7 for pandoc 223d" . t2s
  unwrap7 = showT

--------------------  TexSnip
-- a tex snip is a piece of latex code, but not a full compilable
-- latex which results in a pdf

extTexSnip :: UniformBase.Extension
extTexSnip = Extension "texsnip"

-- | a wrapper around TexSnip
data TexSnip = TexSnip {snipyam :: Value, unTexSnip :: Text}
  deriving (Show, Read, Eq)

-- unTexSnip (TexSnip a) = a   --needed for other ops

instance Zeros TexSnip where
  zero = TexSnip zero zero

texSnipFileType :: TypedFile5 Text TexSnip
texSnipFileType =
  TypedFile5 {tpext5 = extTexSnip} :: TypedFile5 Text TexSnip

instance TypedFiles7 Text TexSnip where
  -- handling TexSnip and read them into TexSnipText
  -- the file on disk is readable for texstudio

  wrap7 = readNote "wrap7 for TexSnip dwe11d" . t2s
  unwrap7 = showT

----------------  Tex
-- this is a full file, not just a snippet

extTex :: Extension
extTex = Extension "tex"

texFileType :: TypedFile5 Text Latex
texFileType = TypedFile5 {tpext5 = extTex} :: TypedFile5 Text Latex

instance TypedFiles7 Text Latex where
  wrap7 = Latex
  unwrap7 = unLatex

newtype Latex = Latex {unLatex :: Text}
  deriving (Eq, Ord, Read, Show)

instance Zeros Latex where
  zero = Latex zero

---------------------------------------------- PDF

extPDF :: Extension
extPDF = Extension "pdf"

pdfFileType :: TypedFile5 Text PDFfile
pdfFileType = TypedFile5 {tpext5 = extPDF} :: TypedFile5 Text PDFfile

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