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
-- Module      : all the filetype used for daino
----------------------------------------------------------------------

{- | daino uses sequences of transformations between data structures (types)
 MD -> Docrep -> Panrep -> TexSnip -> Tex -> PDF
                 Pandrep -> HTML
 Each result is written as a typed file with a specific extension
-}
module Foundational.Filetypes4sites (
    module Foundational.Filetypes4sites,
) where

import Uniform.Json (FromJSON, ToJSON, Value)
import UniformBase
import Foundational.SettingsPage ( DainoMetaPlus )

--------------------------------------------typed file Docrep

{- | representation of a document
all the data is in the meta part
-}
type Docrep = DainoMetaPlus 


extDocrep :: Extension
extDocrep = Extension "docrep"



docrepFileType :: TypedFile5 Text Docrep
docrepFileType =
    TypedFile5{tpext5 = extDocrep} :: TypedFile5 Text Docrep

instance TypedFiles7 Text Docrep where
    wrap7 = readNote ("Docrep wrap7 sfasdwe") . t2s
    --    wrap7 a = readNote (show a) . t2s $ a

    unwrap7 = showT

-------------------- fileType Panrep ----------

extPanrep :: Extension
extPanrep = Extension "panrep"

-- | a file containing the metadata
panrepFileType :: TypedFile5 Text Panrep
panrepFileType =
    TypedFile5{tpext5 = extPanrep} :: TypedFile5 Text Panrep



type Panrep = DainoMetaPlus 

-------------------- fileType  TTh TemplateTest for html output----------

extTTH :: Extension
extTTH = Extension "tth"

-- | a file containing the metadata for html output
tthFileType :: TypedFile5 Text TTH
tthFileType =
    TypedFile5{tpext5 = extTTH} :: TypedFile5 Text TTH

instance TypedFiles7 Text Text where
    wrap7 = id -- readNote ("TTH wrap7 sdwwe") . t2s
    --    wrap7 a = readNote (show a) . t2s $ a

    unwrap7 = id

type TTH = Text  
------------------- fileType  TTL TemplateTest ----------

extTTL :: Extension
extTTL = Extension "ttl"

-- | a file containing the metadata
ttlFileType :: TypedFile5 Text TTL
ttlFileType =
    TypedFile5{tpext5 = extTTL} :: TypedFile5 Text TTL

-- instance TypedFiles7 Text Text where
--     wrap7 = id -- readNote ("TTL wrap7 sdwwe") . t2s
--     --    wrap7 a = readNote (show a) . t2s $ a

--     unwrap7 = id

type TTL = Text  
--------------------  TexSnip

extTexSnip :: UniformBase.Extension
extTexSnip = Extension "texsnip"

{- | a wrapper around TexSnip
 
-}
type TexSnip = DainoMetaPlus

texSnipFileType :: TypedFile5 Text TexSnip
texSnipFileType =
    TypedFile5{tpext5 = extTexSnip} :: TypedFile5 Text TexSnip

-- instance TypedFiles7 Text TexSnip where
    -- handling TexSnip and read them into TexSnipText
    -- the file on disk is readable for texstudio

    -- wrap7 = readNote "wrap7 for TexSnip dwe11d" . t2s
    -- unwrap7 = showT

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


extPDF :: Extension
extPDF = Extension "pdf"

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