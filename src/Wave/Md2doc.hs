---------------------------------------------------------------------
--
-- Module      :  Wave.Md2doc
-- the conversion of markdown to docrep
------------------------------------------------------------------
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
{-# OPTIONS_GHC -Wall #-}

module Wave.Md2doc (
    module Wave.Md2doc,
    MarkdownText (..),
) where

import UniformBase

import Foundational.Foundation
import Foundational.MetaPage
import Uniform.Json

import Uniform.Pandoc
import Uniform.Shake (makeRelativeP)

import Foundational.Filetypes4sites

readMarkdown2docrep :: NoticeLevel -> Path Abs Dir -> Path Abs Dir -> Path Abs File -> MarkdownText -> ErrIO Docrep

{- | read a md file into a DocrepJSON
 reads the markdown file with pandoc and extracts the yaml metadaat
 the metadata are then converted to metaPage
 -- duplication possible for data in the pandoc metada (no used)
 TODO may use json record parse, which I have already done 
-}
readMarkdown2docrep debug doughP bakedP filename md = do
    pd <- readMarkdown2 md
    let meta2 = flattenMeta . getMeta $ pd
    let relfn = makeRelativeP doughP filename
    let meta4 =
            MetaPage
                { dyFn = toFilePath filename
                , dyLink = toFilePath relfn
                , dyLang = DLenglish -- getAtKey meta2 "language"
                , dyTitle = fromMaybe "FILL TITLE" $ getAtKey meta2 "title"
                , dyAbstract = fromMaybe "FILL ABSTRCT" $ getAtKey meta2 "abstract"
                , dyAuthor = fromMaybe "FILL AUTHOR" $ getAtKey meta2 "author"
                , dyDate = getAtKey meta2 "date"
                , dyKeywords = fromMaybe "" $ getAtKey meta2 "keywords"
                , dyStyle = getAtKey meta2 "style"
                , dyNoCite = getAtKey meta2 "nocite"
                , dyReferences = gak meta2 "references"
                , dyBibliography = zero
                , dyPublish = getAtKey meta2 "publish"
                , -- TODO use pbulicationState
                  dyIndexPage = fromMaybe False $ getAtKey meta2 "indexPage"
                , dyIndexEntry = zero
                }

    return (Docrep meta4 pd)

md2docrep :: NoticeLevel -> SiteLayout -> Path Abs File -> MarkdownText -> ErrIO Docrep

{- | process one md to a docrep
 for bakeOneMD2docrep and report_metaRec
-}
md2docrep debug layout2 inputFn md1 = do
    let doughP = doughDir layout2 -- the regular dough
        bakedP = bakedDir layout2

    dr1 <- readMarkdown2docrep debug doughP bakedP inputFn md1
    -- with a flattened version of json from Pandoc
    -- what does it contain?

    -- check
    -- the fields for the index are prepared
    -- merge the yaml metadata with default to have the
    -- necessary values set

    when (inform debug) $ putIOwords ["md2docrep", "dr1", showT dr1]

    -- uses the refs listed in the file and discovred by pandoc,
    -- as well as nocite
    -- therefore must use json
    dr3 <- addRefs debug dr1
    
    return dr3


--------------------------------
addRefs :: NoticeLevel -> Docrep -> ErrIO Docrep
{- ^ add the references to the pandoc block
 the biblio is in the yam (otherwise nothing is done)
 ths cls file must be in the yam
-}

-- example: 
-- processCites :: Style -> [Reference] -> Pandoc -> Pandoc

-- Process a Pandoc document by adding citations formatted according to a CSL style. Add a bibliography (if one is called for) at the end of the document.
-- http://hackage.haskell.org/package/citeproc-hs-0.3.10/docs/Text-CSL.html
--   m <- readBiblioFile "mybibdb.bib"
--   s <- readCSLFile "apa-x.csl"
--   let result = citeproc procOpts s m $ [cites]
--   putStrLn . unlines . map (renderPlainStrict) . citations $ result

addRefs debug dr1 = do
    -- the biblio entry is the signal that refs need to be processed
    -- only refs do not work
    when (inform debug) $ putIOwords ["addRefs", showT dr1, "\n"]
    let biblio1 = dyBibliography . meta1 $ dr1
    maybe (return dr1) (addRefs2 debug dr1) biblio1

addRefs2 ::
    NoticeLevel ->
    Docrep ->
    Text ->
    ErrIO Docrep
addRefs2 debug dr1@(Docrep y1 p1) biblio1 = do
    --   let debugx = False
    when (inform debug) $ putIOwords ["addRefs2-1", showT dr1, "\n"]
    let style1 = dyStyle y1

    when (inform debug) $
        putIOwords
            [ "addRefs2-2"
            , "\n biblio"
            , showT biblio1 
            ]

    let loc1 = Just "en" -- TODO depends on language to be used for
    -- for the conventions in the lit list
    -- must be 2 char (all other seems to be difficult with pandoc-citeproc)
    -- change to new citeproc TODO later
    let bibliofp =
            t2s biblio1 :: FilePath
    let stylefp =
            t2s . fromJustNote "style1 in addRefs2 wer23" $ style1 :: FilePath
    --  Raised the exception when style empty
    when (inform debug) $ putIOwords ["addRefs2-3-1", "done"]

    p2 <- readBiblioRefs (inform debug) bibliofp loc1 stylefp (dyReferences y1) p1

    when (inform debug) $ putIOwords ["addRefs2-4", "p2\n", showT p2]

    return (Docrep y1 p2)
