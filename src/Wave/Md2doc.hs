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

import Foundational.LayoutFlags
import Foundational.MetaPage
import Uniform.Json

-- import Lib.IndexCollect
-- import Lib.IndexMake 
import Uniform.Pandoc
import Uniform.Shake (makeRelativeP)

import Foundational.Filetypes4sites


{- | process one md to a docrep
 for bakeOneMD2docrep and report_metaRec
-}
md2docrep :: NoticeLevel
    -> SiteLayout
    -> Path Abs File
    -> MarkdownText
    -> ErrorT Text IO Docrep
md2docrep debug layout2 inputFn md1 = do
    let doughP = doughDir layout2 -- the regular dough
        bakedP = bakedDir layout2

    pd <- readMarkdown2 md1 -- to pandoc 
    -- take metadata and fill metaPage (including IndexEntry)
    let dr1 = pandoc2docrep debug doughP bakedP inputFn pd -- to dr1
    -- with a flattened version of json from Pandoc
    -- convert to metaPage with Index 

    -- check
    -- the fields for the index are prepared
    -- merge the yaml metadata with default to have the
    -- necessary values set

    when (inform debug) $ putIOwords ["md2docrep", "dr1", showT dr1]

    -- uses the refs listed in the file and discovred by pandoc,
    -- as well as nocite
    -- therefore must use json
    dr3 <- addRefs debug doughP dr1  -- to dr3 

    when (inform debug) $ putIOwords ["md2docrep after addRefs", "dr1", showT dr1]

    return dr3 -- same as T.docrep 

pandoc2docrep :: NoticeLevel -> Path Abs Dir -> Path Abs Dir -> Path Abs File -> Pandoc -> Docrep
{- | convert the pandoc text to DocrepJSON
 reads the markdown file with pandoc and extracts the yaml metadaat
 the metadata are then converted to metaPage
 -- duplication possible for data in the pandoc metada (no used)
 TODO may use json record parse, which I have already done
-}
-- pure 
pandoc2docrep debug doughP bakedP filename pd = 
    let meta2 = flattenMeta . getMeta $ pd
        relfn = makeRelativeP doughP filename
        meta4 =
            MetaPage
                { dyFn = toFilePath filename
                , dyLink = toFilePath relfn
                , dyLang = "en_US" -- DLenglish -- getAtKey meta2 "language"
                , dyTitle = fromMaybe "FILL_dyTitle2" $ getAtKey meta2 "title"
                , dyAbstract = fromMaybe "FILL_dyAbstract" $ getAtKey meta2 "abstract"
                , dyAuthor = fromMaybe "FILL_dyAuthor" $ getAtKey meta2 "author"
                , dyDate = getAtKey meta2 "date"
                , dyKeywords = fromMaybe "" $ getAtKey meta2 "keywords"
                , dyStyle = getAtKey meta2 "style"
                , dyStyleBiber = fromMaybe "authoryear" $ getAtKey meta2 "styleBiber"
                , dyNoCite = getAtKey meta2 "nocite"
                , dyReferences = gak meta2 "references"
                , dyContentFiles = maybeToList  . getAtKey meta2 $ "content"
                -- TODO make reading a list
                , dyBibliography = getAtKey meta2 "bibliography"
                , dyPublish = getAtKey meta2 "publish"
                -- , -- TODO use pbulicationState
                --   dyIndexPage = fromMaybe False $ getAtKey meta2 "indexPage"
                , dyIndexSort = getAtKey meta2 "indexSort"
                , dyIndexEntry =   zero
                                            -- else zero
                }
                
        ix1 =  initializeIndex meta4
        meta6 = meta4{dyIndexEntry = ix1}
    in (Docrep meta6 pd)



--------------------------------
addRefs :: NoticeLevel -> Path Abs Dir -> Docrep -> ErrIO Docrep
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

addRefs debug doughP dr1 = do
    -- the biblio entry is the signal that refs need to be processed
    -- only refs do not work
    when (inform debug) $ putIOwords ["addRefs", showT dr1, "\n"]
    let biblio1 = dyBibliography . meta1 $ dr1
    maybe (return dr1) (addRefs2 debug doughP dr1) biblio1

addRefs2 ::
    NoticeLevel ->
    Path Abs Dir ->   -- ^ path to dough (source)
    Docrep ->
    Text ->
    ErrIO Docrep
addRefs2 debug doughP dr1@(Docrep y1 p1) biblio1 = do
    --   let debugx = False
    when (inform debug) $ putIOwords ["addRefs2-1", showT dr1, "\n"]
    let style1 = dyStyle y1

    when (inform debug) $
        putIOwords
            [ "addRefs2-2"
            , "\n\t biblio1" , showT biblio1
            , "\n\t style1" , showT style1
            ]

    let biblioRP = makeRelFile . t2s $ biblio1
    let styleRP = makeRelFile . t2s . fromMaybe "resources/chicago-fullnote-bibliography-bb.csl" $ style1 
    let biblioP =  doughP </> biblioRP
    let styleP = doughP </> styleRP 

    let loc1 = Just "en" -- TODO depends on language to be used for
    -- for the conventions in the lit list
    -- must be 2 char (all other seems to be difficult with pandoc-citeproc)
    -- change to new citeproc TODO later - not used 

    -- let bibliofp =
    --         t2s biblio1 :: FilePath
    -- let stylefp =
    --         t2s . fromMaybe "style1 in addRefs2 wer23" $ style1 :: FilePath
    --  Raised the exception when style empty
    when (inform debug) $ putIOwords ["addRefs2-3-1 v0.4.5"
            , "\n\tstyleP", showT styleP
            , "\n\tbiblioP", showT biblioP
            ]

    -- p2 <- readBiblioRefs True bibliofp loc1 stylefp (dyReferences y1) p1
    p2 <- readBiblioRefs (inform debug) biblioP loc1 styleP (dyReferences y1) p1

    when (inform debug) $ putIOwords ["addRefs2-4", "p2\n", showT p2]

    return (Docrep y1 p2)
