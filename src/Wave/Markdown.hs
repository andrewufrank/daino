---------------------------------------------------------------------
--
-- Module      :  Wave.Markdown 
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
{-# OPTIONS_GHC -Wall  #-}

module Wave.Markdown (
    module Wave.Markdown,
) where

import UniformBase

import Foundational.Foundation  
import Uniform.Json
import Foundational.MetaPage 

import Uniform.Pandoc
 
import Foundational.Filetypes4sites  
-- import Uniform2.HTMLout  

-- data DocrepJSON = DocrepJSON {yam :: Value, blocks :: [Block]} -- a json value
data DocrepJSON = DocrepJSON {yam1 :: Value, pan1 :: Pandoc} -- a json value
    deriving (Show, Read, Eq, Generic, Zeros)


docrepJSON2docrep :: DocrepJSON -> Docrep
docrepJSON2docrep (DocrepJSON j p) = Docrep j p

readMarkdown2docrepJSON :: MarkdownText -> ErrIO DocrepJSON

{- | read a md file into a DocrepJSON
 reads the markdown file with pandoc and extracts the yaml metadaat
 the metadata are then copied over to the meta part
 and converted in regular json
 attention: there is potential duplication
 as the metadata are partially duplicated
-}
readMarkdown2docrepJSON md = do
    pd <- readMarkdown2 md
    let meta2 = flattenMeta . getMeta $ pd
    return (DocrepJSON meta2 pd)

md2docrep :: NoticeLevel -> SiteLayout -> Path Abs File -> MarkdownText -> ErrIO Docrep

{- | process one md to a docrep
 for bakeOneMD2docrep and report_metaRec
-}
md2docrep debug layout2 inputFn md1 = do
    let doughP = doughDir layout2 -- the regular dough
        bakedP = bakedDir layout2

    dr1 <- readMarkdown2docrepJSON md1
    -- with a flattened version of json from Pandoc
    -- what does it contain?
    when (inform debug) $ putIOwords ["md2docrep", "dr1", showT dr1]

    -- check
    -- the fields for the index are prepared
    -- merge the yaml metadata with default to have the
    -- necessary values set

    dr2 <- completeDocRep debug doughP bakedP inputFn dr1
    -- let dr2a = docRepJSON2docrep  dr2

    -- uses the refs listed in the file and discovred by pandoc,
    -- as well as nocite
    -- therefore must use json
    dr3 <- addRefs debug dr2
    -- TODO needs refs
    -- let needs1  = docrepNeeds docrep1  :: [FilePath]
    -- need  needs1  -- TDO this is in the wrong monad
    -- dr4 <- addIndex2yam debug dr3
    -- this will be done twice in html and tex
    return . docrepJSON2docrep $ dr3

-------------------------------------
completeDocRep :: NoticeLevel -> Path Abs Dir -> Path Abs Dir -> Path Abs File -> DocrepJSON -> ErrIO DocrepJSON
-- complete the DocrepJSON (permitting defaults for all values)
-- the bakedP root is necessary to complete the style and bib entries
-- as well as image?
-- first for completeness of metadata in yaml
-- fails if required labels are not present
completeDocRep debug doughP bakedP filename (DocrepJSON y1 p1) = do
    let m0 = def :: MetaPage
        mFiles = addFileMetaPage doughP bakedP filename
        y2 = mergeLeftPref [toJSON mFiles, y1, toJSON m0]
    -- preference of files as computed
    -- over what is set in md file
    -- over default
    -- y2 <- completeMetaPage doughP bakedP filename y1
    -- let y3 = mergeLeftPref [toJSON y2, y1]
    let y3 = putAtKey "dyFn" (s2t . toFilePath $ filename) . putAtKey "dyLink" (s2t . toFilePath $  filename) $ y2
    when (inform debug) $ putIOwords ["completeDocRep", "y3", showT y3]
    return (DocrepJSON y3 p1)

--------------------------------
addRefs :: NoticeLevel -> DocrepJSON -> ErrIO DocrepJSON
{- ^ add the references to the pandoc block
 the biblio is in the yam (otherwise nothing is done)
 ths cls file must be in the yam
-}

-- processCites :: Style -> [Reference] -> Pandoc -> Pandoc

-- Process a Pandoc document by adding citations formatted according to a CSL style. Add a bibliography (if one is called for) at the end of the document.
-- http://hackage.haskell.org/package/citeproc-hs-0.3.10/docs/Text-CSL.html
--   m <- readBiblioFile "mybibdb.bib"
--   s <- readCSLFile "apa-x.csl"
--   let result = citeproc procOpts s m $ [cites]
--   putStrLn . unlines . map (renderPlainStrict) . citations $ result

addRefs debug dr1@(DocrepJSON y1 p1) = do
    -- the biblio entry is the signal that refs need to be processed
    -- only refs do not work
    when (inform debug) $ putIOwords ["addRefs", showT dr1, "\n"]
    let biblio1 = getAtKey y1 "bibliography" :: Maybe Text
    maybe (return dr1) (addRefs2 debug dr1) biblio1

addRefs2 :: 
    NoticeLevel ->
    DocrepJSON ->
    Text ->
    ErrIO DocrepJSON
addRefs2 debug dr1@(DocrepJSON y1 p1) biblio1 = do
    --   let debugx = False
    when (inform debug) $ putIOwords ["addRefs2-1", showT dr1, "\n"]
    let style1 = getAtKey y1 "style" :: Maybe Text
        refs1 = gak y1 "references" :: Maybe Value -- is an array
        -- refs1 = y1 ^? key "references" :: Maybe Value -- is an array
        nocite1 = getAtKey y1 "nocite" :: Maybe Text
    --   let style1 = syStyle y1
    --       rers1 = dy
    when (inform debug) $
        putIOwords
            [ "addRefs2-2"
            , "\n biblio"
            , showT biblio1 -- is only biblio "resources/BibTexLatex.bib"
            , "\n style"
            , showT style1 -- style Just "/home/frank/Workspace8/ssg/docs/site/dough/resources/chicago-fullnote-bibliography-bb.csl"
            , "\n refs"
            , showT refs1
            , "\n nocite"
            , showT nocite1
            ]

    let loc1 = Just "en" -- TODO depends on language to be used for
    -- for the conventions in the lit list
    -- must be 2 char (all other seems to be difficult with pandoc-citeproc)
    -- change to new citeproc TODO later
    -- let refs2 = fromJustNote "refs in addRefs2 vcbnf refs2" refs1 :: Value
    -- let refs3 = fromJSONValue refs2 -- :: Result [Reference]
    -- let refs4 = fromJustNote "addRefs2 08werwe refs4" refs3 :: [Reference]

    let bibliofp =
            t2s biblio1 :: FilePath
    let stylefp =
            t2s . fromJustNote "style1 in addRefs2 wer23" $ style1 :: FilePath
    --  Raised the exception when style empty
    when (inform debug) $ putIOwords ["addRefs2-3-1", "done"]

    p2 <- readBiblioRefs (inform debug) bibliofp loc1 stylefp  refs1 p1  



    when (inform debug) $ putIOwords ["addRefs2-4", "p2\n", showT p2]

    return (DocrepJSON y1 p2)
    
