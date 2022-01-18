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

import Foundational.LayoutFlags ( SiteLayout(doughDir) )
import Foundational.MetaPage
import Uniform.Json ( gak, AtKey(getAtKey) )

import Uniform.Pandoc
    -- ( MarkdownText(..),
    --   Pandoc,
    --   pandocProcessCites,
    --   getMeta,
    --   flattenMeta,
    --   readMarkdown2 
    --   )
import Uniform.Shake (makeRelativeP)

import Foundational.Filetypes4sites ( Docrep(Docrep, meta1) )

readMarkdownFile2docrep  :: NoticeLevel -> Path Abs Dir -> Path Abs File -> ErrIO Docrep 
-- read a markdown file and convert to docrep
readMarkdownFile2docrep debug doughP fnin = do
    when (inform debug) $ putIOwords 
        ["getFile2index fnin", showPretty fnin]

    mdfile <- read8 fnin markdownFileType 
    pd <- readMarkdown2 mdfile
    -- could perhaps "need" all ix as files?

    let doc1 = pandoc2docrep doughP fnin pd
    return doc1

{- | process one md to a docrep
 for bakeOneMD2docrep and report_metaRec
 add the refs 
-}
md2docrep :: NoticeLevel
    -> SiteLayout
    -> Path Abs File
    -> MarkdownText
    -> ErrorT Text IO Docrep
md2docrep debug layout2 inputFn md1 = do
    let doughP = doughDir layout2 -- the regular dough
        -- bakedP = bakedDir layout2

    pd <- readMarkdown2 md1 -- to pandoc 
    -- take metadata and fill metaPage (including IndexEntry)
    let dr1 = pandoc2docrep  doughP inputFn pd -- to dr1
    -- with a flattened version of json from Pandoc
    -- convert to metaPage with Index 

    -- check
    -- the fields for the index are prepared
    -- merge the yaml metadata with default to have the
    -- necessary values set

    when (inform debug) $ putIOwords ["\nmd2docrep", "dr1", showT dr1]

    -- uses the refs listed in the file and discovred by pandoc,
    -- as well as nocite
    -- therefore must use json
    dr3 <- addRefs debug dr1  -- to dr3 

    -- does currently not add anything.. needs minimal working siteNameExample

    when (inform debug) $ putIOwords ["\nmd2docrep after addRefs", "dr3", showT dr3]

    return dr3 -- same as T.docrep 

pandoc2docrep ::  Path Abs Dir ->  Path Abs File -> Pandoc -> Docrep
{- | convert the pandoc text to DocrepJSON
 reads the markdown file with pandoc and extracts the yaml metadaat
 the metadata are then converted to metaPage
 -- duplication possible for data in the pandoc metada (no used)
 TODO may use json record parse, which I have already done
-}
-- pure 
pandoc2docrep  doughP filename pd = Docrep meta6 pd
    where 
        meta6 = pandoc2MetaPage doughP filename pd 



--------------------------------
addRefs :: NoticeLevel -> Docrep -> ErrIO Docrep
{- ^ add the references to the pandoc block
 the biblio is in the yaml (otherwise nothing is done)
 ths cls file must be in the yaml as well

-}


-- Process a Pandoc document by adding citations formatted according to a CSL style. Add a bibliography (if one is called for) at the end of the document.

addRefs debug dr1@(Docrep y1 p1) = do
    -- the biblio entry is the signal that refs need to be processed
    when (inform debug) $ putIOwords ["addRefs", showT dr1, "\n"]
    case (dyBibliography y1) of
        Nothing -> (return dr1) 
        Just _ ->  do

            when (informAll debug) $ putIOwords 
                ["addRefs2-1", showT $ dyFn y1
                    -- , "\npandoc", showT dr1, "\n"
                    , "\n\t biblio1" , showT $ dyBibliography y1
                    , "\n\t style1" , showT $ dyStyle y1
                    ]

            p2 <- pandocProcessCites  p1
        
            when (inform debug) $ putIOwords ["addRefs2-4", "p2\n", showT p2]

            return (Docrep y1 p2)
