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
    ( MarkdownText(..),
      Pandoc,
      pandocProcessCites,
      getMeta,
      flattenMeta,
      readMarkdown2 
      )
import Uniform.Shake (makeRelativeP)

import Foundational.Filetypes4sites ( Docrep(Docrep, meta1) )


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
pandoc2docrep  doughP filename pd = 
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
                , dyBibliography = getAtKey meta2 "bibliography"
                -- used as signal for processing biblio
                , dyImage = fromMaybe "" $ getAtKey meta2 "image"
                , dyImageCaption = fromMaybe "" $ getAtKey meta2 "caption"
                , dyKeywords = fromMaybe "" $ getAtKey meta2 "keywords"
                , dyStyle = getAtKey meta2 "style"
                , dyStyleBiber = fromMaybe "authoryear" $ getAtKey meta2 "styleBiber"
                , dyNoCite = getAtKey meta2 "nocite"
                , dyReferences = gak meta2 "references"
                , dyContentFiles = maybeToList  . getAtKey meta2 $ "content"
                -- TODO make reading a list
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
