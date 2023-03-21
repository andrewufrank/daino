---------------------------------------------------------------------
--
-- Module      :  Wave.Md2doc
-- the conversion of markdown to docrep
------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds       #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wall #-}

module Wave.Md2doc (
    module Wave.Md2doc,
    -- MarkdownText (..),
) where

import UniformBase

import Foundational.MetaPage
    ( pandoc2MetaPage,
      MetaPage(dyFn, dyBibliography, dyStyle, dyVersion, dyVisibility) )
import Foundational.Filetypes4sites ( Docrep(Docrep), meta1)
import Foundational.CmdLineFlags
    ( PubFlags(draftFlag, privateFlag) )
import Uniform.Pandoc
    (pandocProcessCites, markdownFileType, readMarkdown2 )


readMarkdownFile2docrep  :: NoticeLevel -> Path Abs Dir ->  Path Abs File ->  ErrIO Docrep 
-- read a markdown file and convert to docrep
readMarkdownFile2docrep debug doughP fnin = do
    when (informAll debug) $ putIOwords 
        ["readMarkdownFile2docrep fnin", showPretty fnin]
        -- place to find PandocParseError

    mdfile <- read8 fnin markdownFileType 
    pd <- readMarkdown2 mdfile
    -- could perhaps "need" all ix as files?

    -- let doc1 = pandoc2docrep doughP fnin  pd
    let meta6 = pandoc2MetaPage doughP fnin  pd 
    let doc1 = Docrep meta6 pd 
    return doc1



-- pandoc2docrep ::  Path Abs Dir ->  Path Abs File  -> Pandoc -> Docrep
-- {- | convert the pandoc text to DocrepJSON
--  reads the markdown file with pandoc and extracts the yaml metadat
--  the metadata are then converted to metaPage from the json pandoc
--  -- duplication possible for data in the pandoc metada (no used)
--  TODO may use json record parse, which I have already done
-- -}
-- -- pure 
-- pandoc2docrep  doughP filename  pd = Docrep meta6  pd
--     where 
--         meta6 = pandoc2MetaPage doughP filename  pd 



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

            when (inform debug) $ putIOwords 
                ["addRefs2-1", showT $ dyFn y1
                    -- , "\npandoc", showT dr1, "\n"
                    , "\n\t biblio1" , showT $ dyBibliography y1
                    , "\n\t style1" , showT $ dyStyle y1
                    ]

            p2 <- pandocProcessCites  p1
        
            when (inform debug) $ putIOwords ["addRefs2-4", "p2\n", showT p2]

            return (Docrep y1 p2)

filterNeeds :: NoticeLevel -> PubFlags -> Path Abs Dir -> Path Rel File -> ErrIO(Maybe (Path Rel File))
-- ^ for md check the flags

filterNeeds debug pubf doughP fn =  do 
    when (inform debug) $ 
        putIOwords ["filterNeeds", "\nPubFlags", showT pubf ]
    d1 <- readMarkdownFile2docrep debug doughP  (doughP </> fn) 
    when (inform debug) $ 
        putIOwords ["filterNeeds2", "\nMeta", showT (meta1 d1) ]

    let t = includeBakeTest3docrep pubf (meta1 d1)
    return $ if t then Just fn else Nothing



includeBakeTest3docrep :: PubFlags -> MetaPage -> Bool 

-- ^ decide whether this is to be included in the bake 

includeBakeTest3docrep pubf doc1 = 
        (draftFlag pubf || vers1 ==   "publish") 
        -- should be less than eq
            && (privateFlag pubf || vis1 ==  "public")
    where
        -- draftF = draftFlag pubf 
        vers1 = dyVersion   doc1
        vis1 = dyVisibility  doc1

