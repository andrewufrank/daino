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

import Foundational.SettingsPage  
import Foundational.MetaPage
  
import Foundational.Filetypes4sites ( Docrep(Docrep), meta1)
import Foundational.CmdLineFlags
    ( PubFlags(draftFlag, privateFlag) )
import Uniform.Pandoc
    (pandocProcessCites, markdownFileType, readMarkdown2 )
import Uniform.Latex
import Lib.FileHandling
import Lib.OneMDfile
import Foundational.MetaPage (MetaPage(dyDoNotReplace))
import Lib.FileHandling (readErlaubt)

readMarkdownFile2docrep  :: NoticeLevel -> Settings ->  Path Abs File ->  ErrIO Docrep 
-- read a markdown file and convert to docrep
readMarkdownFile2docrep debug sett3 fnin = do
    when (inform debug) $ putIOwords 
        ["readMarkdownFile2docrep fnin", showPretty fnin]
        -- place to find PandocParseError

    mdfile <- read8 fnin markdownFileType 
    pd <- readMarkdown2 mdfile
    -- could perhaps "need" all ix as files?

    -- let doc1 = pandoc2docrep doughP fnin  pd
    let meta6 = pandoc2MetaPage sett3 fnin  pd 
    -- gets value from pandoc reading of yaml header
    -- fills values which can be defaulted (if not present)
    let doc1 = Docrep meta6 pd 

    let langCode = latexLangConversion . dyLang . meta1 $ doc1
    let debugReplace = inform debug 
    doc2 <- if langCode == "ngerman"  -- obtained e.g. from YAML header
        then  do 
            erl1 <- readErlaubt (replaceErlaubtFile . siteLayout $ sett3)
            let addErl = dyDoNotReplace . meta1 $ doc1
                -- allow additions to the list in the YAML header
                -- addErl2 = fromJustNote "sdfwer" $ splitOn' addErl ","
                erl2 = addErl ++ erl1
            changed <- applyReplace debugReplace erl2   fnin 
            if (changed && (not debugReplace)) 
                then readMarkdownFile2docrep debug  sett3 fnin 
                -- when debug true then changed files are not written 
                else return doc1
        else return doc1
    return doc2

applyReplace :: Bool -> [Text] -> Path Abs File -> ErrIO Bool 
-- apply the replace for german 
-- if any change true; needs rereading 
applyReplace debugReplace erl2 fnin = do 
    when debugReplace $ putIOwords 
        ["applyReplace fnin", showPretty fnin
        , "\t erlaubt:", showT erl2]
    
    -- let         fnerl = makeAbsFile "/home/frank/Workspace11/replaceUmlaut/nichtUmlaute.txt" :: Path Abs File
    -- cdir <- currentDir
    -- let fnerlabs =   fnerl :: Path Abs File
    -- erl2 <- readErlaubt fnerlabs
    res <- procMd1 debugReplace erl2 fnin
    when debugReplace $ putIOwords ["applyReplace done. Changed:", showT res ]

    return res 

    


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
 the cls file must be in the yaml as well

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

filterNeeds :: NoticeLevel -> PubFlags -> Settings -> Path Rel File -> ErrIO(Maybe (Path Rel File))
-- ^ for md check the flags

filterNeeds debug pubf sett4 fn =  do 
    when (inform debug) $ 
        putIOwords ["filterNeeds", "\nPubFlags", showT pubf ]
    let doughP = doughDir . siteLayout $ sett4
    d1 <- readMarkdownFile2docrep debug sett4  (doughP </> fn) 
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

