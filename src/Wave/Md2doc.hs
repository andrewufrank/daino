---------------------------------------------------------------------
--
-- Module      :  Wave.Md2doc
-- the conversion of markdown to docrep
-- missing umlaut conversion

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
{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
             #-}
            -- -fno-warn-unused-matches

module Wave.Md2doc (
    module Wave.Md2doc,
    -- MarkdownText (..),
) where

import Paths_daino (version)

import UniformBase
import Uniform.MetaPlus hiding (MetaPlus(..), Settings(..), ExtraValues(..)) 

import Foundational.SettingsPage  
-- import Foundational.MetaPage
-- import Uniform.MetaStuff ( md2Meta, getValue4meta,setValue2meta)
import Foundational.Filetypes4sites 
-- ( Docrep(Docrep), meta1)
import Foundational.CmdLineFlags
--     ( PubFlags(draftFlag, privateFlag) )
import Uniform.Pandoc
--     (pandocProcessCites, markdownFileType, readMarkdown2 )
import Uniform.Latex
-- import Lib.FileHandling
-- import Lib.OneMDfile
-- import Foundational.MetaPage (MetaPage(dyDoNotReplace))
-- import Lib.FileHandling (readErlaubt)
import Uniform.Shake ( Path2nd(makeRelativeP) ) 
-- import Uniform.Json (ToJSON(toJSON))
readMarkdownFile2docrep  :: NoticeLevel -> Settings ->  Path Abs File ->  ErrIO Docrep 
-- read a markdown file and convert to docrep
readMarkdownFile2docrep debug sett3 fnin = do
    let debug = NoticeLevel0
    -- when (inform debug) $ 
    putIOwords 
        ["readMarkdownFile2docrep fnin", showPretty fnin]
        -- place to find PandocParseError
    p1 <- readMd2pandoc fnin

    when (inform debug) $ 
        putIOwords ["readMarkdownFile2docrep p1", showPretty p1]
        
    -- check for german and process umlaut, 
    -- repeat readMd2pandoc if changed 

    -- default values only what is used for citeproc and ??
    -- rest can go into settings 
    -- these are copied from previous values (OLD below)

    let defs1 = [("Bibliography", "resources/BibTexLatex.bib")
                , ("version", "publish")  -- todo should probably not be default
                ,  ("visibility", "public") 
                 , ("title", "Title MISSING")
                , ("abstract", "Abstract MISSING")
                , ("data", showT year2000)
                , ("lang", "en")  -- todo conversion? 
                , ("latLanguage", "english") -- for babel - todo 
                , ("styleBiber","authoryear")
                , ("headerShift","1")
                -- , ("indexPagg", False) detect from name 'index.md'
                ] 
            -- "resources/webbiblio.bib")
    let p2 = addListOfDefaults defs1 p1
    m1 <- md2Meta_Process p2
    let mp1 = setMetaPlusInitialize sett3 fnin m1

    -- mdfile <- read8 fnin markdownFileType 
    -- -- todo -- add umlaut test foer german
    -- (Pandoc m1 p1) <- md2Meta_Readmd fnin mdfile 
    -- let m2 = addListOfDefaults (yamlValues sett3 fnin) m1
    -- meta3 <- md2Meta_Process (Pandoc m2 p1)

    when (inform debug) $ 
        putIOwords 
        ["readMarkdownFile2docrep end mp1", showPretty mp1]
    return mp1

setMetaPlusInitialize :: Settings -> Path Abs File -> Meta -> DainoMetaPlus 
-- to move the start values into the MetaPlus 
setMetaPlusInitialize sett3 fnin m1 =  zero { metap = m1
                                , sett = sett3
                                , extra = x1}
    where 
        doughP = doughDir . siteLayout $ sett3
        lang = getTextFromYaml6 "lang" "en-US" m1 :: Text
        x1 = zero   { mdFile = toFilePath fnin
                    , mdRelPath =toFilePath $  makeRelativeP doughP fnin
                    , dainoVersion = showT version
                    , latLanguage = latexLangConversion lang }
        --    , metaMarkdown = resBody
        --    , metaHtml = resBodyhtml
        --    , metaLatex = zero 
             

------------------OLD -----------------------


-- yamlValues :: Settings -> Path Abs File ->  [(Text,Text)]
-- -- values to go into metadata 
-- -- includes the defaults (were values with dy~ prefix)
-- yamlValues sett3 fn = 
--         [("fn", s2t $ toFilePath fn)
--         ,("link", s2t $ toFilePath relfn)
--         , ("title", "Title MISSING")
--         , ("abstract", "Abstract MISSING")
--         , ("data", showT year2000)
--         , ("lang", "en")  -- todo conversion? 
--         , ("latLanguage", "english") -- for babel
--         , ("sitename", s2t . sitename . siteHeader $ sett3)
--         , ("sitebyline", byline . siteHeader $ sett3)
--         , ("sitebanner",  s2t . banner . siteHeader $ sett3)
--         , ("sitebannerCaption", bannerCaption . siteHeader $ sett3)
--         , ("style", "resources/chicago-fullnote-bibliography-bb.csl")
--         , ("styleBiber","authoryear")
--         -- , ("dyDainoieVersion","daino v 0.1.5.6.2")
--         -- set later when filling tempalte
--         , ("visibility", "public") -- 
--         -- no default for publish, must be set in YAML 
--         , ("headerShift","1")
--         , ("settingsAuthor", settingsAuthor sett3 )
--         -- , ("page-title", "PageTitleEx")  not yet used
--         -- , ("page-title-postfix", "PageTitle-PostfixEx")
--         , ("dainoversion","0.1.5.6.2") -- todo set automatically
--         ]
--     where 
--         layout = siteLayout sett3
--         doughP = doughDir layout
--         -- defAuthor = defaultAuthor layout 
--         -- defBiblio = defaultBibliography layout  
--         relfn = makeRelativeP doughP fn
--         -- sett3json = toJSON sett3 

--  used from the yaml, original names left (no dy~)                    
--             , title = dyTitle
--             , abstract = dyAbstract
--             , author = dyAuthor
--             , date = fromMaybe (showT year2000) dyDate
--             , content = zero
--             -- , publish = dyVersion
--             , dirEntries = zero
--             , fileEntries = zero
--             , headerShift = dyHeaderShift

 ---------------------OLD 
    -- could perhaps "need" all ix as files? these are done in docrep2panrep

    -- -- let doc1 = pandoc2docrep doughP fnin  pd
    -- let meta6 = pandoc2MetaPage sett3 fnin  pd 
    -- -- gets value from pandoc reading of yaml header
    -- -- fills values which can be defaulted (if not present)
    -- let doc1 = Docrep meta6 pd 

    -- let langCode = latexLangConversion . dyLang . meta1 $ doc1
    -- let debugReplace = inform debug 
    -- doc2 <- if langCode == "ngerman"  -- obtained e.g. from YAML header
    --     then  do 
    --         erl1 <- readErlaubt (replaceErlaubtFile . siteLayout $ sett3)
    --         let addErl = dyDoNotReplace . meta1 $ doc1
    --             -- allow additions to the list in the YAML header
    --             -- addErl2 = fromJustNote "sdfwer" $ splitOn' addErl ","
    --             erl2 = addErl ++ erl1
    --         changed <- applyReplace debugReplace erl2   fnin 
    --         if (changed && (not debugReplace)) 
    --             then readMarkdownFile2docrep debug  sett3 fnin 
    --             -- when debug true then changed files are not written 
    --             else return doc1
    --     else return doc1
    -- return doc2

-- applyReplace :: Bool -> [Text] -> Path Abs File -> ErrIO Bool 
-- -- apply the replace for german 
-- -- if any change true; needs rereading 
-- applyReplace debugReplace erl2 fnin = do 
--     when debugReplace $ putIOwords 
--         ["applyReplace fnin", showPretty fnin
--         , "\t erlaubt:", showT erl2]
    
--     -- let         fnerl = makeAbsFile "/home/frank/Workspace11/replaceUmlaut/nichtUmlaute.txt" :: Path Abs File
--     -- cdir <- currentDir
--     -- let fnerlabs =   fnerl :: Path Abs File
--     -- erl2 <- readErlaubt fnerlabs
--     res <- procMd1 debugReplace erl2 fnin
--     when debugReplace $ putIOwords ["applyReplace done. Changed:", showT res ]

--     return res 

    


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



-- --------------------------------
-- addRefs :: NoticeLevel -> Docrep -> ErrIO Docrep
-- {- ^ add the references to the pandoc block
--  the biblio is in the yaml (otherwise nothing is done)
--  the cls file must be in the yaml as well

-- -}


-- -- Process a Pandoc document by adding citations formatted according to a CSL style. Add a bibliography (if one is called for) at the end of the document.

-- addRefs debug dr1@(Docrep y1 p1) = do
--     -- the biblio entry is the signal that refs need to be processed
--     when (inform debug) $ putIOwords ["addRefs", showT dr1, "\n"]
--     case (dyBibliography y1) of
--         Nothing -> (return dr1) 
--         Just _ ->  do

--             when (inform debug) $ putIOwords 
--                 ["addRefs2-1", showT $ dyFn y1
--                     -- , "\npandoc", showT dr1, "\n"
--                     , "\n\t biblio1" , showT $ dyBibliography y1
--                     , "\n\t style1" , showT $ dyStyle y1
--                     ]

--             p2 <- pandocProcessCites  p1
        
--             when (inform debug) $ putIOwords ["addRefs2-4", "p2\n", showT p2]

--             return (Docrep y1 p2)

filterNeeds :: NoticeLevel -> PubFlags -> Settings -> Path Rel File -> ErrIO(Maybe (Path Rel File))
-- ^ for md check the flags

filterNeeds debug pubf sett4 fn =  do 
    when (inform debug) $ 
        putIOwords ["filterNeeds", "\nPubFlags", showT pubf ]
    let doughP = doughDir . siteLayout $ sett4
    d1 <- readMarkdownFile2docrep debug sett4  (doughP </> fn) 
    when (inform debug) $ 
        putIOwords ["filterNeeds2", "\nMeta", showT (meta1 d1) ]

    let t = includeBakeTest3docrep pubf (meta1 . metap $ d1)
    when (inform debug) $ 
        putIOwords ["filterNeeds3 ", "\n t", showT t ]
    return $ if t then Just fn else Nothing

meta1 :: a -> a
meta1 = id

-- includeBakeTest3docrep :: PubFlags -> MetaPage -> Bool 

-- ^ decide whether this is to be included in the bake 

includeBakeTest3docrep :: PubFlags -> Meta -> Bool
includeBakeTest3docrep pubf met2 = 
        (draftFlag pubf || vers1 ==   "publish") 
        -- should be less than eq
            && (privateFlag pubf || vis1 ==  "public")
    where
        -- draftF = draftFlag pubf 
        vers1 = getValue4meta met2 "version" 
        vis1 = getValue4meta met2 "visibility" 

