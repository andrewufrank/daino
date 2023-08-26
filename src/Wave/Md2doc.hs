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
-- import Development.Shake 
import Foundational.SettingsPage  
import Foundational.Filetypes4sites 
import Foundational.CmdLineFlags
import Uniform.Pandoc
    ( addListOfDefaults,
      getTextFromYaml6,
      getValue4meta,
      md2Meta_Process,
      readMd2pandoc )
import Uniform.Latex
import Uniform.Shake  

default (Text)
readMarkdownFile2docrep  :: NoticeLevel -> PubFlags -> Settings ->  Path Abs File ->  ErrIO Docrep 
-- read a markdown file and convert to docrep
-- reads setting file!
readMarkdownFile2docrep debug flags sett3 fnin = do
    -- let debug = NoticeLevel0   -- avoid_output_fromHere_down
    -- when (inform debug) $ 
    putInform  NoticeLevel2 
        ["readMarkdownFile2docrep fnin", showPretty fnin]
        -- place to find PandocParseError
    p1 <- readMd2pandoc fnin

    putInform debug ["readMarkdownFile2docrep p1", showPretty p1]
        
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
                , ("date", showT year2000)
                , ("lang", "en")  -- todo conversion? 
                , ("latLanguage", "english") -- for babel - todo 
                , ("styleBiber","authoryear")
                , ("headerShift","1")
                , ("author", settingsAuthor sett3)
                , ("sortOrder", "filename")
                -- , ("indexPage", False) detect from name 'index.md'
                ] 
            -- "resources/webbiblio.bib")
            -- check that defaults work? 
            -- defaults are set in panrep2html (and 2latex??)
    let p2 = addListOfDefaults defs1 p1
    m1 <- md2Meta_Process p2
    -- process citeproc
    let mp1 = setMetaPlusInitialize sett3 fnin m1
        incl = includeBakeTest3docrep flags (metap mp1) 

    putInform debug 
        ["readMarkdownFile2docrep end mp1", showPretty mp1]
    putInform NoticeLevel1 ["readMarkdownFile2docrep end include", showPretty incl]
    return $ if incl then mp1 else zero 

setMetaPlusInitialize :: Settings -> Path Abs File -> Meta -> DainoMetaPlus 
-- to move the start values into the MetaPlus 
setMetaPlusInitialize sett3 fnin m1 =  zero { metap = m1
                                , sett = sett3
                                , extra = x1}
    where 
        doughP = doughDir . siteLayout $ sett3
        relFn = makeRelativeP doughP fnin
        lang = getTextFromYaml6 "lang" "en-US" m1 :: Text
        x1 = zero   { mdFile = toFilePath fnin
                    , mdRelPath =toFilePath $  relFn
                    , dainoVersion = showT Paths_daino.version
                    , latLanguage = latexLangConversion lang 
                    , pdf2 = toFilePath $ replaceExtension2 ".pdf" relFn }
             
-- filters cannot be moved to another file, circular imports!

-- filterNeeds :: NoticeLevel -> PubFlags -> Settings -> Path Rel File -> ErrIO(Maybe (Path Abs File))
-- -- ^ for md check the flags
-- -- md given as rel file, completes with dir from sitelayout

-- filterNeeds debug pubf sett4 fn =  do 
--     -- let debug = NoticeLevel2   -- avoid_output_fromHere_down
--     putInform debug ["filterNeeds", "\nPubFlags", showT pubf ]
--     let doughP = doughDir . siteLayout $ sett4 :: Path Abs Dir 
--     let fn2 = doughP </> fn :: Path Abs File 
--     filterNeeds2 debug pubf sett4 fn2 
 

-- filterNeeds2 :: NoticeLevel -> PubFlags -> Settings -> Path Abs File -> ErrIO(Maybe (Path Abs File))
-- -- check if file should be included (version > privateFlag)
-- filterNeeds2 debug pubf sett4 fn2 =  do 
--     -- let debug = NoticeLevel0   -- avoid_output_fromHere_down
--     -- when (inform debug) $ 
--         -- putIOwords ["filterNeeds2", "\nPubFlags", showT pubf ]
--     putInform debug ["filterNeeds2", "\nPubFlags", showT pubf ]
    
--     d1 <- readMarkdownFile2docrep debug sett4  fn2 
--     putInform debug ["filterNeeds2", "\nMeta", showT ( d1) ]

--     let t = includeBakeTest3docrep pubf (metap $ d1)
--     putInform debug ["filterNeeds3 ", "\n t", showT t ]
--     return $ if t then Just fn2 else Nothing


-- includeBakeTest3docrep :: PubFlags -> MetaPage -> Bool 

-- ^ decide whether this is to be included in the bake 

includeBakeTest3docrep :: PubFlags -> Meta -> Bool
includeBakeTest3docrep pubf met2 = includeBakeTest3 pubf vers1 vis1
    where
        -- draftF = draftFlag pubf 
        vers1 = getValue4meta met2 "version" 
        vis1 = getValue4meta met2 "visibility" 

includeBakeTest3  :: PubFlags -> Text -> Text -> Bool
includeBakeTest3  pubf vers1 vis1 = 
        (draftFlag pubf || vers1 ==   "publish") 
        -- should be less than eq
            && (privateFlag pubf || vis1 ==  "public")


