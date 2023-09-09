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
import Uniform.MetaPlus  
-- import Development.Shake 
import Foundational.SettingsPage  
import Foundational.Filetypes4sites 
import Foundational.CmdLineFlags
import Uniform.Pandoc
import Uniform.Latex
-- import Uniform.TemplateStuff
import Uniform.Shake  
-- import Uniform.MetaPlus
import ShakeBake.Shake2indexes (fillTextual4MP)
import Text.Pandoc.Walk
import Text.Pandoc.Definition
import Lib.OneMDfile
import Lib.FileHandling
default (Text)


-- the conversion in shake2docrep
-- it produces for debuggin the pandoc values in a ttp file
pandoc2metaplus :: NoticeLevel -> Settings -> Path Abs File ->    ErrIO DainoMetaPlus
pandoc2metaplus debug sett4 bakedFrom  = do

    pandoc1 <- readMd2pandoc bakedFrom -- need posted
    -- apply the umlaut conversion first 
    -- treat the .md file and reread it if changed
    let langCode = latexLangConversion 
            . getTextFromYaml5 "" "language" $ pandoc1
        debugReplace = inform debug 
    pandoc2 <- if langCode == "ngerman"
        then do
            erl1 :: [Text] <- readErlaubt (replaceErlaubtFile . siteLayout $ sett4)
            let addErl = words' . getTextFromYaml5 "" "DoNotReplace" $ pandoc1
                -- allow additions to the list in the YAML header
                -- addErl2 = fromJustNote "sdfwer" $ splitOn' addErl ","
                erl2 = addErl ++ erl1
            changed <- applyReplace debugReplace erl2   bakedFrom 
            if (changed && (not debugReplace)) 
                then readMd2pandoc bakedFrom 
                -- then readMarkdownFile2docrep debug  sett4 bakedFrom 
                -- when debug true then changed files are not written 
                else return pandoc1
        else return pandoc1


    let p2 = addListOfDefaults (metaDefaults sett4) pandoc2
    let p3 = walk lf2LineBreak p3
    -- to convert the /lf/ marks in hard LineBreak
    meta1 <- md2Meta_Process p3 -- includes process citeproc

            -- pushes all what is further needed into metaplus
    let mp1 = setMetaPlusInitialize sett4 bakedFrom meta1

    mp2 <- completeMetaPlus mp1  -- converts the body to tex and html
    let mp3 = fillTextual4MP mp2 

    return mp3

lf2LineBreak :: Inline -> Inline
-- | converts a "/lf/" string in a hard linebreak
lf2LineBreak (Str "/lf/")= LineBreak 
lf2LineBreak a = a

applyReplace :: Bool -> [Text] -> Path Abs File -> ErrIO Bool 
-- apply the replace for german 
-- if any change true; needs rereading 
applyReplace debugReplace erl2 fnin = do 
    when debugReplace $ putIOwords 
        ["applyReplace fnin", showPretty fnin
        , "\t erlaubt:", showT erl2]
    res <- procMd1 debugReplace erl2 fnin
    when debugReplace $ putIOwords ["applyReplace done. Changed:", showT res ]

    return res 

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

setMetaPlusInitialize :: Settings -> Path Abs File -> Meta 
            -> (MetaPlus Settings DainoValues)
            -- -> DainoMetaPlus 
-- to move the start values into the MetaPlus 
setMetaPlusInitialize sett3 fnin m1 =  
        (zero:: MetaPlus Settings DainoValues) 
        { metap = m1
        , sett = sett3
        , extra = x1}  :: MetaPlus Settings DainoValues
    where 
        doughP = doughDir . siteLayout $ sett3
        relFn = makeRelativeP doughP fnin
        lang = getTextFromYaml6 "lang" "en-US" m1 :: Text
        x1 = zero   { mdFile = toFilePath fnin
                    , mdRelPath =toFilePath $  relFn
                    , dainoVersion = showT Paths_daino.version
                    , latLanguage = latexLangConversion lang 
                    , pdf2 = toFilePath $ replaceExtension2 ".pdf" relFn }

metaDefaults ::  Settings -> [(Text, Text)]
metaDefaults sett9 =  
     [("Bibliography", "resources/BibTexLatex.bib")
    , ("version", "publish")  -- todo should probably not be default
    ,  ("visibility", "public") 
    , ("title", "Title MISSING")
    , ("abstract", "Abstract MISSING")
    , ("date", showT year2000)
    , ("lang", "en")  -- todo conversion? 
    , ("latLanguage", "english") -- for babel - todo 
    , ("styleBiber","authoryear")
    , ("headerShift","1")
    , ("author", settingsAuthor sett9)
    , ("sortOrder", "filename")
    -- , ("indexPage", False) detect from name 'index.md'
    ] 
-- "resources/webbiblio.bib")
-- check that defaults work? 
-- defaults are set in panrep2html (and 2latex??)

metaSetBook :: Settings -> DainoMetaPlus -> DainoValues
-- | set 2 values to allow boolean tests in templates
metaSetBook sett4 dr1 =  extra5{authorReduced = blankAuthorName hpname aut1
                            ,booklet = "booklet" == bookval
                            ,bookBig= "bookBig" == bookval
                            ,webroot = s2t $ toFilePath bakedP
                            }
    where 
            aut1 = getTextFromYaml6 defaut "author" meta5
            bookval = getTextFromYaml6  "" "book"   meta5 
            -- doughP = doughDir layout -- the regular dough
            bakedP = bakedDir layout -- the regular dough
            layout = siteLayout sett4 
            hpname = blogAuthorToSuppress layout
            defaut = defaultAuthor layout
            meta5 = metap  dr1 -- ~ panyam 
            extra5 = extra dr1
        

fillTemplate_render  tpl dat = render (Just 50)
        -- just a plausible line length of 50 
        $  renderTemplate tpl (toJSON dat)        

-- readMarkdownFile2docrep  :: NoticeLevel -> PubFlags -> Settings ->  Path Abs File ->  ErrIO Docrep 
-- -- read a markdown file and convert to docrep
-- -- reads setting file!
-- readMarkdownFile2docrep debug flags sett3 fnin = do
--     -- let debug = NoticeLevel0   -- avoid_output_fromHere_down
--     -- when (inform debug) $ 
--     putInform  NoticeLevel2 
--         ["readMarkdownFile2docrep fnin", showPretty fnin]
--         -- place to find PandocParseError
--     p1 <- readMd2pandoc fnin

--     putInform debug ["readMarkdownFile2docrep p1", showPretty p1]
        
--     -- check for german and process umlaut, 
--     -- repeat readMd2pandoc if changed 

--     -- default values only what is used for citeproc and ??
--     -- rest can go into settings 
--     -- these are copied from previous values (OLD below)

--     let defs1 = [("Bibliography", "resources/BibTexLatex.bib")
--                 , ("version", "publish")  -- todo should probably not be default
--                 ,  ("visibility", "public") 
--                  , ("title", "Title MISSING")
--                 , ("abstract", "Abstract MISSING")
--                 , ("date", showT year2000)
--                 , ("lang", "en")  -- todo conversion? 
--                 , ("latLanguage", "english") -- for babel - todo 
--                 , ("styleBiber","authoryear")
--                 , ("headerShift","1")
--                 , ("author", settingsAuthor sett3)
--                 , ("sortOrder", "filename")
--                 -- , ("indexPage", False) detect from name 'index.md'
--                 ] 
--             -- "resources/webbiblio.bib")
--             -- check that defaults work? 
--             -- defaults are set in panrep2html (and 2latex??)
--     let p2 = addListOfDefaults defs1 p1
--     m1 <- md2Meta_Process p2
--     -- process citeproc
--     let mp1 = setMetaPlusInitialize sett3 fnin m1
--         incl = includeBakeTest3docrep flags (metap mp1) 

--     putInform debug 
--         ["readMarkdownFile2docrep end mp1", showPretty mp1]
--     putInform NoticeLevel1 ["readMarkdownFile2docrep end include", showPretty incl]
--     return $ if incl then mp1 else zero 


             
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



