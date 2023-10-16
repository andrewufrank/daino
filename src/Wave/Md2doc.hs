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

import Foundational.SettingsPage  
import Foundational.CmdLineFlags
import ShakeBake.Shake2indexes (fillTextual4MP)
import Lib.OneMDfile
import Lib.FileHandling

import UniformBase
import Uniform.MetaPlus  
import Uniform.PandocImports
import Uniform.Markdown 
import Uniform.MetaStuff
import Uniform.TemplateStuff
import Uniform.Latex
import Uniform.Shake  

default (Text)


-- the conversion in shake2docrep
-- it produces for debuggin the pandoc values in a ttp file
md2doc :: NoticeLevel -> Settings -> Path Abs File ->    ErrIO DainoMetaPlus
-- must not added needs (because test for inclusion follows later)
md2doc debug sett4 bakedFrom  = do
    putInformOne debug ["md2doc 1 start ", showT bakedFrom]

    pandoc1 <- readMd2pandoc bakedFrom  
    -- apply the umlaut conversion first 
    -- treat the .md file and reread it if changed
    let langCode = latexLangConversion 
            . getTextFromYaml5 "" "language" $ pandoc1
        debugReplace = False 
        -- with debug not new file written in umlautconversin
                    -- inform debug 
    pandoc2 <- if langCode == "ngerman"
        then do
            putInformOne debug ["md2doc 2 "]
            erl1 :: [Text] <- readErlaubt (replaceErlaubtFile . siteLayout $ sett4)
            let addErl = words' . getTextFromYaml5 "" "DoNotReplace" $ pandoc1
            let addErl2 = words' . getTextFromYaml5 "" "doNotReplace" $ pandoc1
                -- allow additions to the list in the YAML header
                -- with or without capital D
                erl2 = addErl ++ addErl2 ++ erl1
            putInformOne debug ["md2doc 3 "]
            changed <- applyReplace debugReplace erl2   bakedFrom 
            if changed -- && (not debugReplace)
                then do 
                        pan2 <- readMd2pandoc bakedFrom 
                        putInformOne debug ["md2doc 4 "]
                        return pan2
                    -- then readMarkdownFile2docrep debug  sett4 bakedFrom 
                -- when debug true then changed files are not written 
                else return pandoc1
        else return pandoc1

    putInformOne debug ["md2doc 5 umlaut done "]

    let p2 = addListOfDefaults (metaDefaults sett4) pandoc2
    let p3 = walkPandoc lf2LineBreak p2
    -- to convert the /lf/ marks in hard LineBreak
    pan4@(Pandoc m4 p4) <- mdCiteproc p3 
    putInformOne debug ["md2doc 6 citeproc done "]

    let (Pandoc _ p5) = usingSideNotes pan4 
        -- changed the body (p5) to prepare for tufte style

    -- move the body and then converts to html and latex 
    let meta4base = setBlocks2meta "bodyBase" ( p4) m4
    let meta5tufte = setBlocks2meta "bodyTufte" ( p5) meta4base
    -- let meta4base = Meta $ M.insert "bodyBase" (MetaBlocks p4) (unMeta m4)
    -- let meta5tufte = Meta $ M.insert "bodyTufte" (MetaBlocks p5) (unMeta meta4base)
     
    let mp1 = setMetaPlusInitialize sett4 bakedFrom meta5tufte
    -- let mp2 = mp1 { metaHtml = meta5tufte  -- 2 versions of body
    --                 , metaLatex = meta4base}  -- one only
    putInformOne debug ["md2doc 6a usingSidenotes done "]

            -- pushes all what is further needed into metaplus
    mp2 <- completeMetaPlus mp1  -- converts the body to tex and html
    let mp3 = fillTextual4MP mp2 
    putInformOne debug ["md2doc 7 end "]

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
    -- , ("styleBiber","authoryear")
    -- , ("csl", "resources/theme/templates/chicago-fullnote-bibliography.csl")
    , ("headerShift","1")
    , ("author", settingsAuthor sett9)
    , ("sortOrder", "filename")
    -- should book have a default of ""
    -- , ("indexPage", False) detect from name 'index.md'
    ] 
-- "resources/webbiblio.bib")
-- check that defaults work? 
-- defaults are set in panrep2html (and 2latex??)

metaSetBook :: Settings -> DainoMetaPlus -> DainoValues
-- | set 2 values to allow boolean tests in templates
metaSetBook sett4 dr1 =  extra5{authorReduced = blankAuthorName hpname aut1
                            ,booklet = "booklet" == bookval
                            ,bookBig= "bookbig" == bookval
                            ,webroot = s2t $ toFilePath bakedP
                            }
    where 
            aut1 = getTextFromYaml6 defaut "author" meta5
            bookval = toLower' $ getTextFromYaml6  "" "book"   meta5 
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




