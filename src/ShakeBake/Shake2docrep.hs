----------------------------------------------------------------------
--
-- Module Shake2 :
----------------------------------------------------------------------
{- the conversion starts with the root files to produce, 
    i.e. only index.md 
    This triggers the rule html -> panrep 
    and panrep2html produces the needs for *.pdf, templates, jpg and bib

    for now the css, dtpl, jpg etc. are still included
    -}
----------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans
            -fno-warn-missing-signatures
            -fno-warn-missing-methods
            -fno-warn-duplicate-exports  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use ++" #-}


module ShakeBake.Shake2docrep where

import UniformBase 
import Foundational.CmdLineFlags
import Uniform.Shake
import Uniform.Pandoc 
import Foundational.SettingsPage
import Foundational.Filetypes4sites

import ShakeBake.Bake
 
import ShakeBake.Shake2aux
-- import Development.Shake (getDirectoryFilesIO)
import Wave.Panrep2html  

import ShakeBake.Shake2html
import ShakeBake.Shake2panrep
import Wave.Md2doc 

shake2docrep debug flags sett4 bakedP  =     
    (toFilePath bakedP <> "**/*.docrep") %> \out -> do 
            -- insert pdfFIles1  -- here start with doughP
    putInform debug ["rule **/*.docrep", showT out]

    let layout = siteLayout sett4 
        doughP = doughDir layout -- the regular dough
        
    let outP = makeAbsFile out :: Path Abs File
    let bakedFrom = replaceDirectoryP  bakedP doughP $  
                        replaceExtension'  "md" outP
    putInform debug ["rule **/*.docrep - bakedFrom", showT bakedFrom]
    needP [bakedFrom]  
  
    
    putInform debug ["rule **/*.docrep continued", showT out]

    _ <- runErr2action $ do -- bakeOneMD2docrep debug flags bakedFrom sett4 outP 
    -- no needs follow 
-- bakeOneMD2docrep debug flags inputFn sett3 resfn2 = do
        when (informAll debug) $    putIOwords
            [ "\n-----------------"
            , "bakeOneMD2docrep 1 fn", showT bakedFrom
            -- , "\n resfn2", showT resfn2
            ]
        -- let layout = siteLayout sett3
        -- let doughP = doughDir layout
        -- let hpname = blogAuthorToSuppress . siteLayout $ sett3
--         dr4 <- readMarkdownFile2docrep NoticeLevel0 flags sett4  bakedFrom 
--         -- dr4 <- addRefs debug dr3
-- readMarkdownFile2docrep debug flags sett3 fnin = do
    -- let debug = NoticeLevel0   -- avoid_output_fromHere_down
    -- when (inform debug) $ 
        putInform  NoticeLevel2 
            ["readMarkdownFile2docrep bakedFrom", showPretty bakedFrom]
            -- place to find PandocParseError
        p1 <- readMd2pandoc bakedFrom -- need posted

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
                    , ("author", settingsAuthor sett4)
                    , ("sortOrder", "filename")
                    -- , ("indexPage", False) detect from name 'index.md'
                    ] 
                -- "resources/webbiblio.bib")
                -- check that defaults work? 
                -- defaults are set in panrep2html (and 2latex??)
        let p2 = addListOfDefaults defs1 p1
        m1 <- md2Meta_Process p2
        -- process citeproc
        let mp1 = setMetaPlusInitialize sett4 bakedFrom m1
            incl = includeBakeTest3docrep flags (metap mp1) 

        putInform debug 
            ["readMarkdownFile2docrep end mp1", showPretty mp1]
        putInform NoticeLevel1 ["readMarkdownFile2docrep end include", showPretty incl]
        let dr4 =  if incl then mp1 else zero     
        write8 outP docrepFileType dr4



        when (inform debug) $
            putIOwords
                [ "\n-----------------"
                , "bakeOneMD2docrep done resfn2"
                , showT outP
                ]
        return []





    putInform debug ["rule **/*.docrep end"] -- no  - needs2", showT needs2]
    return ()
    

  

