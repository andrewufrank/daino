----------------------------------------------------------------------
--
-- Module Shake2 html :
----------------------------------------------------------------------
{- the conversion starts with the root files to produce, 
    it starts with the needs for the *.html
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


module ShakeBake.Shake2html where

import UniformBase 
import Foundational.CmdLineFlags
import Uniform.Shake
import Uniform.TemplateStuff
import Uniform.Http 
import Foundational.SettingsPage
import Foundational.Filetypes4sites

import ShakeBake.Shake2aux
import Wave.Panrep2html  

htmlTestTemplateFn = makeAbsFile 
    "/home/frank/Workspace11/daino/theme/templates/test63html.dtpl"
    
shake2html :: NoticeLevel -> PubFlags -> Settings ->
     Path Abs Dir -> Rules ()
shake2html debug flags sett4 bakedP  =    
    (toFilePath bakedP <> "**/*.html") %> \out -> do  -- from Panrep
    -- calls the copy html if a html exist in dough 
            -- pushes a need for *.pandoc 

    let layout = siteLayout sett4
        doughP = doughDir layout -- the regular dough

    putInformOne debug ["rule **/*.html 1 start ", showT out]

    let outP = makeAbsFile out :: Path Abs File
    let fromFile = doughP </> makeRelativeP bakedP outP
            -- try to see if the file exists in dough 
    fileExists <- io2bool $ doesFileExist' fromFile
    putInformOne debug ["rule **/*.html 2 - fileExist:", showT fileExists]
    
    if fileExists 
        then copyFileToBaked debug doughP bakedP out
        else do
        let bakedFrom = replaceExtension'  "panrep" outP
        putInformOne debug ["rule **/*.html 3 - bakedFrom", showT bakedFrom, "\n"]
        needP [bakedFrom]
        let pdfNeeded = replaceExtension' "pdf" outP
        putInformOne debug ["rule **/*.html 3b - pdfNeeded", showT pdfNeeded, "\n"]
        needP [pdfNeeded]

        putInformOne debug ["\nrule **/*.html 4 continued out" , showT out
                , "bakedFrom", showT bakedFrom]

        needsFound :: [Path Abs File]<- runErr2action $ do
            pan0 <- read8 bakedFrom panrepFileType
            let needsFound1 = map (addFileName bakedP . replaceExtension' "html") . getIndexFiles4meta $ pan0 
            putInformOne debug ["\nrule **/*.html 5 needsFound1" 
                    , showT needsFound1]
            return needsFound1
        needP needsFound

        putInformOne debug ["\nrule **/*.html 6 continued ", showT out]

            -- was ist mit needs2?
            --  bakeOnePanrep2html debug flags bakedFrom sett4 outP 
            -- bakeOnePanrep2html debug flags inputFn sett3 resfn2 = do
        putInformOne debug [ "\n-----\nrule **/*.html 7 fn", showT bakedFrom
                , "\n                    resfn2", showT outP
                ]
        (pan0) <- runErr2action $  read8 bakedFrom panrepFileType
        
        putInformOne debug ["\nrule **/*.html 7b panrep read "
                        , "bakedFrom", showT bakedFrom, "\n"
                        , showT pan0, "\n"
                        , showT (sett pan0)]

        let sett3 = sett pan0
            -- extra4 = extra pan0
            mf = if tufteFlag flags 
                then tufteHtmlTemplateFile $ siteLayout sett3 
                else htmlTemplateFile $ siteLayout sett3
            htmlTemplateFn = templatesDir (siteLayout sett3) </> mf

    -- -- braucht needs fuer die panrep files
        let ixs0 = getIndexFiles4meta pan0 :: [Path Rel File]
    --     let ixs0pan = map (addFileName bakedP 
    --                     . addExtension extPanrep) ixs0 ::[Path Abs File]
    --     putInformOne debug ["\n-----\nrule **/*.html 8"
    --                 , "needs panrep ixs0pan", showT ixs0pan]
        
    --     needP ixs0pan

    --     putInformOne debug["\n-----rule **/*.html 9 siteLayout sett3"
    --                 , showT $ siteLayout sett3]
        putInformOne debug ["-----rule **/*.html 10 mf", showT mf]
        putInformOne debug ["-----rule **/*.html 11 masterfn"
                        , showT htmlTemplateFn]

        targetTempl  <- runErr2action $ compileTemplateFile2 htmlTemplateFn
        testTempl <- runErr2action $ compileTemplateFile2 htmlTestTemplateFn

        let metaplus5 = pan0  -- {extra = extra5}

        -- putInformOne debug ["-----rule **/*.html 12", "extra5", showT extra]
        putInformOne debug ["-----rule **/*.html 13", "metaplus5", showT metaplus5]

        let ht1 = fillTemplate_render targetTempl metaplus5
        let test_templatehtml = fillTemplate_render testTempl metaplus5 

        -- putInformOne debug ["panrep2html render html done", "ht1",  ht1 ]
        -- putInformOne debug ["panrep2html render testTemplate done", "test_templatehtml",  test_templatehtml ]

        runErr2action $ do 
            write8 outP htmloutFileType ( HTMLout ht1) 
            write8 outP tthFileType test_templatehtml

        when (inform debug) $ putIOwords
            ["-----rule **/*.html 14 bakeOnePanrep2html done fn"
            , showT outP]

        
        -- needs to start the pdf production 
        let is2pdf = map (addFileName bakedP . 
                        addExtension extPDF) ixs0 
                        -- to avoid pdfs extTex) ixs0 
        let this2pdf = addExtension extTex outP 

        putInformOne debug 
            ["-----rule **/*.html 15 needsfor pdf (now tex)"
            , showT this2pdf, showT is2pdf]

        needP $ this2pdf : is2pdf

        putInformOne debug ["\n-----rule **/*.html 15 end continued 4"
            , showT out,"\n"]


fillTemplate_render  tpl dat = render (Just 50)
        -- just a plausible line length of 50 
        $  renderTemplate tpl (toJSON dat)
 