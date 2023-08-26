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

 
shake2html :: NoticeLevel -> PubFlags -> Settings -> Path Abs Dir -> Rules ()
shake2html debug flags sett4 bakedP  =    
    (toFilePath bakedP <> "**/*.html") %> \out -> do  -- from Panrep
    -- calls the copy html if a html exist in dough 
            -- pushes a need for *.pandoc 

    let layout = siteLayout sett4
        doughP = doughDir layout -- the regular dough

    putInform debug ["rule **/*.html", showT out]

    let outP = makeAbsFile out :: Path Abs File
    let fromFile = doughP </> makeRelativeP bakedP outP
            -- try to see if the file exists in dough 
    fileExists <- io2bool $ doesFileExist' fromFile
    putInform debug ["rule **/*.html - fileExist:", showT fileExists]
    
    if fileExists 
        then copyFileToBaked debug doughP bakedP out
        else do
        let bakedFrom = replaceExtension'  "panrep" outP
        putInform debug ["rule **/*.html - bakedFrom", showT bakedFrom]
        need [toFilePath bakedFrom]

        putInform debug ["\nrule **/*.html continued 1" , showT out]

        needsFound :: [Path Abs File]<- runErr2action $ do
            pan0 <- read8 bakedFrom panrepFileType
            let needsFound1 = map (addFileName bakedP . replaceExtension' "html") . getIndexFiles4meta $ pan0 
            putInform debug ["\nrule **/*.html needsFound1" 
                    , showT needsFound1]
            return needsFound1
        needP needsFound

        putInform debug ["\nrule **/*.html continued 3", showT out]

            -- was ist mit needs2?
            --  bakeOnePanrep2html debug flags bakedFrom sett4 outP 
            -- bakeOnePanrep2html debug flags inputFn sett3 resfn2 = do
        when (inform debug) $    putIOwords
                [ "\n-----------------"
                , "bakeOnePanrep2html 1 fn"
                , showT bakedFrom
                , "\n resfn2"
                , showT outP
                ]
        (pan0) <- runErr2action $  read8 bakedFrom panrepFileType
        

        -- (p, needsFound3, test_templatehtml) <- 
        --  panrep2html debug  flags pan0
        -- panrep2html debug flags  metaplus4 = do
        -- let debug = NoticeLevel0   -- avoid_output_fromHere_down
        let sett3 = sett pan0
            extra4 = extra pan0
            mf = masterTemplateFile $ siteLayout sett3
            masterfn = templatesDir (siteLayout sett3) </> mf
        let ixs0 = getIndexFiles4meta pan0 :: [Path Rel File]

    -- braucht needs fuer die panrep files

                -- the rel path to index files
        let ixs0pan = map (addFileName bakedP 
                            . addExtension extPanrep) ixs0 :: [Path Abs File]
        putInform debug ["panrep2html", "needs panrep ixs0pan", showT ixs0pan]
        
        needP ixs0pan

        putInform debug["\npanrep2html", "siteLayout sett3"
                    , showPretty $ siteLayout sett3]
        putInform debug ["panrep2html", "mf", showPretty mf]
        putInform debug ["panrep2html", "masterfn", showPretty masterfn]

        targetTempl  <- runErr2action $ compileTemplateFile2 masterfn
        testTempl  <- runErr2action $ compileTemplateFile2 testTemplateFn

            -- htm1 <- meta2xx writeHtml5String2 (metap pan0)

            --if this is an index file it has files and dirs 
            -- putInform debug ["panrep2html", "extra4", showPretty extra4]

        let files = fileEntries  $ extra4 :: [IndexEntry2]
            dirs = dirEntries  $ extra4 :: [IndexEntry2]

            -- let bakedP =   bakedDir . siteLayout $ sett3
        
 

        valsDirs :: [Maybe IndexEntry2]<- runErr2action $ mapM 
                            (getVals2html debug flags bakedP) dirs
        valsFiles :: [Maybe IndexEntry2] <- runErr2action $ mapM 
                        (getVals2html debug flags bakedP) files

        putInform debug["panrep2html", "valsDirs", showPretty valsDirs]
        putInform debug ["panrep2html", "valsFiles", showPretty valsFiles]

        let extra5 = extra4{fileEntries = catMaybes valsFiles
                            , dirEntries = catMaybes valsDirs}
        let metaplus5 = pan0{extra = extra5}

            -- putInform debug ["panrep2html", "extra5", showPretty extra5]
            -- putInform debug ["panrep2html", "metaplus5", showPretty metaplus5]

            -- let hpl1 = renderTemplate targetTempl (toJSON metaplus5)  -- :: Doc Text
        let ht1 = fillTemplate_render targetTempl metaplus5

            -- let ttpl1 = renderTemplate testTempl (toJSON metaplus5)  -- :: Doc Text
        let test_templatehtml = fillTemplate_render testTempl metaplus5 

            -- putInform debug ["panrep2html render html done", "ht1",  ht1 ]
            -- putInform debug ["panrep2html render testTemplate done", "test_templatehtml",  test_templatehtml ]

            -- bakeOnePanrep2html will write to disk
            -- return (HTMLout ht1, [], tt1) -- needs are dealt with so far above
            -- let (p, needsFound3, test_templatehtml) = (HTMLout ht1, [], tt1)
        runErr2action $ do 
            write8 outP htmloutFileType ( HTMLout ht1) 
            write8 outP tthFileType test_templatehtml

        when (inform debug) $
            putIOwords
                ["\n-----------------", "bakeOnePanrep2html done fn", showT outP]

        putInform debug ["\nrule **/*.html end continued 4", showT out]


fillTemplate_render  tpl dat = render (Just 50)
        -- just a plausible line length of 50 
        $  renderTemplate tpl (toJSON dat)
    -- let ttpl1 = renderTemplate testTempl (toJSON metaplus5)  -- :: Doc Text
    --         let test_templatehtml = render (Just 50) ttpl1  -- line length, can 