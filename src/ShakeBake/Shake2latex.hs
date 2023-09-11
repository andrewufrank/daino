----------------------------------------------------------------------
--
-- Module Shake2 latex :
----------------------------------------------------------------------
{- produce the texsnip and tex files 
    texsnip are probably not used at all 
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


module ShakeBake.Shake2latex where

import UniformBase 
import Foundational.CmdLineFlags
import Uniform.Shake
import Uniform.TemplateStuff
-- import Uniform.Http 
import Foundational.SettingsPage
import Foundational.Filetypes4sites

import ShakeBake.Shake2aux
import Wave.Panrep2html
    -- ( getIndexFiles4meta, getVals2html, testTemplateFn )  

latexTestTemplateFN = makeAbsFile 
    "/home/frank/Workspace11/daino/theme/templates/test63latex.dtpl"
    -- "/home/frank/Workspace11/daino/theme/templates/latexDaino63.dtpl"
    
shake2latex :: NoticeLevel -> PubFlags -> Settings ->
     Path Abs Dir -> Rules ()
shake2latex debug flags sett4 bakedP  =    
    (toFilePath bakedP <> "**/*.tex") %> \out -> do  -- from Panrep
    -- calls the copy html if a html exist in dough 
        -- not covered, but can remain for later
            -- pushes a need for *.pandoc 

    let layout = siteLayout sett4
        doughP = doughDir layout -- the regular dough

    putInform debug ["rule **/*.tex 1 start ", showT out]

    let outP = makeAbsFile out :: Path Abs File
    let fromFile = doughP </> makeRelativeP bakedP outP
            -- try to see if the file exists in dough 
    fileExists <- io2bool $ doesFileExist' fromFile
    putInform debug ["rule **/*.tex 2 - fileExist:", showT fileExists]
    
    if fileExists 
        then copyFileToBaked debug doughP bakedP out
        else do
        let bakedFrom = replaceExtension'  "panrep" outP
        putInform debug ["rule **/*.tex 3 - bakedFrom", showT bakedFrom, "\n"]
        need [toFilePath bakedFrom]

        putInform debug ["\nrule **/*.tex 4 continued out" , showT out
                , "bakedFrom", showT bakedFrom]

        needsFound :: [Path Abs File]<- runErr2action $ do
            pan0 <- read8 bakedFrom panrepFileType
            let needsFound1 = map (addFileName bakedP . replaceExtension' "html") . getIndexFiles4meta $ pan0 
            putInform debug ["\nrule **/*.tex 5 needsFound1" 
                    , showT needsFound1]
            return needsFound1
        needP needsFound

        putInform debug ["\nrule **/*.tex 6 continued ", showT out]

            -- was ist mit needs2?
            --  bakeOnePanrep2html debug flags bakedFrom sett4 outP 
            -- bakeOnePanrep2html debug flags inputFn sett3 resfn2 = do
        putInform debug [ "\n-----\nrule **/*.tex 7 fn", showT bakedFrom
                , "\n                    resfn2", showT outP
                ]
        (pan0) <- runErr2action $  read8 bakedFrom panrepFileType
        
        putInform debug ["\nrule **/*.tex 7b panrep read " ]

        let sett3 = sett pan0
            -- extra4 = extra pan0
            mf = texTemplateFile $ siteLayout sett3
            masterfn = templatesDir (siteLayout sett3) </> mf

    -- braucht needs fuer die panrep files
        let ixs0 = getIndexFiles4meta pan0 :: [Path Rel File]
        let ixs0pan = map (addFileName bakedP 
                        . addExtension extPanrep) ixs0 ::[Path Abs File]
        putInform debug ["\n-----\nrule **/*.tex 8"
                    , "needs panrep ixs0pan", showT ixs0pan]
        
        needP ixs0pan

        putInform debug["\n-----rule **/*.tex 9 siteLayout sett3"
                    , showT $ siteLayout sett3]
        putInform debug ["-----rule **/*.tex 10 mf", showT mf]
        putInform debug ["-----rule **/*.tex 11 masterfn"
                        , showT masterfn]

        targetTempl  <- runErr2action $ compileTemplateFile2 masterfn
        testTempl <- runErr2action $ compileTemplateFile2 latexTestTemplateFN

            --if this is an index file it has files and dirs 
            -- putInform debug ["panrep2html", "extra4", showT extra4]

    -- the indexentries are produced in panrep
        -- let files = fileEntries  $ extra4 :: [IndexEntry2]
        --     dirs = dirEntries  $ extra4 :: [IndexEntry2]

        -- valsDirs :: [Maybe IndexEntry2]<- runErr2action $ mapM 
        --                     (getVals2html debug flags bakedP) dirs
        -- valsFiles :: [Maybe IndexEntry2] <- runErr2action $ mapM 
        --                 (getVals2html debug flags bakedP) files
        -- -- will require one level of recursion more 

        -- putInform debug["\n-----rule **/*.tex 12 valsDirs"
        --             , showT valsDirs]
        -- putInform debug ["\n-----rule **/*.tex 13 valsFiles", showT valsFiles]

        -- let extra5 = extra4{fileEntries = catMaybes valsFiles
        --                     , dirEntries = catMaybes valsDirs
        --                     }
        let metaplus5 = pan0 -- {extra = extra5}

        -- putInform debug ["panrep2html", "extra5", showT extra5]
        -- putInform debug ["panrep2html", "metaplus5", showT metaplus5]

        let ht1 = fillTemplate_render targetTempl metaplus5
        let test_template = fillTemplate_render testTempl metaplus5 

        -- putInform debug ["panrep2html render html done", "ht1",  ht1 ]
        -- putInform debug ["panrep2html render testTemplate done", "test_template",  test_template ]

        runErr2action $ do 
            unless (quickFlag flags) $ 
                write8 outP texFileType ( Latex ht1) 
            write8 outP ttlFileType test_template

        putInform debug 
            ["-----rule **/*.tex 14  done fn"
            , showT outP]



        putInform debug ["\n-----rule **/*.tex 15 end continued 4"
            , showT out,"\n"]


fillTemplate_render  tpl dat = render (Just 50)
        -- just a plausible line length of 50 
        $  renderTemplate tpl (toJSON dat)
 