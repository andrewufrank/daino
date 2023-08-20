---------------------------------------------------------------------
--
-- Module      :
----------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-unused-matches #-}

{- |  process to convert
              files from md to all the formats required
              orginals are found in dire doughDir and go to bakeDir
-}
module ShakeBake.Bake where

import Foundational.SettingsPage  
import Foundational.CmdLineFlags

import Foundational.Filetypes4sites

import Wave.Docrep2panrep 
import Wave.Md2doc  
import Wave.Panrep2pdf

import Uniform.Http
import Uniform.Shake
import Wave.Panrep2html  
import UniformBase
-- import ShakeBake.Shake2 (needPwithoutput)
-- import Lib.Template_test (test_templatehtml)


type BakeOp =
    NoticeLevel ->
    PubFlags ->
    -- | md file
    Path Abs File ->
    Settings->
    Path Abs File ->
    ErrIO [FilePath] -- additional needs found 

bakeOneMD2docrep :: BakeOp --    MD -> DOCREP
-- process the md to pandoc format (parser)
-- and add the refs 
bakeOneMD2docrep debug flags inputFn sett3 resfn2 = do
    when (inform debug) $    putIOwords
        [ "\n-----------------"
        , "bakeOneMD2docrep 1 fn", showT inputFn
        , "\n resfn2", showT resfn2
        ]
    -- let layout = siteLayout sett3
    -- let doughP = doughDir layout
    -- let hpname = blogAuthorToSuppress . siteLayout $ sett3
    dr4 <- readMarkdownFile2docrep debug sett3  inputFn 
    -- dr4 <- addRefs debug dr3

    write8 resfn2 docrepFileType dr4

    when (inform debug) $
        putIOwords
            [ "\n-----------------"
            , "bakeOneMD2docrep done resfn2"
            , showT resfn2
            ]
    return []

bakeOneDocrep2panrep :: BakeOp --  DOCREP -> PANREP
--   add index  
bakeOneDocrep2panrep debug flags inputFn sett3 resfn2 = do
    -- let debug = NoticeLevel2
    when (inform debug) $    putIOwords
        [ "-----------------"
        , "bakeOneDocrep2panrep 1 inputFn"
        , showT inputFn
        , showT resfn2
        ]
    dr1 <- read8 inputFn docrepFileType

    -- let layout = siteLayout sett3
    (p3, needsFound) <- docrep2panrep debug flags  dr1
            -- completes index and should process reps 
            -- what to do with needs?
    -- needP  needsFound 
    -- let needsFound2 =  map makeAbsFile needsFound :: [Path Abs File]
    -- needsChecked :: [Maybe (Path Abs File)] <- mapM (filterNeeds2 debug flags sett3) needsFound2 
    -- let needsChecked2 = catMaybes needsChecked

    write8 resfn2 panrepFileType p3 -- content is html style
    putInform NoticeLevel2 
            ["\n-----------------", "bakeOneDocrep2panrep done produced resf2n", showT resfn2
                , "\n needsFound", showT needsFound]
    return   needsFound
        -- these needs were not tested for version >= publish

bakeOnePanrep2html :: BakeOp -- PANREP -> HTML  -- TODO
bakeOnePanrep2html debug flags inputFn sett3 resfn2 = do
    when (inform debug) $    putIOwords
        [ "\n-----------------"
        , "bakeOnePanrep2html 1 fn"
        , showT inputFn
        , "\n resfn2"
        , showT resfn2
        ]
    dr1 <- read8 inputFn panrepFileType
    -- let layout = siteLayout sett3
    -- this gives the siteLayout section of settingsN.yml file
    -- let staticMenu = sett3
    -- let mf = masterTemplateFile layout
    -- let masterfn = templatesDir layout </> mf

    (p, needsFound, test_templatehtml) <- panrep2html debug  flags dr1

    write8 resfn2 htmloutFileType ( p) -- content is html style
    write8 resfn2 tthFileType test_templatehtml
    -- write the test for filling the template always 
    when (inform debug) $
        putIOwords
            ["\n-----------------", "bakeOnePanrep2html done fn", showT resfn2]
    return  needsFound


bakeOnePanrep2texsnip :: BakeOp --  PANREP -> TEXSNIP
-- TODO
bakeOnePanrep2texsnip debug flags inputFn sett3 resfn2 = do
                -- debug flags inputFn layout resfn2 
    when (inform debug) $    putIOwords
        [ "\n-----------------"
        , "bakeOnePanrep2texsnip 1 fn"
        , showT inputFn
        , "debug"
        , showT debug
        , "\n resfn2"
        , showT resfn2
        ]

    dr1 <- read8 inputFn panrepFileType
    snip1 <- panrep2texsnip debug dr1
    write8 resfn2 texSnipFileType snip1 -- content is html style
    when (inform debug) $
        putIOwords
            ["\n-----------------", "bakeOneFile2html done fn", showT resfn2]
    return []

bakeOneTexsnip2tex :: BakeOp -- TEXSNIP -> TEX
bakeOneTexsnip2tex debug flags inputFn sett3 resfn2 = do
    when (inform debug) $   putIOwords
        [ "\n-----------------"
        , "bakeOneFile2tex 1 fn"
        , showT inputFn
        , "\n resfn2"
        , showT resfn2
        ]


    snip1 <- read8 inputFn texSnipFileType

    -- let layout = siteLayout sett3
    -- let doughP = doughDir layout
    --     bakedP = bakedDir layout 

    (tex1, needs, test_templatehtml) <- texsnip2tex debug  snip1

    -- tex1 <- texsnip2tex NoticeLevel0 doughP bakedP snip1 
        -- ((templatesDir layout) </> (texTemplateFile layout))
    -- let tex1 = tex2latex2 zero [snip1]
    write8 resfn2 texFileType tex1 -- content is html style
    write8 resfn2 ttlFileType test_templatehtml

    when (inform debug) $
        putIOwords
            ["\n-----------------", "bakeOneFile2tex done fn", showT resfn2]
    return []

bakeOneTex2pdf :: BakeOp
bakeOneTex2pdf debug flags inputFn sett3 resfn2 = do
    when (inform debug) $    putIOwords
        [ "\n-----------------"
        , "bakeOneTex2pdf 1 fn:"
        , showT inputFn
        , "\n\t debug:"
        , showT debug
        , "\n\t resfn2:"
        , showT resfn2
        ]

    -- let refDir =
            -- makeAbsDir . getParentDir . toFilePath $ inputFn :: Path Abs Dir
    -- dr1 <- read8 inputFn docrepFileType
    let layout = siteLayout sett3
    let doughP = doughDir layout

    tex2pdf debug  inputFn resfn2 doughP -- content is html style
    when (inform debug) $
        putIOwords
            ["\n-----------------", "bakeOneTex2pdf done fn", showT resfn2]
    return []
