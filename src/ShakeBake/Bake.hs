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

import Lib.CmdLineArgs (PubFlags (..))
import Foundational.Foundation  
-- import Wave.Docrep 
import Wave.Md2doc

import Foundational.Filetypes4sites
   
-- import Uniform2.Markdown  
import Uniform.Pandoc
import Uniform2.ProcessPDF  
import Uniform2.HTMLout

import Wave.Doc2html  
import UniformBase

type BakeOp =
    NoticeLevel ->
    PubFlags ->
    -- | md file
    Path Abs File ->
    SiteLayout ->
    Path Abs File ->
    ErrIO ()

bakeOneMD2docrep :: BakeOp --    MD -> DOCREP
bakeOneMD2docrep debug flags inputFn layout resfn2 = do
    when (inform debug) $    putIOwords
        [ "\n-----------------"
        , "bakeOneMD2docrep 1 fn"
        , showT inputFn
        , "debug"
        , showT debug
        , "\n resfn2"
        , showT resfn2
        ]

    md1 <- read8 inputFn markdownFileType
    dr3 <- md2docrep debug layout inputFn md1

    write8 resfn2 docrepFileType dr3
    when (informall debug) $
        putIOwords
            [ "\n-----------------"
            , "bakeOneMD2docrep done fn"
            , showT resfn2
            ]
    return ()

bakeOneDocrep2panrep :: BakeOp --  DOCREP -> PANREP
-- change to metaPage and add index data 
bakeOneDocrep2panrep debug flags inputFn layout resfn2 = do
    when (inform debug) $    putIOwords
        [ "\n-----------------"
        , "bakeOneDocrep2panrep 1 fn"
        , showT inputFn
        , "debug"
        , showT debug
        , "\n resfn2"
        , showT resfn2
        ]
    dr1 <- read8 inputFn docrepFileType
    p3 <- docrep2panrep debug layout dr1

    write8 resfn2 panrepFileType p3 -- content is html style
    when (informall debug) $
        putIOwords
            ["\n-----------------", "bakeOneDocrep2panrep done fn", showT resfn2]
    return ()

bakeOnePanrep2html :: BakeOp -- PANREP -> HTML  -- TODO
bakeOnePanrep2html debug flags inputFn layout resfn2 = do
    when (inform debug) $    putIOwords
        [ "\n-----------------"
        , "bakeOnePanrep2html 1 fn"
        , showT inputFn
        , "debug"
        , showT debug
        , "\n resfn2"
        , showT resfn2
        ]
    dr1 <- read8 inputFn panrepFileType
    p <- panrep2html debug layout dr1
    write8 resfn2 htmloutFileType p -- content is html style
    when (informall debug) $
        putIOwords
            ["\n-----------------", "bakeOnePanrep2html done fn", showT resfn2]
    return ()

-- bakeOnePanrep2panrep1 :: BakeOp -- PANREP -> Panrep1  -- TODO
-- -- split in panrep -> panrep1
-- bakeOnePanrep2panrep1 debug flags inputFn layout resfn2 = do
--     dr1 <- read8 inputFn panrepFileType
--     p :: Panrep1 <- panrep2panrep1 debug layout dr1
--     write8 resfn2 panrep1FileType p  
--     when (informall debug) $
--         putIOwords
--             ["\n-----------------", "bakeOnePanrep2panrep1 done fn", showT resfn2]
--     return ()

-- bakeOnePanrep12html :: BakeOp -- PANREP -> Panrep1  -- TODO
-- -- split in panrep -> panrep1
-- bakeOnePanrep12html debug flags inputFn layout resfn2 = do
--     dr1 <- read8 inputFn panrep1FileType
--     p :: HTMLout <- panrep12html debug layout dr1
--     write8 resfn2 htmloutFileType p  
--     when (informall debug) $
--         putIOwords
--             ["\n-----------------", "bakeOnePanrep12html done fn", showT resfn2]
--     return ()

bakeOnePanrep2texsnip :: BakeOp --  PANREP -> TEXSNIP
-- TODO
bakeOnePanrep2texsnip debug flags inputFn layout resfn2 = do
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
    snip1 <- panrep2texsnip dr1
    write8 resfn2 texSnipFileType snip1 -- content is html style
    when (informall debug) $
        putIOwords
            ["\n-----------------", "bakeOneFile2html done fn", showT resfn2]
    return ()

bakeOneTexsnip2tex :: BakeOp -- TEXSNIP -> TEX
bakeOneTexsnip2tex debug flags inputFn layout resfn2 = do
    when (inform debug) $   putIOwords
        [ "\n-----------------"
        , "bakeOneFile2tex 1 fn"
        , showT inputFn
        , "debug"
        , showT debug
        , "\n resfn2"
        , showT resfn2
        ]

    snip1 <- read8 inputFn texSnipFileType
    let tex1 = tex2latex2 zero [snip1]
    write8 resfn2 texFileType tex1 -- content is html style
    when (informall debug) $
        putIOwords
            ["\n-----------------", "bakeOneFile2tex done fn", showT resfn2]
    return ()

bakeOneTex2pdf :: BakeOp
bakeOneTex2pdf debug flags inputFn layout resfn2 = do
    when (inform debug) $    putIOwords
        [ "\n-----------------"
        , "bakeOneFile2pdf 1 fn:"
        , showT inputFn
        , "\n\t debug:"
        , showT debug
        , "\n\t resfn2:"
        , showT resfn2
        ]

    let refDir =
            makeAbsDir . getParentDir . toFilePath $ inputFn :: Path Abs Dir
    writePDF1 debug inputFn resfn2 refDir -- content is html style
    when (informall debug) $
        putIOwords
            ["\n-----------------", "bakeOneFile2pdf done fn", showT resfn2]
    return ()
