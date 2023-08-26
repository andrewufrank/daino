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
-- import Wave.Panrep2pdf

import Uniform.Http
import Uniform.Shake
import Wave.Panrep2html  
-- import Development.Shake (getDirectoryContents)


type BakeOp =
    NoticeLevel ->
    PubFlags ->
    -- | md file
    Path Abs File ->
    Settings->
    Path Abs File ->
    ErrIO [FilePath] -- additional needs found 

 

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
 

    (p, needsFound, test_templatehtml) <- panrep2html debug  flags dr1

    write8 resfn2 htmloutFileType ( p) -- content is html style
    write8 resfn2 tthFileType test_templatehtml
    -- write the test for filling the template always 
    when (inform debug) $
        putIOwords
            ["\n-----------------", "bakeOnePanrep2html done fn", showT resfn2]
    return  needsFound

-- getNeeds4pan :: BakeOp
-- getNeeds4pan debug flags bakedFrom sett3 outP = do 
--     when (inform debug) $    putIOwords
--         [ "\n-----getNeeds4pan 1", showT outP
--         , "\n   bakedFrom docrep ", showT bakedFrom
--         ]

--     return [toFilePath bakedFrom]

-- bakeOneDocrep2panrep :: BakeOp --  DOCREP -> PANREP
-- --   add index  
-- bakeOneDocrep2panrep debug flags inputFn sett3 resfn2 = do
--     -- let debug = NoticeLevel2
--     when (informAll debug) $    putIOwords
--         [ "-----------------"
--         , "bakeOneDocrep2panrep 1 inputFn"
--         , showT inputFn
--         , showT resfn2
--         ]
--     dr1 <- read8 inputFn docrepFileType

--     -- let layout = siteLayout sett3
--     (p3, needsFound) <- docrep2panrep debug flags  dr1
--             -- completes index and should process reps 
--             -- what to do with needs?


--     write8 resfn2 panrepFileType p3 -- content is html style
--     putInform NoticeLevel1 
--             ["\n-----------------", "bakeOneDocrep2panrep done produced resf2n", showT resfn2
--                 , "\n needsFound", showT needsFound]
--     return   needsFound
        -- these needs were not tested for version >= publish

-- getNeeds4doc :: BakeOp
-- getNeeds4doc debug flags bakedFrom sett3 outP = do 
--     when (inform debug) $    putIOwords
--         [ "\n-----"
--         , "getNeeds4doc 1"
--         , showT outP
--         , "\n   bakedFrom"
--         , showT bakedFrom
--         ]

--     return [toFilePath bakedFrom]

-- bakeOneMD2docrep :: BakeOp --    MD -> DOCREP
-- -- process the md to pandoc format (parser)
-- -- and add the refs 
-- bakeOneMD2docrep debug flags inputFn sett3 resfn2 = do
--     when (informAll debug) $    putIOwords
--         [ "\n-----------------"
--         , "bakeOneMD2docrep 1 fn", showT inputFn
--         -- , "\n resfn2", showT resfn2
--         ]
--     -- let layout = siteLayout sett3
--     -- let doughP = doughDir layout
--     -- let hpname = blogAuthorToSuppress . siteLayout $ sett3
--     dr4 <- readMarkdownFile2docrep NoticeLevel0 flags sett3  inputFn 
--     -- dr4 <- addRefs debug dr3
 
--     write8 resfn2 docrepFileType dr4



--     when (inform debug) $
--         putIOwords
--             [ "\n-----------------"
--             , "bakeOneMD2docrep done resfn2"
--             , showT resfn2
--             ]
--     return []


