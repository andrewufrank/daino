------------------------------------------------------------------------------
--
-- Module      :   the  process to convert
--              files in any input format to html
--              orginals are found in dire doughDir and go to bakeDir
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- {-# LANGUAGE TypeSynonymInstances  #-}
module Lib.Bake (bakeOneFile2html, bakeOneFile2tex) 
  --  (openMain, htf_thisModuelsTests)
                where

import           Uniform.FileStrings            ( ) -- for instances
import           Uniform.Filenames
import           Lib.Pandoc ( docValToAllVal
                                                , markdownToPandocBiblio
                                                , pandocToContentHtml
                                                -- , MenuEntry(..)
                                                )-- with a simplified Action ~ ErrIO

import           Lib.Templating                 ( putValinMaster )
import           Uniform.Pandoc                 ( Pandoc
                                                , htmloutFileType
                                                , write8
                                                , HTMLout (..)
                                                , extTex
                                                , writeLatex2text
                                                , texFileType
                                                )
import           Lib.CmdLineArgs                ( PubFlags(..) )
import Lib.CheckInput (getTripleDoc)
import Lib.Foundation (SiteLayout(..), templatesDir)


bakeOneFile2html
  :: Bool
  -> PubFlags
  -> Path Abs File
  -> SiteLayout
  -> Path Abs File
  -> ErrIO Text
-- files exist
-- convert a file md2, process citations if any
-- separate html content and put in contentHtml
-- get pageType, read file and process
--test in bake_tests:
bakeOneFile2html debug flags pageFn layout ht2 =
  do
    putIOwords ["\n-----------------", "bakeOneFile2html 1 fn", showT pageFn, "debug", showT debug]
    (pandoc, metaRec, report) <- getTripleDoc layout pageFn
    -- how are errors dealt with 
    -- let debug = True

    pandoc2 :: Pandoc <- markdownToPandocBiblio debug flags (doughDir layout) (pandoc, metaRec, report) -- AG -> AD
                    -- withSettings.pandoc
                        -- produce html and put into contentHtml key
                        -- can be nothing if the md file is not ready to publish
    when debug $  putIOwords ["\n-----------------", "bakeOneFile2html 2 fn", showT pageFn ]

    htmlout :: HTMLout <- pandocToContentHtml debug pandoc2 -- content.docval  AD -> AF

    when debug $  putIOwords ["\n-----------------", "bakeOneFile2html 3 fn", showT pageFn ]

    val    <- docValToAllVal debug layout flags htmlout  metaRec
    -- includes the directory list and injection, which should be in 
    -- value "menu2"
    html2  <- putValinMaster debug val (templatesDir layout)
    write8 ht2 htmloutFileType html2

    when debug $  putIOwords
        ["bakeOneFile2html resultFile", showT ht2, "from", showT pageFn, "\n"]
    when debug $ putIOwords
        ["bakeOneFile2html resultvalue", showT val, "\n"
            , showT html2]--   when debug $ 
    putIOwords ["......................"]
    return . unwords' $ ["bakeOneFile2html outhtml ", showT pageFn, "done"]

  `catchError` 
    (\e -> 
        do
            let errmsg2 =
                    [ "\n****************"
                    , "bakeOneFile catchError"
                    , showT e
                    , "for "
                    , showT pageFn
                    , "\n****************"
                    ]
            putIOwords errmsg2
            return . unwords' $ errmsg2
            )


bakeOneFile2tex
  :: Bool
  -> PubFlags
  -> Path Abs File
  -> SiteLayout
  -> Path Abs File
  -> ErrIO Text
-- files exist
-- convert a file md2, process citations if any
-- produce latex raw file (no standalone)

    -- TODO remove duplication in code 

bakeOneFile2tex debug flags pageFn layout texFn2 =
  do
    putIOwords ["\n-----------------", "bakeOneFile2tex 1 fn", showT pageFn, "debug", showT debug]
    (pandoc, metaRec, report) <- getTripleDoc layout pageFn
    -- how are errors dealt with 
    -- let debug = True

    pandoc2 :: Pandoc <- markdownToPandocBiblio debug flags (doughDir layout) (pandoc, metaRec, report) -- AG -> AD
                    -- withSettings.pandoc
                        -- produce html and put into contentHtml key
                        -- can be nothing if the md file is not ready to publish
    when debug $  putIOwords ["\n-----------------", "bakeOneFile2tex 2 fn", showT pageFn ]

    -- here start with tex 
   
    texText :: Text <- writeLatex2text pandoc2 


    -- htmlout :: HTMLout <- pandocToContentHtml debug pandoc2 -- content.docval  AD -> AF

    -- when debug $  putIOwords ["\n-----------------", "bakeOneFile2tex 3 fn", showT pageFn ]

    -- val    <- docValToAllVal debug layout flags htmlout  metaRec
    -- -- includes the directory list and injection, which should be in 
    -- -- value "menu2"
    -- html2  <- putValinMaster debug val (templatesDir layout)
    write8 texFn2 texFileType texText

    when debug $  putIOwords
        ["bakeOneFile2tex resultFile", showT texFn2, "from", showT pageFn, "\n"]
    when debug $ putIOwords
        ["bakeOneFile2tex result TeX", texText]--   when debug $ 
    putIOwords ["......................"]
    return . unwords' $ ["bakeOneFile2tex tetfn ", showT pageFn, "done"]

  `catchError` 
    (\e -> 
        do
            let errmsg2 =
                    [ "\n****************"
                    , "bakeOneFile2tex catchError"
                    , showT e
                    , "for "
                    , showT pageFn
                    , "\n****************"
                    ]
            putIOwords errmsg2
            return . unwords' $ errmsg2
            )

