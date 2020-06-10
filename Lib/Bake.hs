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
module Lib.Bake (bakeOneFile2html, bakeOneFile2tex, bakeOneFile2pdf) 
  --  (openMain, htf_thisModuelsTests)
                where

import           Uniform.FileStrings            ( ) -- for instances
import           Uniform.Filenames
import Uniform.FileIO (read8)
import           Uniform.Shake (replaceExtension')
-- todo - check replaceextension in fileio 
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
                                                , pdfFileType, extPDF
                                                , writePDF2text
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
bakeOneFile2html debug flags inputFn layout ht2 =
  do
    putIOwords ["\n-----------------", "bakeOneFile2html 1 fn", showT inputFn, "debug", showT debug]
    (pandoc, metaRec, report) <- getTripleDoc layout inputFn
    -- how are errors dealt with 
    -- let debug = True

    pandoc2 :: Pandoc <- markdownToPandocBiblio debug flags (doughDir layout) (pandoc, metaRec, report) -- AG -> AD
                    -- withSettings.pandoc
                        -- produce html and put into contentHtml key
                        -- can be nothing if the md file is not ready to publish
    when debug $  putIOwords ["\n-----------------", "bakeOneFile2html 2 fn", showT inputFn ]

    htmlout :: HTMLout <- pandocToContentHtml debug pandoc2 -- content.docval  AD -> AF

    when debug $  putIOwords ["\n-----------------", "bakeOneFile2html 3 fn", showT inputFn ]

    val    <- docValToAllVal debug layout flags htmlout  metaRec
    -- includes the directory list and injection, which should be in 
    -- value "menu2"
    html2  <- putValinMaster False val (templatesDir layout)
    write8 ht2 htmloutFileType html2

    when debug $  putIOwords
        ["bakeOneFile2html resultFile", showT ht2, "from", showT inputFn, "\n"]
    when debug $ putIOwords
        ["bakeOneFile2html resultvalue", take' 300 $ showT val, "\n"
            , take' 300 $ showT html2]--   when debug $ 
    putIOwords ["......................"]
    return . unwords' $ ["bakeOneFile2html outhtml ", take' 300 $ showT inputFn, "done"]

  `catchError` 
    (\e -> 
        do
            let errmsg2 =
                    [ "\n****************"
                    , "bakeOneFile2html catchError"
                    , "\nfor "
                    , showT inputFn
                    , "\n"
                    , take' 300 . showT $ e
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

bakeOneFile2tex debug flags inputFn layout texFn2 =
  do
    putIOwords ["\n-----------------", "bakeOneFile2tex 1 fn", showT inputFn, "debug", showT debug]  
    (pandoc, metaRec, report) <- getTripleDoc layout inputFn
    -- how are errors dealt with 
    -- let debug = True

    pandoc2 :: Pandoc <- markdownToPandocBiblio debug flags (doughDir layout) (pandoc, metaRec, report) -- AG -> AD
                    -- withSettings.pandoc
                        -- produce html and put into contentHtml key
                        -- can be nothing if the md file is not ready to publish
    when debug $  putIOwords ["\n-----------------", "bakeOneFile2tex 2 fn", showT inputFn ]

    -- here start with tex 
   
    texText :: Text <- writeLatex2text pandoc2 

    write8 texFn2 texFileType texText

    when debug $  putIOwords
        ["bakeOneFile2tex resultFile", showT texFn2, "from", showT inputFn, "\n"]
    when debug $ putIOwords
        ["bakeOneFile2tex result TeX", texText]--   when debug $ 
    putIOwords ["......................"]
    return . unwords' $ ["bakeOneFile2tex tetfn ", showT inputFn, "done"]

  `catchError` 
    (\e -> 
        do
            let errmsg2 =
                    [ "\n****************"
                    , "bakeOneFile2tex catchError"
                    , "\nfor "
                    , showT inputFn
                    , "\n"
                    , take' 300 . showT $ e
                    , "\n****************"
                    ]
            putIOwords errmsg2
            return . unwords' $ errmsg2
            )

bakeOneFile2pdf
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

bakeOneFile2pdf debug flags inputFn layout pdfFn2 =
  do
    putIOwords ["\n-----------------", "bakeOneFile2pdf 1 fn", showT inputFn, "beomces pdfFn2", showT pdfFn2, "debug", showT debug]
            -- inputFn has html extension, same as pdfn2

            --  Path Abs File /home/frank/Workspace8/ssg/docs/site/baked/index.html pdfFn Path Abs File /home/frank/Workspace8/ssg/docs/site/baked/index.html debug True

    -- (pandoc, metaRec, report) <- getTripleDoc layout inputFn
    -- how are errors dealt with 
    -- let debug = True

    -- pandoc2 :: Pandoc <- markdownToPandocBiblio debug flags (doughDir layout) (pandoc, metaRec, report) -- AG -> AD
    --                 -- withSettings.pandoc
    --                     -- produce html and put into contentHtml key
    --                     -- can be nothing if the md file is not ready to publish
    let texfn = replaceExtension' "tex" inputFn :: Path Abs File
    putIOwords ["bakeOneFile2pdf texfn" , showT texfn]
    inputTex <- read8 texfn texFileType

    when debug $  putIOwords ["\n-----------------", "bakeOneFile2pdf 2 fn", showT texfn 
        , "the tex file\n"
        , take' 200 $ inputTex]

    -- here start with tex, this path is for regular files,
    --      booklets later
   
    writePDF2text debug inputTex pdfFn2
    -- converts the text into latex, stored in the file pdfFn2

    -- write8 pdfFn2 pdfFileType respdf   -- assume the correct 
    -- write8 pdfFn2 pdfFileType pdfText

    when debug $  putIOwords
        ["bakeOneFile2pdf resultFile", showT pdfFn2, "from", showT inputFn, "\n"
        ]
     
    putIOwords ["......................"]
    return . unwords' $ ["bakeOneFile2pdf pdfText ", showT inputFn, "done"]

  `catchError` 
    (\e -> 
        do
            let errmsg2 =
                    [ "\n****************"
                    , "bakeOneFile2pdf catchError"
                    , "\nfor "
                    , showT inputFn
                    , "\n"
                    , take' 300 . showT $ e
                    , "\n****************"
                    ]
            putIOwords errmsg2
            return . unwords' $ errmsg2
            )
