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

{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports 
            -fno-warn-unused-imports 
            -fno-warn-unused-matches 
            #-}

-- {-# LANGUAGE TypeSynonymInstances  #-}
module Lib.Bake (bakeOneFile2html, bakeOneFile2texsnip, bakeOneFile2pdf) 
                where

import           Uniform.FileStrings            ( ) -- for instances
import           Uniform.Filenames
import Uniform.FileIO (read8)
import           Uniform.Shake (replaceExtension')
import Uniform.Pandoc (writeLatex2, TexSnip, texSnipFileType, extTexSnip)

-- todo - check replaceextension in fileio 
import           Lib.Pandoc ( docValToAllVal
                            , markdownToPandocBiblio
                            , pandocToContentHtml
                            ,  htmloutFileType 
                            , HTMLout (..)
                            )

import           Lib.Templating                 ( putValinMaster )
import Uniform.ProcessPDF (writePDF2text, extPDF, pdfFileType, texFileType,  extTex, Latex)
import           Uniform.Pandoc    ( Pandoc , write8)
import           Lib.CmdLineArgs                ( PubFlags(..) )
import Lib.CheckInput (getTripleDoc)
import Lib.Foundation (SiteLayout(..), templatesDir)


bakeOneFile2html
  :: Bool
  -> PubFlags
  -> Path Abs File  -- ^ md file 
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


bakeOneFile2texsnip
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
    -- TODO needs bib and rest of stuff done for html 
    -- TODO produce docval files 

bakeOneFile2texsnip debug flags inputFn layout texFn2 =
  do
    putIOwords ["\n-----------------", "bakeOneFile2tex 1 fn", showT inputFn, "debug", showT debug]  
    (pandoc, metaRec, report) <- getTripleDoc layout inputFn

    pandoc2 :: Pandoc <- markdownToPandocBiblio debug flags (doughDir layout) (pandoc, metaRec, report) -- AG -> AD
                    -- withSettings.pandoc
                        -- produce html and put into contentHtml key
                        -- can be nothing if the md file is not ready to publish
    when debug $  putIOwords ["\n-----------------", "bakeOneFile2tex 2 fn", showT inputFn ]

    -- here start with texsnip 
   
    texText :: TexSnip <- writeLatex2 pandoc2 

    write8 texFn2 texSnipFileType texText

    when debug $  putIOwords
        ["bakeOneFile2tex resultFile", showT texFn2, "from", showT inputFn, "\n"]
    when debug $ putIOwords
        ["bakeOneFile2tex result TeX", showT texText]--   when debug $ 
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
-- convert a tex file,  form standalone latex and 
-- process with luatex, the result file is not the name
-- of the md file with just replaced the extension 
-- issue: the standalone latex must not have the same name
-- as the tex (body only) file

bakeOneFile2pdf debug flags inputFn  layout pdfFn2 =
  do
    -- let infn = setExtension extTex inputFn
    -- let medfn1 = setExtension extTex inputFn  -- for the standalone file 
    putIOwords ["\n-----------------", "bakeOneFile2pdf 1 inputFn", showT inputFn
    -- , "beomces \n\tinfn", showT infn, "\n\tmedfn1", showT medfn1
            , "debug", showT debug]
            -- inputFn has html extension, same as pdfn2

    -- (pandoc, metaRec, report) <- getTripleDoc layout inputFn

    -- pandoc2 :: Pandoc <- markdownToPandocBiblio debug flags (doughDir layout) (pandoc, metaRec, report) -- AG -> AD
    --                 -- withSettings.pandoc
    --                     -- produce html and put into contentHtml key
    --                     -- can be nothing if the md file is not ready to publish
    -- let texfn = replaceExtension' extTexSnip inputFn :: Path Abs File
    putIOwords ["bakeOneFile2pdf texfn" , showT inputFn]
   
    writePDF2text debug inputFn -- takes texsnip extension, produces pdf extension

    when debug $  putIOwords
        ["bakeOneFile2pdf resultFile   pdf", showT inputFn ]
     
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

--     --- the preamble and the end -- escape \
-- pre1 = ["%%% eval: (setenv \"LANG\" \"en_US.UTF-8\")"
--         , "\\documentclass[a4paper,10pt]{scrbook}"
--         , "\\usepackage[german]{babel}"
--         -- necessary for the pandoc produced TeX files: 
--         , "\\usepackage[colorlinks]{hyperref}" 
--         , "\\newenvironment{cslreferences}{}{\\par}"
--         , "\\begin{document}"] :: [Text]

-- end9    = ["\\end{document}"]
