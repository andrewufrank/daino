---------------------------------------------------------------------------
--
-- Module      :   the  process to convert
--              files from md to all the formats required 
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
module Lib.Bake 
    (module Lib.Bake
    , bakeOneFile2docval
    -- , bakeOneFile2html
    , bakeOneFile2texsnip, bakeOneTexSnip2pdf
    , bakeDocValue2html
    ) 
                where

import           Uniform.FileStrings            ( ) -- for instances
import           Uniform.Filenames
import Uniform.FileIO (read8, write8, copyOneFileOver)
import           Uniform.Shake (replaceExtension')
import Uniform.Pandoc (writeTexSnip2, TexSnip, texSnipFileType, extTexSnip)
import Uniform.DocValue (docValueFileType, docvalExt)
-- todo - check replaceextension in fileio 
import           Lib.Pandoc ( docValToAllVal
                            , markdownToPandocBiblio
                            , pandocToContentHtml
                            ,  htmloutFileType 
                            , HTMLout (..)
                            )

import           Lib.Templating                 ( putValinMaster )
import Uniform.ProcessPDF (writePDF2text, extPDF, pdfFileType, texFileType,  extTex, Latex(..),tex2latex)
import           Uniform.Pandoc    ( Pandoc , write8)
import           Lib.CmdLineArgs                ( PubFlags(..) )
import Lib.CheckInput (getTripleDoc)
import Lib.Foundation (SiteLayout(..), templatesDir)
import qualified Path.IO  as Path (getTempDir)

bakeOneFile2docval
  :: Bool
  -> PubFlags
  -> Path Abs File  -- ^ md file 
  -> SiteLayout
  -> Path Abs File
  -> ErrIO ()
-- files exist
-- convert a md file, process citations if any
-- produce the docval (from which html texsnip are derived)
-- 
 
bakeOneFile2docval debug flags inputFn layout resfn2 =
  do
    putIOwords ["\n-----------------", "bakeOneFile2docval 1 fn", showT inputFn, "debug", showT debug
        , "\n resfn2", showT resfn2]
    (pandoc, metaRec, report) <- getTripleDoc layout inputFn
    -- how are errors dealt with 
    -- let debug = True

    pandoc2 :: Pandoc <- markdownToPandocBiblio debug flags (doughDir layout) (pandoc, metaRec, report) -- AG -> AD
                    -- withSettings.pandoc
                        -- produce html and put into contentHtml key
                        -- can be nothing if the md file is not ready to publish
    when debug $  putIOwords ["\n-----------------", "bakeOneFile2docval 2 fn", showT inputFn ]

    htmlout :: HTMLout <- pandocToContentHtml debug pandoc2 -- content.docval  AD -> AF

    when debug $  putIOwords ["\n-----------------", "bakeOneFile2docval 3 fn", showT inputFn ]

    val    <- docValToAllVal debug layout flags htmlout  metaRec
    -- why from htmlout as a base? 
    -- includes the directory list and injection, which should be in 
    -- value "menu2"
    when debug $  putIOwords ["\n-----------------", "bakeOneFile2docval 4 fn", showT inputFn ]

    write8 resfn2 docValueFileType val   -- content is html style

    when debug $  putIOwords ["\n-----------------", "bakeOneFile2docval done fn", showT resfn2 ]
    return () -- "ok bakeOneFile2docval"




-- bakeOneFile2html
--   :: Bool
--   -> PubFlags
--   -> Path Abs File  -- ^ md file 
--   -> SiteLayout
--   -> Path Abs File
--   -> ErrIO ()
-- -- files exist
-- -- convert a file md2, process citations if any
-- -- separate html content and put in contentHtml
-- -- get pageType, read file and process
-- --test in bake_tests:
-- bakeOneFile2html debug flags inputFn layout ht2 =
--   do
--     putIOwords ["\n-----------------", "bakeOneFile2html 1 fn", showT inputFn, "debug", showT debug]
--     (pandoc, metaRec, report) <- getTripleDoc layout inputFn
--     -- how are errors dealt with 
--     -- let debug = True

--     pandoc2 :: Pandoc <- markdownToPandocBiblio debug flags (doughDir layout) (pandoc, metaRec, report) -- AG -> AD
--                     -- withSettings.pandoc
--                         -- produce html and put into contentHtml key
--                         -- can be nothing if the md file is not ready to publish
--     when debug $  putIOwords ["\n-----------------", "bakeOneFile2html 2 fn", showT inputFn ]

--     htmlout :: HTMLout <- pandocToContentHtml debug pandoc2 -- content.docval  AD -> AF

--     when debug $  putIOwords ["\n-----------------", "bakeOneFile2html 3 fn", showT inputFn ]

--     val    <- docValToAllVal debug layout flags htmlout  metaRec
--     -- includes the directory list and injection, which should be in 
--     -- value "menu2"
--     html2  <- putValinMaster False val (templatesDir layout)
--     write8 ht2 htmloutFileType html2

--     when debug $  putIOwords
--         ["bakeOneFile2html resultFile", showT ht2, "from", showT inputFn, "\n"]
--     when debug $ putIOwords
--         ["bakeOneFile2html resultvalue", take' 300 $ showT val, "\n"
--             , take' 300 $ showT html2]--   when debug $ 
--     putIOwords ["......................"]
--     return . unwords' $ ["bakeOneFile2html outhtml ", take' 300 $ showT inputFn, "done"]

--   `catchError` 
--     (\e -> 
--         do
--             let errmsg2 =
--                     [ "\n****************"
--                     , "bakeOneFile2html catchError"
--                     , "\nfor "
--                     , showT inputFn
--                     , "\n"
--                     , take' 300 . showT $ e
--                     , "\n****************"
--                     ]
--             putIOwords errmsg2
--             return . unwords' $ errmsg2
--             )

bakeDocValue2html
  :: Bool
  -> PubFlags
  -> Path Abs File  -- ^ a docval file (no extension) 
  -> SiteLayout
  -> Path Abs File  -- ^ where the html should go 
  -> ErrIO ()
-- produce html from docval 
bakeDocValue2html debug flags inputFn layout ht2 =
  do
    putIOwords ["\n-----------------", "bakeDocValue2html 1 fn", showT inputFn, "debug", showT debug]

    val <- read8 inputFn docValueFileType 
    when debug $  putIOwords
        ["bakeDocValue2html docval read " ]
    
    html2  <- putValinMaster False val (templatesDir layout)
    write8 ht2 htmloutFileType html2

    when debug $  putIOwords
        ["bakeDocValue2html resultFile", showT ht2, "from", showT inputFn, "\n"]
    when debug $ putIOwords
        ["bakeDocValue2html resultvalue", take' 300 $ showT val, "\n"
            , take' 300 $ showT html2]--   when debug $ 
    putIOwords ["......................"]
    -- return . unwords' $ ["bakeDocValue2html outhtml ", take' 300 $ showT inputFn, "done"]

  `catchError` 
    (\e -> 
        do
            let errmsg2 =
                    [ "\n****************"
                    , "bakeDocValue2html catchError"
                    , "\nfor "
                    , showT inputFn
                    , "\n"
                    , take' 300 . showT $ e
                    , "\n****************"
                    ]
            putIOwords errmsg2
            -- return . unwords' $ errmsg2
            )



bakeOneFile2texsnip
  :: Bool
  -> PubFlags
  -> Path Abs File
  -> SiteLayout
  -> Path Abs File
  -> ErrIO ()
-- TODO should use something like docvalue (pandoc and meta, completed with references and title from meta)
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
   
    texText :: TexSnip <- writeTexSnip2 pandoc2 

    write8 texFn2 texSnipFileType texText

    when debug $  putIOwords
        ["bakeOneFile2tex resultFile", showT texFn2, "from", showT inputFn, "\n"]
    when debug $ putIOwords
        ["bakeOneFile2tex result TeX", showT texText]--   when debug $ 
    putIOwords ["......................"]
    -- return . unwords' $ ["bakeOneFile2tex tetfn ", showT inputFn, "done"]

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
            -- return . unwords' $ errmsg2
            )

bakeOneTexSnip2pdf
  :: Bool
  -> PubFlags
  -> Path Abs File  -- ^ texsnip
  -> SiteLayout
  -> Path Abs File  -- ^ target 
  -> ErrIO ()
-- files exist
-- convert a tex file,  form standalone latex and 
-- process with luatex, the result file is not the name
-- of the md file with just replaced the extension 
-- issue: the standalone latex must not have the same name
-- as the tex (body only) file

bakeOneTexSnip2pdf debug flags inputFn  layout pdfFn2 =
  do
 
    putIOwords ["\n-----------------", "bakeOneTexSnip2pdf 1 inputFn", showT inputFn
            , "\n beomces \n pdfFn2", showT pdfFn2 
            , "\n debug", showT debug]

    texsnip1 :: texsnip <- read8 inputFn texSnipFileType
    let latex1 = tex2latex [texsnip1]  -- :: [TexSnip] -> Latex
    putIOwords ["bakeOneTexSnip2pdf latex1" , unLatex latex1]

    -- write latex1 to tmp dir 

    tempdir <- Path.getTempDir 
    let tempdir2 = tempdir </> (makeRelDir "snip2pdf")
    let nakedFn = getNakedFileName inputFn :: FilePath  
    let tempfile = tempdir2 </> (makeRelFile nakedFn )
    write8 tempfile texFileType latex1 
   
    when debug $  putIOwords
        ["bakeOneTexSnip2pdf tempFile", showT tempfile  ]

    writePDF2text debug tempfile -- takes texsnip extension, produces pdf extension (in the same dir)
    -- the result filenames are: 
    let pdftempfile = replaceExtension' (s2t . unExtension $ extPDF) tempfile 
    let pdfFn3 = pdfFn2 <.> extPDF 
    when debug $  putIOwords
        ["bakeOneTexSnip2pdf pdeftempfile", showT pdftempfile
        , "\n pdfFn3", showT pdfFn3  ]
    copyOneFileOver pdftempfile pdfFn3  -- does this have the extension pdf?

    when debug $  putIOwords
        ["bakeOneTexSnip2pdf resultFile   pdf", showT pdfFn2 ]
     
    putIOwords ["......................"]
    -- return . unwords' $ ["bakeOneTexSnip2pdf pdfText ", showT inputFn, "done"]

  `catchError` 
    (\e -> 
        do
            let errmsg2 =
                    [ "\n****************"
                    , "bakeOneTexSnip2pdf catchError"
                    , "\nfor "
                    , showT inputFn
                    , "\n"
                    , take' 300 . showT $ e
                    , "\n****************"
                    ]
            putIOwords errmsg2
            -- return . unwords' $ errmsg2
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
