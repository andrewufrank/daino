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
module Lib.Bake (bakeOneFile) 
  --  (openMain, htf_thisModuelsTests)
                where

import           Uniform.FileStrings            ( ) -- for instances
import           Uniform.Filenames
import           Lib.Pandoc                     ( docValToAllVal
                                                , markdownToPandocBiblio
                                                , pandocToContentHtml
                                                -- , MenuEntry(..)
                                                )-- with a simplified Action ~ ErrIO

import           Lib.Templating                 ( putValinMaster )
import           Uniform.Pandoc                 ( Pandoc
                                                , htmloutFileType
                                                , write8
                                                , HTMLout (..)
                                                )
import           Lib.CmdLineArgs                ( PubFlags(..) )
import Lib.CheckInput (checkOneMdFile)
import Lib.Foundation (SiteLayout(..), templatesDir)

bakeOneFile
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
bakeOneFile debug flags pageFn layout ht2 =
  do
      putIOwords ["\n-----------------", "bakeOneFile fn", showT pageFn, "debug", showT debug]
                         -- currently only for md files
                 --        pageMd :: MarkdownText <- read8 pageFn markdownFileType -- pageFn -> pageMd
                         -- process the md file (including bibtex citations)
                 --        let resourcesPath = doughP </> resourcesDirName :: Path Abs Dir
      (pandoc, metaRec, report) <- checkOneMdFile layout pageFn
      -- how are errors dealt with 
      -- let debug = True
      pandoc2 :: Pandoc <- markdownToPandocBiblio debug flags (doughDir layout) (pandoc, metaRec, report) -- AG -> AD
                        -- withSettings.pandoc
                         -- produce html and put into contentHtml key
                         -- can be nothing if the md file is not ready to publish
      -- case mtripledoc of
      --   Just tripleDoc -> do
      htmlout :: HTMLout <- pandocToContentHtml debug pandoc2 -- content.docval  AD -> AF
    --   index :: MenuEntry <- makeIndex debug flags metaRec
    --   putIOwords ["bakeOneFile index", showT index, "\n"]

      val    <- docValToAllVal debug layout flags htmlout  metaRec
      -- includes the directory list and injection, which should be in 
      -- value "menu2"
      html2  <- putValinMaster debug val (templatesDir layout)
      write8 ht2 htmloutFileType html2
        --            putIOwords ["bakeOneFile outhtml
        --            (which was just written) \n", unHTMLout html2, "\n"]
      when debug $  putIOwords
            ["bakeOneFile resultFile", showT ht2, "from", showT pageFn, "\n"]
      when debug $ putIOwords
            ["bakeOneFile resultvalue", showT val, "\n"
                , showT html2]--   when debug $ 
      putIOwords ["......................"]
      return . unwords' $ ["bakeOneFile outhtml ", showT pageFn, "done"]
        -- Nothing ->
        --   return
        --     . unwords'
        --     $ ["bakeOneFile outhtml ", showT pageFn, "nothing to do"]
    `catchError` (\e -> do
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
