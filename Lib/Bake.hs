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
module Lib.Bake -- (openMain, htf_thisModuelsTests)
                where

import           Uniform.FileStrings            ( ) -- for instances
import           Uniform.Filenames
-- import Uniform.Strings hiding ((</>))

-- import Uniform.TypedFile
import           Lib.Pandoc                     ( docValToAllVal
                                                , markdownToPandoc
                                                , pandocToContentHtml
                                                )-- with a simplified Action ~ ErrIO

import           Lib.Templating                 ( putValinMaster )
import           Uniform.Pandoc                 ( Pandoc
                                                , htmloutFileType
                                                , write8
                                                )
import           Lib.CmdLineArgs                ( PubFlags(..) )

bakeOneFile
  :: Bool
  -> PubFlags
  -> Path Abs File
  -> Path Abs Dir
  -> Path Abs Dir
  -> Path Abs File
  -> ErrIO Text
-- files exist
-- convert a file md2, process citations if any
-- separate html content and put in contentHtml
-- get pageType, read file and process
--test in bake_tests:
bakeOneFile debug flags pageFn doughP templatesP ht2 =
  do
      putIOwords ["\n-----------------", "bakeOneFile fn", showT pageFn]
                         -- currently only for md files
                 --        pageMd :: MarkdownText <- read8 pageFn markdownFileType -- pageFn -> pageMd
                         -- process the md file (including bibtex citations)
                 --        let resourcesPath = doughP </> resourcesDirName :: Path Abs Dir
      mpandoc :: Maybe Pandoc <- markdownToPandoc debug doughP pageFn -- AG -> AD
                                                                         -- withSettings.pandoc
                         -- produce html and put into contentHtml key
                         -- can be nothing if the md file is not ready to publish
      case mpandoc of
        Just pandoc -> do
          docval <- pandocToContentHtml debug pandoc -- content.docval  AD -> AF
          val    <- docValToAllVal debug flags docval pageFn doughP templatesP
          html2  <- putValinMaster debug val templatesP
          write8 ht2 htmloutFileType html2
                                 --            putIOwords ["bakeOneFile outhtml
                                 --            (which was just written) \n", unHTMLout html2, "\n"]
          when debug $ putIOwords
            ["bakeOneFile resultFile", showT ht2, "from", showT pageFn, "\n"]
          when debug $ putIOwords ["......................"]
          return . unwords' $ ["bakeOneFile outhtml ", showT pageFn, "done"]
        Nothing ->
          return
            . unwords'
            $ ["bakeOneFile outhtml ", showT pageFn, "nothing to do"]
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
