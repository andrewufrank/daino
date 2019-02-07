
------------------------------------------------------------------------------
--
-- Module      :   the  process to convert
--              files in any input format to html
--              orginals are found in dire doughDir and go to bakeDir
--

-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lib.Bake  -- (openMain, htf_thisModuelsTests)
     where

import Uniform.Strings hiding ((</>))
import Uniform.Filenames
import Uniform.FileStrings () -- for instances
import Uniform.TypedFile

import Lib.Pandoc --  (markdownToPandoc, pandocToContentHtml)
        -- with a simplified Action ~ ErrIO

import Lib.Templating
import Lib.FileMgt
import Data.Aeson

bakeOneFileFPs :: FilePath -> FilePath -> FilePath -> FilePath -> ErrIO ()
-- this is called from shake and produce the final html
-- bake one file absolute fp , page template and result html
-- exceptionally calls with FilePath (usually the files are read in shake and
-- passed the values)
bakeOneFileFPs md doughD templatesD ht = do
        putIOwords ["bakeOneFileFPs - from shake xx", s2t md]
            --baked/SGGdesign/Principles.md
        let md2 = makeAbsFile md
        let dough2 = makeAbsDir doughD
        let templates2 = makeAbsDir templatesD
        let ht2 = makeAbsFile ht
        when False $ putIOwords ["bakeOneFileIO - files", showT md2
                        , "\ntemplate: ", showT templates2, "\noutput file: ", showT ht2]
--        let masterSettings = makeAbsFile masterSettingsFn
        res <- bakeOneFile False md2 dough2 templates2 ht2
        putIOwords ["bakeOneFileFPs - done", showT ht2, res]


bakeOneFile :: Bool -> Path Abs File -> Path Abs Dir
        -> Path Abs Dir -> Path Abs File -> ErrIO Text
-- files exist
-- convert a file md2, process citations if any
-- separate html content and put in contentHtml
-- get pageType, read file and process

--test in bake_tests:
bakeOneFile debug pageFn dough2 templates2 ht2 = do
        putIOwords ["\n-----------------", "bakeOneFile fn", showT pageFn]
        -- currently only for md files
        pageMd :: MarkdownText <- read8 pageFn markdownFileType -- pageFn -> pageMd
        -- process the md file (including bibtex citations)
        pandoc <- markdownToPandoc debug pageMd  -- AG -> AD
                                                        -- withSettings.pandoc
        -- produce html and put into contentHtml key
        docval <- pandocToContentHtml debug pandoc  -- content.docval

        val <- docValToAllVal debug docval pageFn dough2 templates2

        html2 <- putValinMaster debug val templates2

        write8   ht2 htmloutFileType html2
--            putIOwords ["bakeOneFile outhtml
--            (which was just written) \n", unHTMLout html2, "\n"]

        when debug $ putIOwords ["bakeOneFile resultFile"
                        , showT ht2, "from",  showT pageFn, "\n"]
        when debug $ putIOwords ["......................"]

        return . unwords' $  ["bakeOneFile outhtml ", showT pageFn, "done"]

 `catchError` (\e -> do
                    let errmsg2 =  ["\n****************"
                                , "bakeOneFile catchError", showT e , "for ", showT pageFn

                                , "\n****************"]
                    putIOwords errmsg2
                    return . unwords' $ errmsg2
                )

putValinMaster :: Bool -> DocValue -> Path Abs Dir -> ErrIO HTMLout
-- ^ get the master html template and put the val into it
-- takes the master filename from val
putValinMaster debug val templates2 =  do
        let mmt = getMaybeStringAtKey val "masterTemplate"
        let masterfn = t2s $ fromMaybe "master4.dtpl"  mmt :: FilePath
        template <- read8 (templates2 </> masterfn) dtmplFileType
        html2 <-  applyTemplate3 template val  -- inTemplate.html
        when debug $
            putIOwords ["putValinMaster", showT html2]
        return html2






