
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
--import Lib.Foundation

--import Data.Yaml (decodeThrow)
--import Control.Lens
import Data.Aeson
--import Data.Aeson.Lens
import  Data.Yaml.Union

bakeOneFileFPs :: FilePath -> FilePath -> FilePath -> FilePath -> ErrIO ()
-- this is called from shake and produce the final html
-- bake one file absolute fp , page template and result html
-- exceptionally calls with FilePath (usually the files are read in shake and
-- passed the values)
bakeOneFileFPs md doughD templateD ht = do
        putIOwords ["bakeOneFileFPs - from shake xx", s2t md]
            --baked/SGGdesign/Principles.md
        let md2 = makeAbsFile md
        let dough2 = makeAbsDir doughD
        let template2 = makeAbsDir templateD
        let ht2 = makeAbsFile ht
        when False $ putIOwords ["bakeOneFileIO - files", showT md2
                        , "\ntemplate: ", showT template2, "\noutput file: ", showT ht2]
--        let masterSettings = makeAbsFile masterSettingsFn
        res <- bakeOneFile False md2 dough2 template2 ht2
        putIOwords ["bakeOneFileFPs - done", showT ht2, res]


bakeOneFile :: Bool -> Path Abs File -> Path Abs Dir
        -> Path Abs Dir -> Path Abs File -> ErrIO Text
-- files exist
-- convert a file md2, process citations if any
-- separate html content and put in contentHtml
-- get pageType, read file and process

--test in bake_tests:
bakeOneFile debug pageFn dough2 template2 ht2 = do
        putIOwords ["\n-----------------", "bakeOneFile fn", showT pageFn, "\n\n"]
        -- currently only for md files
        pageMd :: MarkdownText <- read8 pageFn markdownFileType -- pageFn -> pageMd
        -- process the md file (including bibtex citations)
        pandoc <- markdownToPandoc debug pageMd  -- AG -> AD
                                                        -- withSettings.pandoc
        -- produce html and put into contentHtml key
        docval <- pandocToContentHtml debug pandoc  -- content.docval

        let mpt = getMaybeStringAtKey docval "pageTemplate"
        let pageType = maybe "page3" id mpt
        -- TODO where is default page set?
        yaml <- read8  ( template2 </> (pageType)) yamlFileType
--        ptype :: Value <- decodeThrow   . t2b . unYAML $ yaml

        let mmt = getMaybeStringAtKey docval "masterTemplate"

        settings <- read8 (dough2 </> makeRelFile "settings2") yamlFileType
--        svalue <- decodeThrow . t2b . unYAML $ settings

        -- TODO where is settings2 file name fixed
        let masterfn = maybe "master4.dtpl" id mmt
        template <- read8 (template2 </> masterfn) dtmplFileType

        let val = DocValue . fromJustNote "decoded union 2r2e"
                      . decodeBytestrings
                    $ [bl2b . encode $ unDocValue docval, t2b $ unYAML yaml, t2b $ unYAML settings]

        html2 <-  applyTemplate3 template val  -- inTemplate.html

--
        write8   ht2 htmloutFileType html2
--            putIOwords ["bakeOneFile outhtml
--            (which was just written) \n", unHTMLout html2, "\n"]

        when debug $ putIOwords ["bakeOneFile resultFile"
                        , showT ht2, "from",  showT pageFn, "\n"]
        when debug $ putIOwords ["\n......................"]

        return . unwords' $  ["bakeOneFile outhtml ", showT pageFn, "done"]

 `catchError` (\e -> do
                    let errmsg2 =  ["\n****************"
                                , "bakeOneFile catchError", showT e , "for ", showT pageFn

                                , "\n****************"]
                    putIOwords errmsg2
                    return . unwords' $ errmsg2
                )

--bakeOneFileCore :: Bool -> YamlText -> MarkdownText  -> Gtemplate  -> ErrIO HTMLout
---- the template can only be combined here,
---- because the page template name is only accessible in the pandoc
---- this is teh complete processing (above is only read and write)
---- AK (markdow) ->  R (HTMLout)
---- the yaml text needs --- before and ofter to separate from doc text value
--bakeOneFileCore debug preface intext masterTempl  = do
--        let intext2 = spliceMarkdown preface intext  --  AK -> AG
--        pandoc <- markdownToPandoc debug intext2  -- AG -> AD
--        val :: DocValue <- pandocToContentHtml debug pandoc
--
--        templ2 <- spliceTemplates val masterTempl
--
--        html2 <-  applyTemplate3 templ2 val
--        when debug $
--            putIOwords ["bakeOneFile val\n\n", showT html2]
--
--        return html2
--
--
--spliceMarkdown :: YamlText -> MarkdownText -> MarkdownText
---- postfix the master yaml text to a markdown text
--spliceMarkdown (YamlText y) (MarkdownText m) = MarkdownText $ m <> yamlSep1 <> y <> yamlSep2
--    where
--        yamlSep1 = "\n---"
--        yamlSep2 = "\n---\n"
--    -- assumes that the is spliced aftera a newline and text ends with newline


