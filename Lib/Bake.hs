
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

import Uniform.Strings
import Uniform.Filenames
import Uniform.FileStrings () -- for instances
import Uniform.TypedFile

import Lib.Pandoc  (markdownToPandoc, pandocToContentHtml)   -- with a simplified Action ~ ErrIO

import Lib.Templating
import Lib.FileMgt
import Lib.Foundation

import Control.Lens
--import Data.Aeson
import Data.Aeson.Lens

bakeOneFileFPs :: FilePath -> FilePath -> FilePath -> FilePath -> ErrIO ()
-- this is called from shake and produce the final html
-- bake one file absolute fp , page template and result html
-- exceptionally calls with FilePath (usually the files are read in shake and
-- passed the values)
bakeOneFileFPs md masterSettingsFn template ht = do
        putIOwords ["bakeOneFileFPs - from shake xx", s2t md] --baked/SGGdesign/Principles.md
        let md2 = makeAbsFile md
        let template2 = makeAbsFile template
        let ht2 = makeAbsFile ht
        when False $ putIOwords ["bakeOneFileIO - files", showT md2
                        , "\n", showT template2, "\n", showT ht2]
        let masterSettings = makeAbsFile masterSettingsFn
        res <- bakeOneFile True md2 masterSettings template2 ht2
        putIOwords ["bakeOneFileFPs - done", showT ht2, res]


bakeOneFile :: Bool -> Path Abs File -> Path Abs File -> Path Abs File -> Path Abs File -> ErrIO Text
-- files exist
-- convert a file md2, append  masterSettings and put result in masterTemplate2 and produce th2
--test in bake_tests:
bakeOneFile debug md2 masterSettings masterTemplName ht2 = do
        putIOwords ["\n-----------------", "bakeOneFile fn", showT md2, "\n\n"]
        -- currently only for md files, add static next
        -- read all files
        intext :: MarkdownText <- read8 md2 markdownFileType
        preface :: YamlText <- read8 masterSettings yamlFileType
        masterTmpl :: Gtemplate <- read8  masterTemplName  gtmplFileType

        html2 :: HTMLout <- bakeOneFileCore debug preface intext masterTmpl

        write8   ht2 htmloutFileType html2
--            putIOwords ["bakeOneFile outhtml (which was just written) \n", unHTMLout html2, "\n"]

        when debug $ putIOwords ["bakeOneFile resultFile", showT ht2, "from",  showT md2, "\n"]
        when debug $ putIOwords ["\n......................"]

        return . unwords' $  ["bakeOneFile outhtml ", showT md2, "done"]

 `catchError` (\e -> do
                    let errmsg2 =  ["\n****************"
                                , "bakeOneFile catchError", showT e , "for ", showT md2
                                , "\n****************"]
                    putIOwords errmsg2
                    return . unwords' $ errmsg2
                )

bakeOneFileCore :: Bool -> YamlText -> MarkdownText  -> Gtemplate  -> ErrIO HTMLout
-- the template can only be combined here,
-- because the page template name is only accessible in the pandoc
-- this is teh complete processing (above is only read and write)
-- AK (markdow) ->  R (HTMLout)
bakeOneFileCore debug preface intext masterTempl  = do
        let intext2 = spliceMarkdown preface intext  --  AK -> AG
        pandoc <- markdownToPandoc debug intext2  -- AG -> AD
        val :: DocValue <- pandocToContentHtml debug pandoc

        templ2 <- spliceTemplates val masterTempl

        html2 <-  applyTemplate3 templ2 val
        when debug $
            putIOwords ["bakeOneFile val\n\n", showT html2]

        return html2


spliceMarkdown :: YamlText -> MarkdownText -> MarkdownText
-- postfix the master yaml text to a markdown text
spliceMarkdown (YamlText y) (MarkdownText m) = MarkdownText $ m <> y

spliceTemplates :: DocValue  -> Gtemplate -> ErrIO Dtemplate
-- splice the desired page template with the master template
            --is the product of a gtempl and a page template
            -- but is produced for each page (wasteful)
spliceTemplates val masterTempl = do
--        putIOwords ["spliceTemplates", "val", shownice val, "\nmasterTempl", showT masterTempl]
        let ptemplate = fmap t2s $  (unDocValue val) ^? key "pageTemplate" . _String :: Maybe FilePath
--
        templ2 <- case ptemplate of
            Nothing -> throwErrorT ["bakeOneFileCore", "no page template in markdown"]
            Just tfn -> do
                            let tfn2 = addFileName (themeDir layoutDefaults)
                                    (addFileName templatesDirName (makeRelFile tfn))
                            putPageInMaster2 tfn2 masterTempl "body"
        return templ2

