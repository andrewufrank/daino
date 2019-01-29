
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

import Lib.Pandoc  (markdownToPandoc, pandocToContentHtml)   -- with a simplified Action ~ ErrIO

import Lib.Templating
import Lib.FileMgt
import Lib.Foundation

import Control.Lens
--import Data.Aeson
import Data.Aeson.Lens


bakeOneFile :: Bool -> Path Abs File -> Path Abs File -> Path Abs File -> Path Abs File -> ErrIO Text
-- this is called from shake
-- files exist
-- convert a file md2, append  masterSettings and put result in masterTemplate2 and produce th2
bakeOneFile debug md2 masterSettings masterTemplName ht2 = do
        putIOwords ["\n--------------------------------", "bakeOneFile fn", showT md2, "\n\n"]
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
bakeOneFileCore debug preface intext masterTempl  = do
        let intext2 = spliceMarkdown preface intext
        pandoc <- markdownToPandoc debug intext2
        val <- pandocToContentHtml debug pandoc

        let ptemplate = fmap t2s $  (unDocValue val) ^? key "pageTemplate" . _String :: Maybe FilePath
--
        templ2 <- case ptemplate of
            Nothing -> throwErrorT ["bakeOneFileCore", "no page template in markdown"]
            Just tfn -> do
                            let tfn2 = addFileName (themeDir layoutDefaults)
                                    (addFileName templatesDirName (makeRelFile tfn))
                            putPageInMaster2 tfn2 masterTempl "body"

        html2 <-  applyTemplate3 templ2 val
--        when debug $
        putIOwords ["bakeOneFile val\n\n", showT html2]

        return html2


spliceMarkdown :: YamlText -> MarkdownText -> MarkdownText
-- postfix the master yaml text to a markdown text
spliceMarkdown (YamlText y) (MarkdownText m) = MarkdownText $ m <> y

--spliceTemplates :: DocValue  -> Gtemplate -> ErrIO Dtemplate
-- splice the desired page template with the master template
--
spliceTemplates val masterTempl = do
        let ptemplate = fmap t2s $  (unDocValue val) ^? key "pageTemplate" . _String :: Maybe FilePath
--
        templ2 <- case ptemplate of
            Nothing -> throwErrorT ["bakeOneFileCore", "no page template in markdown"]
            Just tfn -> do
                            let tfn2 = addFileName (themeDir layoutDefaults)
                                    (addFileName templatesDirName (makeRelFile tfn))
                            putPageInMaster2 tfn2 masterTempl "body"
        return templ2
