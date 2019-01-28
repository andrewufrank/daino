
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
import Data.Aeson
import Data.Aeson.Lens

--
readMarkdownFile :: Path Abs File -> ErrIO MarkdownText
-- read one file
readMarkdownFile fnn = read8  fnn markdownFileType

bakeOneFile :: Bool -> Path Abs File -> Path Abs File -> Path Abs File -> Path Abs File -> ErrIO Text
-- convert a file md2, append  masterSettings and put result in masterTemplate2 and produce th2
bakeOneFile debug md2 masterSettings templateFn ht2 = do
--        let   fnn = removeExtension fp :: Path Rel File
--        when debug $
        putIOwords ["\n--------------------------------", "bakeOneFile fn", showT md2, "\n\n"]
        -- currently only for md files, add static next

        intext :: MarkdownText <- read8 md2 markdownFileType
        preface :: YamlText <- read8 masterSettings yamlFileType

--        let intext2 = MarkdownText (unMT intext <> unYAML preface )
--        putIOwords ["bakeOneFile spliced", showT intext2, "\n"]
--        templText :: Dtemplate <- read8  templateFn  dtmplFileType

        html2 :: HTMLout <- bakeOneFileCore debug intext templateFn

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
--combineTemplates :: Bool -> Path Abs File -> Path Abs File -> ErrIO Dtemplate
---- combine the master templates with the page template
--combineTemplates debug mtpl ptpl = do
--        when debug $ putIOwords ["combineTemplates Master", showT mtpl, "\npagetemplate", showT ptpl]

bakeOneFileCore :: Bool -> MarkdownText  -> Path Abs File  -> ErrIO HTMLout
-- the template can only be combined here,
-- because the page template name is only accessible in the pandoc
bakeOneFileCore debug intext masterTemplName = do
        pandoc <- markdownToPandoc debug intext
        val <- pandocToContentHtml debug pandoc

        let ptemplate = fmap t2s $  (unDocValue val) ^? key "pageTemplate" . _String :: Maybe FilePath
--
        templ2 <- case ptemplate of
            Nothing -> read8  masterTemplName  dtmplFileType
            Just tfn -> do
                            let tfn2 = addFileName (themeDir layoutDefaults)
                                    (addFileName templatesDirName (makeRelFile tfn))
                            putPageInMaster2 tfn2 masterTemplName "tag"
--    -- convert to html
--        val :: DocValue <- markdownToHTML4x debug intext
----            val  <- markdownToHTML4a intext
--    --    let html1  =  HTMLout $  val ^.  key "content" . _String

    --    putIOwords ["bakeOneFile html1\n\n", unHTMLout html1]
        when debug $ putIOwords ["bakeOneFile val\n\n", showNice val]

        html2 <-  applyTemplate3 templ2 val

        when debug $ putIOwords ["bakeOneFile val\n\n", showT html2]

        return html2
