
------------------------------------------------------------------------------
--
-- Module      :   the main process to convert
--              files in any input format to html
--              orginals are found in dire doughDir and go to bakeDir

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
import Uniform.FileStrings

--import qualified Text.Pandoc as Pan
----        (writeHtml5String, readMarkdown, runIOorExplode)
--import Text.Pandoc.Highlighting
--import Text.Pandoc.Shared
import Slick.Pandoc

import Control.Lens
import Data.Aeson.Lens
import Data.Aeson.Encode.Pretty
import Data.Aeson


bake :: Path Rel File -> ErrIO ()
bake fp = do
    putIOwords ["bake for " , showT fp]
    bakeOneFile fp

    return ()

doughDir = makeRelDir "dough" :: Path Rel Dir
bakedDir = makeRelDir "baked" :: Path Rel Dir
siteDir = makeAbsDir "/home/frank/Workspace8/SSG/site" :: Path Abs Dir

doughPath = addDir siteDir doughDir :: Path Abs Dir
bakedPath = addDir siteDir bakedDir :: Path Abs Dir

extMD = Extension "md"
extHTML = Extension "html"

bakeOneFile :: -- Path Rel Dir - Path Rel Dir ->
            Path Rel File -> ErrIO ()
-- convert a file (path relative to dough) and put in baked
bakeOneFile fp = do
    let   fnn = removeExtension fp :: Path Rel File
    putIOwords ["fn", showT fnn]
    let   fpi = addFileName doughPath fp  :: Path Abs File
    putIOwords ["fpi", showT fpi]
    let   fpo = addExtension extHTML $
                        addFileName bakedPath fnn :: Path Abs File
    putIOwords ["fpo", showT fpo]
    intext <- readFile2 fpi
    putIOwords ["bakeOne infile\n", intext]

--    outhtml :: Text <-  callIO $ mdToHtml5 intext
    val <- markdownToHTML intext
--    let outhtml = -- maybe (fail "no conten") s2t $
    let v1 =            val ^.  key "content" . _String
    writeFile2 fpo v1
    putIOwords ["bakeOne outhtml\n", v1]
    putIOwords ["bakeOne result\n", showPretty val]

showPretty :: ToJSON a => a -> Text
showPretty = bb2t . bl2b . encodePretty

----import Data.Text (Text)
----import qualified Data.Text.IO as T
--
----mdToRST :: Text -> IO Text
----mdToRST txt = runIOorExplode $
----  readMarkdown def txt
----    >>= writeRST def{ writerReferenceLinks = True }
--
---- writeHtml5String :: PandocMonad m => WriterOptions -> Pandoc -> m Text
--
--mdToHtml5errio :: Text -> ErrIO a
---- do the md to html conversion in the ErrIO
--mdToHtml5errio txt = do
--    res <- callIO $   do
--               Pan.Pandoc meta a :: Pan.Pandoc
--                        <- Pan.runIO $ Pan.readMarkdown markdownOptions  txt
--               txt <-  Pan.runIO $ Pan.writeHtml5String Pan.def
--               return (meta, txt)
--    return (either (fail . show)  res)
--
--mdToHtml5 :: Text -> IO Text
--mdToHtml5 txt = runIOorExplode $
--  readMarkdown markdownOptions  txt
--    >>= writeHtml5String def
--
---- | Reasonable options for reading a markdown file
--markdownOptions :: Pan.ReaderOptions
--markdownOptions = def { Pan.readerExtensions = exts }
-- where
--  exts = mconcat
--    [ extensionsFromList
--      [ Ext_yaml_metadata_block
--      , Ext_fenced_code_attributes
--      , Ext_auto_identifiers
--      ]
--    , githubMarkdownExtensions
--    ]
--
---- | Reasonable options for rendering to HTML
--html5Options :: Pan.WriterOptions
--html5Options = def { Pan.writerHighlightStyle = Just tango
--                   , Pan.writerExtensions     = Pan.writerExtensions def
--                   }
