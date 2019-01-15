
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

import Text.Pandoc


bake :: Path Rel File -> ErrIO ()
bake fp = do
    putIOwords ["bake for " , showT fp]
    bakeOneFile fp

    return ()

doughDir = "site/dough" :: Path Rel Dir
bakedDir = "site/baked" :: Path Rel Dir

extMD = Extension "md"
extHTML = Extension "html"

bakeOneFile :: -- Path Rel Dir - Path Rel Dir ->
            Path Rel File -> ErrIO ()
-- convert a file (path relative to dough) and put in baked
bakeOneFile fp = do
    let   fnn = removeExtension fp :: Path Rel File
    putIOwords ["fn", showT fnn]
    let   fpi = addFileName doughDir fp  :: Path Rel File
    putIOwords ["fpi", showT fpi]
    let   fpo = addFileName bakedDir fnn
    putIOwords ["fpo", showT fpo]
    infile <- readFile2 fpi

    outhtml :: Text <-  callIO $ mdToHtml5 infile

    writeFile2 fpo outhtml


--import Data.Text (Text)
--import qualified Data.Text.IO as T

mdToRST :: Text -> IO Text
mdToRST txt = runIOorExplode $
  readMarkdown def txt
    >>= writeRST def{ writerReferenceLinks = True }

-- writeHtml5String :: PandocMonad m => WriterOptions -> Pandoc -> m Text

mdToHtml5 :: Text -> IO Text
mdToHtml5 txt = runIOorExplode $
  readMarkdown def txt
    >>= writeHtml5String def
