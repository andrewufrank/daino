
------------------------------------------------------------------------------
--
-- Module      :   the  process to convert
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

import Slick.Pandoc (markdownToHTML)   -- with a simplified Action ~ ErrIO

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

    val <- markdownToHTML intext

   let v1 =            val ^.  key "content" . _String
    writeFile2 fpo v1
    putIOwords ["bakeOne outhtml\n", v1]
    putIOwords ["bakeOne result\n", showPretty val]

showPretty :: ToJSON a => a -> Text
showPretty = bb2t . bl2b . encodePretty

