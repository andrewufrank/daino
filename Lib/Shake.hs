
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

module Lib.Shake
     where

import Uniform.Strings (putIOwords) -- hiding ((<.>), (</>))
import Uniform.Filenames -- (toFilePath, makeAbsFile, makeRelFile, makeRelDir, stripProperPrefix')
         hiding ((<.>), (</>))
import Uniform.FileStrings () -- for instances
import Uniform.Error


import Lib.Foundation
import Lib.Bake

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

shake ::    ErrIO ()
shake   = do
    putIOwords ["\nshake start"]
    callIO $ shakeWrapped-- bakeAllInSiteMD (bakeOneFile2 False)  doughPath  reportFilePath
    putIOwords ["\nshake done", "\n"]

    return ()

bakedD  = "site/baked" -- toFilePath bakedPath
doughD = "site/dough"

shakeWrapped :: IO  ()
shakeWrapped = shakeArgs shakeOptions {shakeFiles=bakedD
                , shakeVerbosity=Loud
                , shakeLint=Just LintBasic
                } $
    do
        want ["allMarkdownConversion"]
        phony "allMarkdownConversion" $
            do
                mds <- getDirectoryFiles  doughD ["//*.md"] -- markdown ext ??
                let htmlFiles = [bakedD </> md -<.> "html" | md <- mds]
--                liftIO $ putIOwords ["shakeWrapped - htmlFile", showT htmlFiles]
                need htmlFiles

        (bakedD <> "//*.html") %> \out ->
            do
                let c = dropDirectory1 $ out -<.> "md"
                liftIO $  bakeOneFileIO  c


instance Exception Text

bakeOneFileIO :: FilePath -> IO ()
bakeOneFileIO fp = do
            et <- runErr $ do
                    putIOwords ["bakeOneFileIO - from shake xx", s2t fp] --baked/SGGdesign/Principles.md
                    let fp1 = fp
--                            let fp1 = "/"<>fp
                    putIOwords ["bakeOneFileIO - gp1", s2t fp1]
                    let fp2 = makeRelFile fp1
                    fp3 :: Path Rel File  <- stripProperPrefix' (makeRelDir "baked") fp2
                    putIOwords ["bakeOneFileIO - next run shake", showT fp2]
--                            bakeOneFileIO - next run shake "baked/SGGdesign/Principles.md"

                    res <- bakeOneFile False fp3
                    putIOwords ["bakeOneFileIO - done", showT fp3, res]
            case et of
                Left msg -> throw msg
                Right _ -> return ()
