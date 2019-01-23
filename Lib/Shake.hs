
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
import Uniform.Filenames (toFilePath, makeAbsFile) -- hiding ((<.>))
import Uniform.FileStrings () -- for instances
import Uniform.Error

--import Uniform.Piped
--import           Uniform.FileIO as FN hiding ( (</>), (<.>))  -- (<>),
--
--import Lib.Pandoc (markdownToHTML4x)   -- with a simplified Action ~ ErrIO
--
--import Lib.Templating
--import Lib.FileMgt
import Lib.Foundation
import Lib.Bake

--import qualified Pipes as Pipe
--import qualified Pipes.Prelude as Pipe
--import Pipes ((>->)) -- , (~>)
--import qualified Path  as Path

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

shake ::    ErrIO ()
shake   = do
    putIOwords ["\nshake start"]
    shakeWrapped-- bakeAllInSiteMD (bakeOneFile2 False)  doughPath  reportFilePath
    putIOwords ["\nshake done", "\n"]

    return ()

bakedD  = "site/baked" -- toFilePath bakedPath
doughD = "site/dough"

shakeWrapped :: ErrIO  ()
shakeWrapped = callIO $ shakeArgs shakeOptions {shakeFiles=bakedD } $
    do
        want ["index"<.>"html"]

        "index"<.>"html" %> \out ->
            do
                mds <- getDirectoryFiles  doughD ["//*.md"] -- markdown ext ??
                let htmlFile = [bakedD </> md -<.> "html" | md <- mds]
                putIOwords ["shakeWrapped - htmlFile", showT htmlFile]
                need htmlFile
--                liftIO $ mapM_ bakeOneFileIO  c

        (bakedD <> "//*.html") %> \out ->
            do
                let c = dropDirectory1 $ out -<.> "md"
                liftIO $  bakeOneFileIO  c


instance Exception Text

bakeOneFileIO :: FilePath -> IO ()
bakeOneFileIO fp = do
            et <- runErr $ do
                    putIOwords ["bakeOneFileIO - from shake xx", s2t fp] --baked/SGGdesign/Principles.md

--                            let fp1 = "/"<>fp
                    putIOwords ["bakeOneFileIO - gp1", s2t fp1]
                    let fp2 = makeRelFile fp1
                    putIOwords ["bakeOneFileIO - next run shake", showT fp2]
                    bakeOneFileVoid fp1
            case et of
                Left msg -> throw msg
                Right _ -> return ()
