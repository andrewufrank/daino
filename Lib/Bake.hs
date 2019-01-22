
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
import Uniform.Piped
import           Uniform.FileIO as FN hiding ( (</>), (<.>))  -- (<>),

import Lib.Pandoc (markdownToHTML4x)   -- with a simplified Action ~ ErrIO

import Lib.Templating
import Lib.FileMgt
import Lib.Foundation

import qualified Pipes as Pipe
import qualified Pipes.Prelude as Pipe
import Pipes ((>->)) -- , (~>)
import qualified Path  as Path


bake ::    ErrIO ()
bake   = do
    putIOwords ["\nbake start"]
    msg <- bakeAllInSiteMD (bakeOneFile2 False)  doughPath  reportFilePath
    putIOwords ["\nbake all msg " , msg]

    return ()

bake4test :: Path Rel File -> ErrIO ()
bake4test fp = do
    putIOwords ["\nbake     for " , showT fp]
    msg <- bakeOneFile2 True (addDir doughPath fp)
    putIOwords ["\nbake all msg " , msg]

    return ()

--reportFile :: Path Abs File
--reportFile = makeAbsFile "/home/frank/reportBakeAll.txt"

bakeAllInSiteMD :: (Path Abs File -> ErrIO Text)-> Path Abs Dir
                -> Path Abs File -> ErrIO Text
-- convert all markdonw in site with ops
bakeAllInSiteMD ops siteDough   reportFile1 = do
    putIOwords ["\nbakeAllInSiteMD", "site", showT siteDough, "reportFile", showT reportFile1]
--    let path = toFilePath site
--    resFile :: Path Abs File <- makeAbsoluteFile' file
    bracketErrIO (FN.openFile2handle reportFile1 WriteMode)
                (\hand -> do
                    putIOwords ["bakeAllInSiteMD close"]
                    closeFile2 hand -- not with transaction tmp
                    )
                (\hand ->
                      Pipe.runEffect $
                        getRecursiveContents siteDough
--                        >-> Pipe.filter test--  filter is in bakeOneFile2 as a case
                        >-> Pipe.mapM (fmap t2s . ops)
                            --  putOneFile2xx debug forceFlag server db mgraph)
                    --    >-> P.stdoutLn
                        >-> Pipe.toHandle hand
                )
    report <- readFile2 reportFile1
    return $ unwords' ["\nbakeAllInSiteMD end", showT siteDough, showT reportFile1
            , "\n", report
            , "..........................", "ok"]

testMD :: Path Abs File -> Bool
testMD = hasExtension extMD

bakeOneFile2 :: Bool -> Path Abs File -> ErrIO Text
-- bakes one file - first true gives lots of debug info
bakeOneFile2 debug fp2 =  do
    fpath :: Path Rel File  <- stripProperPrefix' doughPath fp2
--        : MonadThrow m => Path b Dir -> Path b t -> m (Path Rel t)
--    let   fnn = removeExtension fpath
    bakeOneFile debug fpath

readMarkdownFile :: Path Rel File -> ErrIO MarkdownText
-- read one file
readMarkdownFile fnn = read7 doughPath fnn markdownFileType

bakeOneFileVoid ::  FilePath  -> ErrIO ()
-- convert a file (path relative to dough) and put in baked
bakeOneFileVoid fp = do

        putIOwords ["\n--------------------------------", "bakeOneFileVoid fn", showT fp, "\n"]
        let fp2 = makeAbsFile fp
        fpath :: Path Rel File  <- stripProperPrefix' doughPath fp2
        -- produces errror if not a prefix?

        r <- bakeOneFile False fpath
        putIOwords ["\n--------------------------------", "done"]
        return ()

bakeOneFile :: Bool -> Path Rel File -> ErrIO Text
-- convert a file (path relative to dough) and put in baked
bakeOneFile debug fp = do
        let   fnn = removeExtension fp :: Path Rel File
--        when debug $
        putIOwords ["\n--------------------------------", "bakeOneFile fn", showT fnn, "\n\n"]
        -- currently only for md files, add static next

        intext :: MarkdownText <- readMarkdownFile fnn
    -- convert to html
        val  <- markdownToHTML4x debug intext
--            val  <- markdownToHTML4a intext
    --    let html1  =  HTMLout $  val ^.  key "content" . _String

    --    putIOwords ["bakeOneFile html1\n\n", unHTMLout html1]
        when debug $ putIOwords ["bakeOneFile val\n\n", showNice val]

    --     apply template before writing
        let templateFileName =  addDir templatePath
                        (makeRelFile "pandocDefault.html"::Path Rel File)
                          :: Path Abs File
        html2 <-  applyTemplate2 templateFileName val

        when debug $ putIOwords ["bakeOneFile resultFile", showT bakedPath, showT fnn, "\n"]
        write7 bakedPath fnn htmloutFileType html2
--            putIOwords ["bakeOneFile outhtml (which was just written) \n", unHTMLout html2, "\n"]

        when debug $ putIOwords ["\n......................"]

        return . unwords' $  ["bakeOneFile outhtml ", showT fnn, "done"]

 `catchError` (\e -> do
                    let errmsg2 =  ["\n****************"
                                , "bakeOneFile catchError", showT e , "for ", showT fp
                                , "\n****************"]
                    putIOwords errmsg2
                    return . unwords' $ errmsg2
                )

stripProperPrefix' ::  Path b Dir -> Path b t -> ErrIO (Path Rel t)
stripProperPrefix' dir fn = fmap Path $ Path.stripProperPrefix (unPath dir) (unPath fn)
