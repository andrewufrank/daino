
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
--import Uniform.TypedFile
import Uniform.Piped
import           Uniform.FileIO as FN hiding ( (</>), (<.>))  -- (<>),
--import Uniform.Error

import Slick.Pandoc (markdownToHTML3)   -- with a simplified Action ~ ErrIO
--import Text.Pandoc.Templates (applyTemplate)

--import Control.Lens
--import Data.Aeson.Lens
--import Data.Aeson.Encode.Pretty (showPretty)
--import Data.Aeson

import Lib.Templating
import Lib.FileMgt

import qualified Pipes as Pipe
import qualified Pipes.Prelude as Pipe
import Pipes ((>->)) -- , (~>)
import qualified Path  as Path

--import Text.Pandoc

bake ::   ErrIO ()
bake   = do
    putIOwords ["\nbake start"]
--    msg <- bakeOneFile2 (addDir doughPath fp)
--    putIOwords ["bake simple for " , showT fp]
    msg <- bakeAllInSiteMD bakeOneFile2   doughPath  reportFile
    putIOwords ["\nbake all msg " , msg]

    return ()

bake4test :: Path Rel File -> ErrIO ()
bake4test fp = do
    putIOwords ["\nbake     for " , showT fp]
    msg <- bakeOneFile2 (addDir doughPath fp)
    putIOwords ["\nbake all msg " , msg]

    return ()

reportFile :: Path Abs File
reportFile = makeAbsFile "/home/frank/reportBakeAll.txt"

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
    return $ unwords' ["\nbakeAllInSiteMD end", showT siteDough, showT reportFile1
            , "..........................", "ok"]

testMD :: Path Abs File -> Bool
testMD = hasExtension extMD

bakeOneFile2 :: Path Abs File -> ErrIO Text
-- a version which makes the path first  to use in Pipe
bakeOneFile2 fp2 =  do
    fpath :: Path Rel File  <- stripProperPrefix' doughPath fp2
--        : MonadThrow m => Path b Dir -> Path b t -> m (Path Rel t)
--    let   fnn = removeExtension fpath
    bakeOneFile fpath


bakeOneFile :: -- Path Rel Dir - Path Rel Dir ->
            Path Rel File -> ErrIO Text
-- convert a file (path relative to dough) and put in baked
bakeOneFile fp = do
            let   fnn = removeExtension fp :: Path Rel File
            putIOwords ["\n--------------------------------", "bakeOneFile fn", showT fnn, "\n\n"]
            -- currently only for md files, add static next

            intext :: MarkdownText <- read7 doughPath fnn markdownFileType



        --
        -- convert to html
            val  <- markdownToHTML3 intext
        --    let html1  =  HTMLout $  val ^.  key "content" . _String

        --    putIOwords ["bakeOneFile html1\n\n", unHTMLout html1]
            putIOwords ["bakeOneFile val\n\n", showNice val]

        --     apply template before writing
            let templateFN = makeAbsFile
                              "/home/frank/Workspace8/SSG/theme/templates/pandocDefault.html"
                              :: Path Abs File
            html2 <-  applyTemplate2 templateFN val

            putIOwords ["bakeOneFile resultFile", showT bakedPath, showT fnn, "\n"]
            write7 bakedPath fnn htmloutFileType html2
--            putIOwords ["bakeOneFile outhtml (which was just written) \n", unHTMLout html2, "\n"]

            putIOwords ["\n......................"]

            return . unwords' $  ["bakeOneFile outhtml ", showT fnn, "done", "\n\n"]

     `catchError` (\e -> do
                        putIOwords ["\n****************"
                                    , "bakeOneFile catchError", showT e , "for ", showT fp
                                    , "\n****************"]
                        return . unwords' $  ["bakeOneFile outhtml ", showT fp, "failed", "\n\n"]
                    )

stripProperPrefix' ::  Path b Dir -> Path b t -> ErrIO (Path Rel t)
stripProperPrefix' dir fn = fmap Path $ Path.stripProperPrefix (unPath dir) (unPath fn)
