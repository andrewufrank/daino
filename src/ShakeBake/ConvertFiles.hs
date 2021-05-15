---------------------------------------------------------------
--
-- Module      :
---------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-unused-matches #-}

{- | convert the files and put in targe dir
              input is target filename
          this is the interface (only one) from shake to bake
-}
module ShakeBake.ConvertFiles where

import ShakeBake.Bake
-- ( BakeOp,
--   bakeOneFile2docrep,
--   bakeOneFile2panrep,
--   bakeOneFile2html,
--   bakeOneFile2texsnip,
--   bakeOneFile2tex,
--   bakeOneFile2pdf )
import Lib.CmdLineArgs (PubFlags (..))
import Foundational.Foundation 
import Foundational.Filetypes4sites 
import Uniform.Pandoc  

import Uniform.Shake

type ConvertOp =
    NoticeLevel ->
    Path Abs Dir ->
    Path Abs Dir ->
    PubFlags ->
    SiteLayout ->
    FilePath ->
    Action ()

type ConvertA2BOp =
    NoticeLevel ->
    Path Abs Dir ->
    Path Abs Dir ->
    PubFlags ->
    SiteLayout ->
    FilePath ->
    Extension ->
    BakeOp ->
    Action ()

convMD2docrep :: ConvertOp
convMD2docrep debug doughP bakedP flags layout out =
    convA2B debug doughP bakedP flags layout out extMD bakeOneMD2docrep


convDocrep2panrep :: ConvertOp
convDocrep2panrep debug doughP bakedP flags layout out =
    convA2B debug doughP bakedP flags layout out extDocrep bakeOneDocrep2panrep

-- convPanrep2html :: ConvertOp
-- -- convPanrep2html :: NoticeLevel
-- --     -> Path Abs Dir
-- --     -> Path Abs Dir
-- --     -> PubFlags
-- --     -> SiteLayout
-- --     -> FilePath
-- --     -> Action ()
-- convPanrep2html debug doughP bakedP flags layout out =
--     convA2B debug doughP bakedP flags layout out extPanrep bakeOnePanrep2html

-- convPanrep2html :: ConvertOp
-- convPanrep2html debug doughP bakedP flags layout out =
--     convA2B debug doughP bakedP flags layout out extPanrep bakeOnePanrep2html

convPanrep2panrep1 :: ConvertOp
convPanrep2panrep1 debug doughP bakedP flags layout out =
    convA2B debug doughP bakedP flags layout out extPanrep1 bakeOnePanrep2panrep1

convPanrep12html :: ConvertOp
convPanrep12html debug doughP bakedP flags layout out =
    convA2B debug doughP bakedP flags layout out extPanrep bakeOnePanrep12html

convPanrep2texsnip :: ConvertOp
convPanrep2texsnip debug doughP bakedP flags layout out =
    convA2B debug doughP bakedP flags layout out extPanrep bakeOnePanrep2texsnip

convTexsnip2tex :: ConvertOp
convTexsnip2tex debug doughP bakedP flags layout out =
    convA2B debug doughP bakedP flags layout out extTexSnip bakeOneTexsnip2tex

convTex2pdf :: ConvertOp
convTex2pdf debug doughP bakedP flags layout out =
    convA2B debug doughP bakedP flags layout out extTex bakeOneTex2pdf

convA2B :: ConvertA2BOp
-- ^ produce the B files from A
convA2B debug sourceP targetP flags layout out sourceExtA bakeop = do
    when (inform debug) $ putIOwords ["\n  convA2B   ", showT sourceExtA, showT out]
    let outP = makeAbsFile out :: Path Abs File

    let infile1 =
            replaceExtension' (s2t . unExtension $ sourceExtA) outP :: Path Abs File
    when (inform debug) $ putIOwords ["\n  convA2B   2   ", showT infile1]
    needP [infile1]

    let infile2 = sourceP </> stripProperPrefixP targetP infile1 :: Path Abs File
    need [toFilePath infile2]
    when (inform debug) $
        putIOwords
            ["\n  convA2B - 3 needed", showPretty infile2]

    resfile <-
        runErr2action $
            bakeop NoticeLevel0 flags infile2 layout outP
    when (inform debug) $ putIOwords ["\n  convA2B - return 3", showT resfile]
    return ()

io2bool :: MonadIO m => ErrIO b -> m b
io2bool op = do
    -- todo move
    x <- liftIO $ runErr op
    let res = case x of
            Left msg -> errorT [msg]
            Right b -> b
    return res

convertAny ::
    NoticeLevel ->
    Path Abs Dir ->
    Path Abs Dir ->
    PubFlags ->
    SiteLayout ->
    FilePath ->
    ConvertOp   -- ^ the operation to carry out
    -> Text  -- ^ the name of the operation
    -> Action ()
-- produce any (either copy available in baked or produce with anyop)
convertAny debug sourceP targetP flags layout out anyop anyopName = do
    putIOwords [ "\n-----------------", "convertAny for", anyopName]
    let outP = makeAbsFile out :: Path Abs File
    when (inform debug) $ putIOwords ["\nproduceAny", "\n file out", showT out]
    if sourceP == targetP
        then anyop debug sourceP targetP flags layout out
        else do
            let fromfile = sourceP </> makeRelativeP targetP outP
            -- needP [fromfile]
            fileExists <- io2bool $ doesFileExist' fromfile
            when (inform debug) $
                putIOwords
                    [ "\nconvertAny - fromfile exist:"
                    , showT fileExists
                    , "\nfile"
                    , showT fromfile
                    ]
            if fileExists -- gives recursion, if the file is produced in earlier run
                then do
                    copyFileChangedP fromfile outP
                    when (inform debug) $
                        liftIO $
                            putIOwords
                                ["\n convertAny DONE   - staticP - fromfile ", showT fromfile]
                else anyop debug sourceP targetP flags layout out
            return ()
    when (inform debug) $ putIOwords ["convertAny end for", anyopName]

-- | the generic copy for all the files
-- which can just be copied
-- (exceptions md, which are a special case of needed)
copyFileToBaked :: (Filenames3 fp (Path Rel File),
      FileResultT fp (Path Rel File) ~ Path Abs File) =>
        NoticeLevel -> fp -> Path Abs Dir -> FilePath -> Action ()
copyFileToBaked debug doughP bakedP out = do
    let outP = makeAbsFile out :: Path Abs File
    when True $ liftIO $ putIOwords ["\ncopyFileToBaked outP", showT outP]
    let fromfile = doughP </> makeRelativeP bakedP outP
    when (inform debug) $
        liftIO $
            putIOwords
                ["\ncopyFileToBaked fromfile ", showT fromfile]
    copyFileChangedP fromfile outP
