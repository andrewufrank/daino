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

import Foundational.Filetypes4sites
import Foundational.LayoutFlags
import Uniform.Pandoc

import Uniform.Shake

type ConvertOp =
    NoticeLevel ->
    Path Abs Dir ->
    Path Abs Dir ->
    PubFlags ->
    Settings ->
    FilePath ->
    Action ()

type ConvertA2BOp =
    NoticeLevel ->
    Path Abs Dir ->
    Path Abs Dir ->
    PubFlags ->
    Settings->
    FilePath ->
    Extension ->
    BakeOp ->
    Action ()

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
    Settings ->
    FilePath ->
    -- -- | the operation to carry out
    -- ConvertOp ->
    -- | the name of the operation
    Text ->
    Action ()
-- produce any (either copy available in baked or produce with anyop)
convertAny debug sourceP targetP flags layout out anyopName = do
    putIOwords ["-----------------", "convertAny for", anyopName]
    let outP = makeAbsFile out :: Path Abs File
    when (True) $ putIOwords ["\nconvertAny 1", "\n file out", showT out]
    let (anyop, sourceExtA) = case anyopName of 
            "convMD2docrep" -> (bakeOneMD2docrep, extMD)
            "convDocrep2panrep" -> (bakeOneDocrep2panrep, extDocrep)
            "convPanrep2texsnip" -> (bakeOnePanrep2texsnip, extPanrep )
            "convPanrep2html" -> (bakeOnePanrep2html, extPanrep )
            "convTex2pdf" -> (bakeOneTex2pdf, extTex )
            "convTexsnip2tex" -> (bakeOneTexsnip2tex, extTexSnip )
            _  -> errorT ["convertAny error unknown anyopName ", anyopName]

    let fromfilePath = sourceP </> makeRelativeP targetP outP
    --  same filename, path to source: for case where file exists and needs to be copied 
    let fromfilePathExt = replaceExtension' (s2t . unExtension $ sourceExtA) fromfilePath 
    -- source extension - case: to produce from this by conv 

    putIOwords ["\nconvertAny 2", anyopName
                , "extension", (s2t . unExtension $ sourceExtA)
                ,  "\n fromfilePath", showT fromfilePath, " was causing NEED"   
                ,  "\n fromfilePathExt", showT fromfilePathExt   
                ,  "\n file out", showT out
                ] 

    fileExists <- io2bool $ doesFileExist' fromfilePath  --targetExt
    when (inform debug) $
        putIOwords
            [ "\nconvertAny - fromfile exist:"
            , showT fileExists
            , "\nfile"
            , showT fromfilePath
            ]
    if fileExists 
        -- gives recursion, if the file is produced in earlier run
            -- should only be case for jpg and publist? 
            -- pdf,html,jpg is copide in shake2
        then do
            copyFileChangedP fromfilePath outP
            when (True) $
                liftIO $
                    putIOwords
                        ["\n convertAny  copied"
                         ,   "\n\tfromfilePath ", showT fromfilePath
                         ,  "\n\t  file out", showT out]
        else do
            putIOwords ["\nconvertAny call", anyopName
                ,  "\n\t fromfilePathExt", showT fromfilePathExt, " cause NEED"   
                ,  "\n\t file out", showT out
                ] 
            need [toFilePath fromfilePathExt]    

            runErr2action $ anyop debug flags fromfilePathExt layout outP
    return ()
    when (inform debug) $ putIOwords ["convertAny end for", anyopName]

{- | the generic copy for all the files
 which can just be copied
 (exceptions md, which are a special case of needed)
-}
copyFileToBaked ::
    ( Filenames3 fp (Path Rel File)
    , FileResultT fp (Path Rel File) ~ Path Abs File
    ) =>
    NoticeLevel ->
    fp ->
    Path Abs Dir ->
    FilePath ->
    Action ()
copyFileToBaked debug doughP bakedP out = do
    let outP = makeAbsFile out :: Path Abs File
    when (inform debug) $ liftIO $ putIOwords ["\ncopyFileToBaked outP", showT outP]
    let fromfile = doughP </> makeRelativeP bakedP outP
    when (inform debug) $
        liftIO $
            putIOwords
                ["\ncopyFileToBaked fromfile ", showT fromfile]
    copyFileChangedP fromfile outP
