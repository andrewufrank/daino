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
{-# LANGUAGE TypeOperators #-}
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
import Foundational.SettingsPage
import Foundational.CmdLineFlags

import Uniform.Pandoc

import Uniform.Shake
    ( ErrIO,
      Text,
      when,
      toFilePath,
      errorT,
      runErr,
      inform, informAll, NoticeLevel(..),
      makeAbsFile,
      unExtension,
      s2t,
      putIOwords,
      showT,
      MonadIO(..),
      Path,
      Abs,
      Dir,
      File,
      Rel,
      NoticeLevel,
      FileOps(doesFileExist'),
      Filenames3((</>), FileResultT),
      need,
      (<.>),
      copyFileChangedP,
    --   replaceExtension',
      runErr2action,
      Action,
      Path2nd(makeRelativeP) )
-- import UniformBase


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
    -- | the name of the operation
    Text ->
    Action ()
-- produce any (either copy available in baked or produce with anyop)
convertAny debug sourceP targetP flags layout out anyopName = do
    -- let debug = NoticeLevel0   -- avoid_output_fromHere_down

    putInform NoticeLevel1 ["-----------------", anyopName, "convertAny for", s2t out]
    let outP = makeAbsFile out :: Path Abs File
    putInform debug ["\nconvertAny 1", "\n file out", showT out]
    let (anyop, sourceExtA) = case anyopName of 
            "convMD2docrep" -> (bakeOneMD2docrep, extMD)
            "convDocrep2panrep" -> (bakeOneDocrep2panrep, extDocrep)
            "convPanrep2texsnip" -> (bakeOnePanrep2texsnip, extPanrep )
            "convPanrep2html" -> (bakeOnePanrep2html, extPanrep )
            "convTex2pdf" -> (bakeOneTex2pdf, extTex )
            "convTexsnip2tex" -> (bakeOneTexsnip2tex, extTexSnip )
            _  -> errorT ["convertAny error unknown anyopName ", anyopName]

    let from_filePath = sourceP </> makeRelativeP targetP outP :: Path Abs File
        from_filePathExt = from_filePath <.> ( sourceExtA)
    -- let from_filePathExt = replaceExtension' (s2t . unExtension $ sourceExtA) from_filePath 

    putInform debug 
        ["\nconvertAny 2", anyopName
        , "extension", (s2t . unExtension $ sourceExtA)
        ,  "\n from_filePath", showT from_filePath
        , " was causing NEED"   
        ,  "\n from_filePathExt", showT from_filePathExt   
        ,  "\n file out", showT out
        ] 

    fileExists <-  if sourceP == targetP 
        then return False 
        else io2bool $ doesFileExist' from_filePath  --targetExt

    when (inform debug) $
        putIOwords
            [ "\nconvertAny - fromfile exist:"
            , showT fileExists
            -- , "\nfile"
            -- , showT from_filePath
            ]
    if fileExists 
        -- gives recursion, if the file is produced in earlier run
        then do  -- copy file from source to target
            copyFileChangedP from_filePath outP
            when (inform debug) $
                -- liftIO $
                    putIOwords
                        ["\n convertAny  copied"
                         ,   "\n\tfrom_filePath ", showT from_filePath, "added NEED automatically"
                         ,  "\n\t  file out", showT out]
        else do
            putInform debug 
                ["\nconvertAny call", anyopName
                -- ,  "\n\t from_filePathExt"
                    ,  " cause NEED for" ,showT from_filePathExt  
                        -- should be from doughP
                ,  "\n\t file out", showT out
                ] 
            need [toFilePath from_filePathExt]    
            putInform debug 
                ["\nconvertAny runErr2Action", anyopName
                ,  "\n\t from_filePathExt",  " caused NEED which was then probably satisfied for ", showT from_filePathExt   
                ,  "\n\t file out", showT out
                ]
            needsFound <- runErr2action $ anyop debug flags from_filePathExt layout outP
            when ((inform debug) && needsFound /= []) $ putIOwords 
                ["\nconvertAny runErr2Action", anyopName
                ,  "\n\t needs found", showT needsFound
                ] 
                -- add here a generic tester 
                -- remove the tests from other places            
            need needsFound
    putInform NoticeLevel1 ["-----------------------convertAny end", anyopName, "for", s2t out]
    return ()

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
    putInform debug
                ["\ncopyFileToBaked fromfile ", showT fromfile, "added NEED automatically"]
    copyFileChangedP fromfile outP
