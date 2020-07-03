------------------------------------------------------------------------------
--
-- Module      :  convert the files and put in targe dir 
--              input is target filename
--          this is the interface (only one) from shake to bake 
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports 
            -fno-warn-unused-imports 
            -fno-warn-unused-matches 
            #-}

module Lib.ConvertFiles where

import           Uniform.Error                  ( liftIO )
import           Uniform.Shake
import           Uniform.Strings                ( putIOwords
                                                , showT
                                                )
import           Lib.Foundation                 ( SiteLayout(..)
                -- , resourcesDirName, staticDirName
                --                , templatesDir, templatesImgDirName
                --                , imagesDirName
                                                )
import           Lib.CmdLineArgs                ( PubFlags(..) )
import           Lib.Bake
        -- (bakeOneFile2html, bakeOneFile2texsnip, bakeOneFile2pdf)
import           Uniform.Pandoc

type ConvertOp
    =  Bool
    -> Path Abs Dir
    -> Path Abs Dir
    -> PubFlags
    -> SiteLayout
    -> FilePath
    -> Action ()

type ConvertA2BOp
    =  Bool
    -> Path Abs Dir
    -> Path Abs Dir
    -> PubFlags
    -> SiteLayout
    -> FilePath
    -> Extension
    -> BakeOp
    -> Action ()


convMD2docrep :: ConvertOp
convMD2docrep debug doughP bakedP flags layout out = do
    convA2B debug doughP bakedP flags layout out extMD (bakeOneFile2docrep doughP bakedP)
    -- needs1 <- docRepNeeds2  -- questionable 
    -- need needs1

convDocrep2panrep :: ConvertOp
convDocrep2panrep debug doughP bakedP flags layout out = do
    convA2B debug doughP bakedP flags layout out extDocRep (bakeOneFile2panrep bakedP)

-- convPanrep2html :: ConvertOp
convPanrep2html debug doughP bakedP flags layout out =
    convA2B debug doughP bakedP flags layout out extPanrep (bakeOneFile2html)

convPanrep2texsnip :: ConvertOp
convPanrep2texsnip debug doughP bakedP flags layout out =
    convA2B debug doughP bakedP flags layout out extPanrep bakeOneFile2texsnip

convTexsnip2tex :: ConvertOp
convTexsnip2tex debug doughP bakedP flags layout out =
    convA2B debug doughP bakedP flags layout out extTexSnip bakeOneFile2tex

convTex2pdf :: ConvertOp
convTex2pdf debug doughP bakedP flags layout out =
    convA2B debug doughP bakedP flags layout out extTex bakeOneFile2pdf


convA2B :: ConvertA2BOp
-- ^ produce the B files from A 
convA2B debug sourceP targetP flags layout out sourceExtA bakeop = do
    putIOwords ["\n  convA2B   ", showT sourceExtA, showT out]
    let outP = makeAbsFile out :: Path Abs File

    let infile1 =
            replaceExtension' (s2t . unExtension $ sourceExtA) outP :: Path Abs File 
    putIOwords ["\n  convA2B   2   ", showT infile1]
    needP [infile1]

    let infile2 = sourceP </> stripProperPrefixP targetP infile1 :: Path Abs File
    need [toFilePath infile2]
    when debug $  putIOwords
        ["\n  convA2B - 3 needed", showT infile2 ]

    resfile <- runErr2action
        $ bakeop False flags infile2 layout outP
    liftIO $ putIOwords ["\n  convA2B - return 3", showT resfile]
    return ()



io2bool op = do         -- todo move 
    x <- liftIO $ runErr $ op
    let res = case x of
            Left  msg -> errorT [msg]
            Right b   -> b
    return res

convertAny
    :: Bool
    -> Path Abs Dir
    -> Path Abs Dir
    -> PubFlags
    -> SiteLayout
    -> FilePath
    -> ConvertOp   -- ^ the operation to carry out 
    -> Action ()
-- produce any (either copy available in baked or produce with anyop)         
convertAny debug sourceP targetP flags layout out anyop = do
    let outP = makeAbsFile out :: Path Abs File
    when debug $   putIOwords ["\nproduceAny", "\n file out", showT out]

    if sourceP == targetP 
        then anyop True sourceP targetP flags layout out
        else 
          do  
            let fromfile = sourceP </> makeRelativeP targetP outP
            -- needP [fromfile]
            fileExists <- io2bool $ doesFileExist' fromfile
            when debug $  putIOwords
                [ "\nconvertAny - fromfile exist:"
                , showT fileExists
                , "\nfile"
                , showT fromfile
                ]
            if fileExists  -- gives recursion, if the file is produced in earlier run
                then do
                    copyFileChangedP fromfile outP
                    when debug $ liftIO $ putIOwords
                        ["\n convertAny DONE   - staticP - fromfile ", showT fromfile]
                else anyop True sourceP targetP flags layout out
            return ()

-- the generic copy for all the files 
-- which can just be copied 
-- (exceptions md, which are a special case of needed)
copyFileToBaked debug doughP bakedP out = do
    let outP = makeAbsFile out :: Path Abs File
    when True $ liftIO $ putIOwords ["\ncopyFileToBaked outP", showT outP]
    let fromfile = doughP </> makeRelativeP bakedP outP
    when debug $ liftIO $ putIOwords
        ["\ncopyFileToBaked fromfile ", showT fromfile]
    copyFileChangedP fromfile outP


