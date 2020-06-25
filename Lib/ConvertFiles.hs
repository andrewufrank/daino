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
-- import           Development.Shake -- (Rules(..))
-- import          Uniform.Shake.Path
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

-- type ConvertA2B = Bool -> Path Abs Dir -> Path Abs Dir
--                         -> PubFlags -> SiteLayout  -> FilePath  -> Action ()

-- convMD2HTML :: ConvertOp
--     -- :: Bool
--     -- -> Path Abs Dir
--     -- -> Path Abs Dir
--     -- -> PubFlags
--     -- -> SiteLayout
--     -- -> FilePath
--     -- -> Action ()
-- -- ^ produce the html file (separated from pdf)
-- convMD2HTML debug doughP bakedP flags layout out = do

--     let outP = makeAbsFile out :: Path Abs File

--     let md   = replaceExtension' "md" outP :: Path Abs File --  <-    out2 -<.> "md"  
--     liftIO $ putIOwords ["\nproduceMD2HTML - bakedP html 2 -  md ", showT md]
--     let md2 = doughP </> stripProperPrefixP bakedP md :: Path Abs File

--     need [toFilePath md2]
--     when debug $ liftIO $ putIOwords
--         ["\nproduceMD2HTML - bakedP - *.html", showT outP, showT md2]

--     -- let debug2 = True 

--     resHtml <- runErr2action $ do
--         bakeOneFile2docval False flags md2 layout outP
--         -- the docval are written into the baked
--         bakeDocValue2html False flags outP layout outP
--     putIOwords
--         [ "\nproduceMD2HTML - return from bakeOneFile2html"
--         , showT resHtml
--         , "/n--------------/n"
--         ]

    -- resPdf <- runErr2action $ bakeOneFile2pdf True flags outP layout outP
    -- the handling of the extension is by the file types
    -- but the result must not be the same name as the md file
    -- liftIO $ putIOwords ["\nproduceMD2HTML - return from bakeOneFile2pdf", showT resTex]
    -- return ()

-- convMD2PDF :: ConvertOp
--     -- :: Bool
--     -- -> Path Abs Dir
--     -- -> Path Abs Dir
--     -- -> PubFlags
--     -- -> SiteLayout
--     -- -> FilePath
--     -- -> Action ()
-- -- ^ produce the PDF and the texsnip file
-- -- TODO break out the docval production and the texsnip 
-- convMD2PDF debug doughP bakedP flags layout out = do
--     putIOwords ["\nproduceMD2PDF - 1  -  md ", showT out]
--     let outP = makeAbsFile out :: Path Abs File

--     let md   = replaceExtension' "md" outP :: Path Abs File --  <-    out2 -<.> "md"  
--     liftIO $ putIOwords ["\nproduceMD2PDF - bakedP html 2 -  md ", showT md]
--     let md2 = doughP </> stripProperPrefixP bakedP md :: Path Abs File

--     need [toFilePath md2]
--     when debug $ liftIO $ putIOwords
--         ["\nproduceMD2PDF - bakedP - *.texsnip", showT outP, showT md2]

--     resTex <- runErr2action $ bakeOneFile2texsnip False flags md2 layout outP
--     liftIO $ putIOwords
--         ["\nproduceMD2PDF - return from bakeOneFile2texsnip", showT resTex]

--     resPdf <- runErr2action $ do
--         bakeOneTexSnip2pdf True flags outP layout outP
--     -- the handling of the extension is by the file types
--     -- but the result must not be the same name as the md file
--     liftIO $ putIOwords
--         ["\nproduceMD2PDF - return from bakeOneFile2pdf", showT resTex]
--     return ()

-- convMD2docrep :: ConvertOp
-- -- ^ produce the docrep   file
-- -- TODO include the index construction here 
-- convMD2docrep debug doughP bakedP flags layout out = do
--     putIOwords ["\n convMD2docrep - 1  -  md ", showT out]
--     let outP = makeAbsFile out :: Path Abs File

--     let md   = replaceExtension' "md" outP :: Path Abs File --  <-    out2 -<.> "md"  
--     liftIO $ putIOwords ["\n convMD2docrep - bakedP html 2 -  md ", showT md]

--     let md2 = doughP </> stripProperPrefixP bakedP md :: Path Abs File
--     need [toFilePath md2]
--     when debug $ liftIO $ putIOwords
--         ["\n convMD2docrep - bakedP - *.texsnip", showT outP, showT md2]

--     resTex <- runErr2action $ bakeOneFile2docrep False flags md2 layout outP
--     liftIO $ putIOwords
--         ["\n convMD2docrep - return from bakeOneFile2docrep", showT resTex]
--     return ()


convMD2docrep :: ConvertOp
convMD2docrep debug doughP bakedP flags layout out =
    convA2B debug doughP bakedP flags layout out extMD bakeOneFile2docrep

convDocrep2html :: ConvertOp
convDocrep2html debug doughP bakedP flags layout out =
    convA2B debug doughP bakedP flags layout out extDocRep bakeOneFile2html

convDocrep2texsnip :: ConvertOp
convDocrep2texsnip debug doughP bakedP flags layout out =
    convA2B debug doughP bakedP flags layout out extDocRep bakeOneFile2texsnip

convTexsnip2tex :: ConvertOp
convTexsnip2tex debug doughP bakedP flags layout out =
    convA2B debug doughP bakedP flags layout out extTexSnip bakeOneFile2tex

convTex2pdf :: ConvertOp
convTex2pdf debug doughP bakedP flags layout out =
    convA2B debug doughP bakedP flags layout out extTex bakeOneFile2pdf


convA2B :: ConvertA2BOp
-- ^ produce the B files from A 
convA2B debug doughP bakedP flags layout out extA bakeop = do
    putIOwords ["\n  convA2B   ", showT extA, showT out]
    let outP = makeAbsFile out :: Path Abs File

    let infile1 =
            replaceExtension' (s2t . unExtension $ extA) outP :: Path Abs File --  <-    out2 -<.> "md"  
    liftIO $ putIOwords ["\n  convA2B   2   ", showT infile1]

    let infile2 = doughP </> stripProperPrefixP bakedP infile1 :: Path Abs File
    need [toFilePath infile2]
    when debug $ liftIO $ putIOwords
        ["\n  convA2B - 3", showT outP, showT infile2]

    resfile <- runErr2action
        $ bakeOneFile2docrep False flags infile2 layout outP
    liftIO $ putIOwords ["\n  convA2B - return 3", showT resfile]
    return ()


conv2HTML :: ConvertOp
-- the producers/convertes of the files         
conv2HTML debug doughP bakedP flags layout out =
    convertAny debug doughP bakedP flags layout out convDocrep2html

conv2PDF :: ConvertOp
-- produce pdf (either copy available or produce from texsnip )         
conv2PDF debug doughP bakedP flags layout out =
    convertAny debug doughP bakedP flags layout out convTex2pdf

conv2tex :: ConvertOp
-- produce pdf (either copy available or produce from texsnip )         
conv2tex debug doughP bakedP flags layout out =
    convertAny debug doughP bakedP flags layout out convTexsnip2tex

conv2texsnip :: ConvertOp
-- produce pdf (either copy available or produce from texsnip )         
conv2texsnip debug doughP bakedP flags layout out =
    convertAny debug doughP bakedP flags layout out convDocrep2texsnip

conv2docrep :: ConvertOp
-- produce pdf (either copy available or produce from texsnip )         
conv2docrep debug doughP bakedP flags layout out =
    convertAny debug doughP bakedP flags layout out convMD2docrep


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
-- produce any (either copy available or produce with anyop)         
convertAny debug doughP bakedP flags layout out anyop = do
    let outP = makeAbsFile out :: Path Abs File
    when debug $ liftIO $ putIOwords ["\nproduceAny", "\n file out", showT out]
    let fromfile = doughP </> makeRelativeP bakedP outP
    needP [fromfile]
    fileExists <- io2bool $ doesFileExist' fromfile
    when debug $ liftIO $ putIOwords
        [ "\nproducePDF - fromfile exist:"
        , showT fileExists
        , "\nfile"
        , showT fromfile
        ]
    if fileExists
        then do
            copyFileChangedP fromfile outP
            when debug $ liftIO $ putIOwords
                ["\n DONE producePDF - staticP - fromfile ", showT fromfile]
        else anyop True doughP bakedP flags layout out
    return ()

-- the generic copy for all the files 
-- which can just be copied 
-- (exceptions md, which are a special case of needed)
copyFileToBaked debug doughP bakedP out = do
    let outP = makeAbsFile out :: Path Abs File
    when debug $ liftIO $ putIOwords ["\ncopyFileToBaked outP", showT outP]
    let fromfile = doughP </> makeRelativeP bakedP outP
    when debug $ liftIO $ putIOwords
        ["\ncopyFileToBaked fromfile ", showT fromfile]
    copyFileChangedP fromfile outP


