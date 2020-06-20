------------------------------------------------------------------------------
--
-- Module      :  convert the files and put in targe dir 
--              input is target filename
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

import           Uniform.Error (liftIO)
import           Uniform.Shake 
-- import           Development.Shake -- (Rules(..))
-- import          Uniform.Shake.Path
import           Uniform.Strings (putIOwords, showT)
import           Lib.Foundation (SiteLayout(..)
                -- , resourcesDirName, staticDirName
                --                , templatesDir, templatesImgDirName
                --                , imagesDirName
                               )
import           Lib.CmdLineArgs (PubFlags(..))
import           Lib.Bake 
        -- (bakeOneFile2html, bakeOneFile2texsnip, bakeOneFile2pdf)

produceMD2HTML :: Bool -> Path Abs Dir -> Path Abs Dir -> PubFlags -> SiteLayout -> FilePath -> Action () 
-- ^ produce the html file (separated from pdf)
produceMD2HTML debug doughP bakedP flags layout out = do

    let outP = makeAbsFile out :: Path Abs File

    let md = replaceExtension' "md" outP :: Path Abs File --  <-    out2 -<.> "md"  
    liftIO $ putIOwords ["\nproduceMD2HTML - bakedP html 2 -  md ", showT md]
    let md2 = doughP </> stripProperPrefixP bakedP md :: Path Abs File

    need [toFilePath md2]  
    when debug $ liftIO $ putIOwords ["\nproduceMD2HTML - bakedP - *.html", showT outP, showT md2]
    
    -- let debug2 = True 
    
    resHtml <- runErr2action $ do 
                bakeOneFile2docval False flags md2 layout outP
                -- the docval are written into the baked
                bakeDocValue2html False flags outP layout outP
    liftIO $ putIOwords ["\nproduceMD2HTML - return from bakeOneFile2html", showT resHtml]

    -- resPdf <- runErr2action $ bakeOneFile2pdf True flags outP layout outP
    -- the handling of the extension is by the file types
    -- but the result must not be the same name as the md file
    -- liftIO $ putIOwords ["\nproduceMD2HTML - return from bakeOneFile2pdf", showT resTex]
    return ()

produceMD2PDF :: Bool -> Path Abs Dir -> Path Abs Dir -> PubFlags -> SiteLayout -> FilePath -> Action () 
-- ^ produce the PDF and the texsnip file
-- TODO break out the docval production and the texsnip 
produceMD2PDF debug doughP bakedP flags layout out = do

    let outP = makeAbsFile out :: Path Abs File

    let md = replaceExtension' "md" outP :: Path Abs File --  <-    out2 -<.> "md"  
    liftIO $ putIOwords ["\nproduceMD2PDF - bakedP html 2 -  md ", showT md]
    let md2 = doughP </> stripProperPrefixP bakedP md :: Path Abs File

    need [toFilePath md2]  
    when debug $ liftIO $ putIOwords ["\nproduceMD2PDF - bakedP - *.texsnip", showT outP, showT md2]

    resTex <- runErr2action $ bakeOneFile2texsnip  False flags md2 layout outP
    liftIO $ putIOwords ["\nproduceMD2PDF - return from tex", showT resTex]

    resPdf <- runErr2action $ do 
                    bakeOneFile2texsnip True flags outP layout outP
    -- the handling of the extension is by the file types
    -- but the result must not be the same name as the md file
    liftIO $ putIOwords ["\nproduceMD2PDF - return from bakeOneFile2pdf", showT resTex]
    return ()

produceHTML :: Bool -> Path Abs Dir -> Path Abs Dir -> PubFlags -> SiteLayout -> FilePath -> Action () 
-- the producers/convertes of the files         
produceHTML debug doughP bakedP flags layout out = do
    let outP = makeAbsFile out :: Path Abs File
    when debug $ liftIO
        $ putIOwords [ "\nproduceHTML  *.html"
            , "file out", showT out]
    let fromfile = doughP </> makeRelativeP bakedP outP
    xishtml   <-  liftIO $ runErr $ doesFileExist' fromfile
    let ishtml = case xishtml of 
                    Left msg -> errorT [msg] 
                    Right b -> b 
    when debug $ liftIO $ putIOwords ["\nproduceHTML - fromfile exist"
        , showT ishtml, "\nfile", showT fromfile]
    if ishtml 
        then do 
            copyFileChangedP fromfile outP
            when debug $ liftIO $ putIOwords ["\n DONE produceHTML - staticP - fromfile ", showT 
                fromfile]
        else produceMD2HTML True doughP bakedP flags layout out
    return () 

producePDF :: Bool -> Path Abs Dir -> Path Abs Dir -> PubFlags -> SiteLayout -> FilePath -> Action () 
-- the producers/convertes of the files         
producePDF debug doughP bakedP flags layout out = do
    let outP = makeAbsFile out :: Path Abs File
    when debug $ liftIO
        $ putIOwords [ "\nproducePDF  *.html"
            , "file out", showT out]
    let fromfile = doughP </> makeRelativeP bakedP outP
    xishtml   <-  liftIO $ runErr $ doesFileExist' fromfile
    let ishtml = case xishtml of 
                    Left msg -> errorT [msg] 
                    Right b -> b 
    when debug $ liftIO $ putIOwords ["\nproducePDF - fromfile exist"
        , showT ishtml, "\nfile", showT fromfile]
    if ishtml 
        then do 
            copyFileChangedP fromfile outP
            when debug $ liftIO $ putIOwords ["\n DONE producePDF - staticP - fromfile ", showT 
                fromfile]
        else  produceMD2PDF True doughP bakedP flags layout out
    return () 

-- the generic copy for all the files 
-- which can just be copied 
-- (exceptions md, which are a special case of needed)
copyFileToBaked debug doughP bakedP out = do
        let outP = makeAbsFile out :: Path Abs File
        when debug $ liftIO $ 
            putIOwords ["\ncopyFileToBaked outP", showT outP]
        let fromfile = doughP </> makeRelativeP bakedP outP
        when debug $ liftIO $ 
            putIOwords ["\ncopyFileToBaked fromfile ", showT fromfile]
        copyFileChangedP fromfile outP


