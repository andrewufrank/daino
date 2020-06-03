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

{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans #-}

module Lib.ConvertFiles where

import           Uniform.Error (ErrIO, callIO, liftIO)
import           Uniform.Shake 
import           Development.Shake -- (Rules(..))
-- import          Uniform.Shake.Path
import           Uniform.Strings (putIOwords, showT)
import           Lib.Foundation (SiteLayout(..), resourcesDirName, staticDirName
                               , templatesDir, templatesImgDirName
                               , imagesDirName)
import           Lib.CmdLineArgs (PubFlags(..))
import           Lib.Bake (bakeOneFile)

      -- liftIO $ putIOwords ["\nshakeMD - bakedP html -  out ", showT out]
      -- hakeMD - bakedP html -  out  "/home/frank/.SSG/bakedTest/SSGdesign/index.html"
      -- called from produceHTML (not shake2)
produceMD2HTML :: Bool -> Path Abs Dir -> Path Abs Dir -> PubFlags -> SiteLayout -> FilePath -> Action () 
produceMD2HTML debug doughP bakedP flags layout out = do
    -- masterSettings_yaml   masterTemplate 

    let outP = makeAbsFile out :: Path Abs File

    let md = replaceExtension' "md" outP :: Path Abs File --  <-    out2 -<.> "md"  
    liftIO $ putIOwords ["\nproduceMD2HTML - bakedP html 2 -  md ", showT md]
    -- --let md1 =  stripProperPrefixP bakedP md :: Path Rel File 
    -- l--iftIO $ putIOwords ["\nshakeMD - bakedP html 3 - md1 ", showT md1]
    -- determine if this must be constructed from md 
    -- or copied as given
    let md2 = doughP </> stripProperPrefixP bakedP md :: Path Abs File

    -- liftIO $ putIOwords ["\nshakeMD - bakedP html 4 - md2 ", showT md2]
    need [toFilePath md2]  
    when debug $ liftIO $ putIOwords ["\nproduceMD2HTML - bakedP - *.html", showT outP, showT md2]
    

    res <- runErr2action $ bakeOneFile False flags md2 layout outP
    liftIO $ putIOwords ["\nproduceMD2HTML - return from bakeOneFile", showT res]
    return ()

produceHTML :: Bool -> Path Abs Dir -> Path Abs Dir -> PubFlags -> SiteLayout -> FilePath -> Action () 
-- the producers/convertes of the files         
produceHTML debug doughP bakedP flags layout out = do
    let outP = makeAbsFile out :: Path Abs File
    when debug $ liftIO
        $ putIOwords
            [ "\nproduceHTML  *.html"
            -- , showT staticP
            , "file out"
            -- , showT outP
            -- , "out"
            , showT out]
    let fromfile = doughP </> makeRelativeP bakedP outP
    xishtml   <-  liftIO $ runErr $ doesFileExist' fromfile
    let ishtml = case xishtml of 
                    Left msg -> errorT [msg] 
                    Right b -> b 
    when debug $ liftIO $ putIOwords ["\nproduceHTML - fromfile exist", showT ishtml
        , "\nfile", showT fromfile]
    if ishtml 
        then do 
            copyFileChangedP fromfile outP
            when debug $ liftIO $ putIOwords ["\n DONE produceHTML - staticP - fromfile ", showT 
                fromfile]
        else produceMD2HTML debug doughP bakedP flags layout out
    return () 

produceCSS debug doughP bakedP out = do
    let outP = makeAbsFile out :: Path Abs File
    when debug $ liftIO
        $ putIOwords
        [ "\nproduceCSS - staticP - *.css\n"
        , showT outP
        -- , "\nTemplatesP"
        -- , showT templatesP
        ]
    let fromfile = doughP </> makeRelativeP bakedP outP
    when debug $ liftIO
        $ putIOwords ["\nproduceCSS - staticP css- fromfile ", showT fromfile]
    copyFileChangedP fromfile outP

producePDF debug doughP bakedP out =  do
        let outP = makeAbsFile out :: Path Abs File
        when True $ liftIO $ putIOwords ["\nproducePDF - staticP - *.pdf", showT outP]
        let fromfile = doughP </> makeRelativeP bakedP outP
        when True $ liftIO
            $ putIOwords ["\nproducePDF - staticP  pdf - fromfile ", showT fromfile]
        copyFileChangedP fromfile outP
    -- return ()

produceJPG debug doughP bakedP out = do
        let outP = makeAbsFile out :: Path Abs File
        when debug $ liftIO $ putIOwords ["\nproducePDF - image jpg", showT outP]
        let fromfile = doughP </> makeRelativeP bakedP outP
        when debug $ liftIO
            $ putIOwords ["\nproducePDF - staticP  img=age jpg- fromfile ", showT fromfile]
        copyFileChangedP fromfile outP

produceBiblio debug doughP bakedP out = do
        let outP = makeAbsFile out :: Path Abs File
        when debug $ liftIO $ putIOwords ["\nproducePDF - biblio jpg", showT outP]
        let fromfile = doughP </> makeRelativeP bakedP outP
        when debug $ liftIO
            $ putIOwords ["\nproducePDF - biblio - fromfile ", showT fromfile]
        copyFileChangedP fromfile outP
 
-- produceBannerImage debug templatesP staticP out = do 
--         -- let bannerImage3 = makeRelFile out
--         let outP = makeAbsFile out 
--         when debug $ liftIO $ putIOwords ["\nproduceBannerImage - bannerImage TargetF", showT outP]
--         let fromfile = templatesP `addFileName` makeRelativeP staticP outP
--         when debug $ liftIO $ putIOwords ["\nproduceBannerImage - bannerImage fromfile ", showT fromfile]
--         copyFileChangedP fromfile outP

