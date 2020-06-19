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
import           Lib.Bake (bakeOneFile2html, bakeOneFile2tex, bakeOneFile2pdf)

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
    
    let debug2 = True 
    resHtml <- runErr2action $ bakeOneFile2html False flags md2 layout outP
    liftIO $ putIOwords ["\nproduceMD2HTML - return from bakeOneFile2html", showT resHtml]

    resTex <- runErr2action $ bakeOneFile2tex  False flags md2 layout outP
    liftIO $ putIOwords ["\nproduceMD2HTML - return from bakeOneFile2html", showT resTex]

    let filenameProper = removeExtension (toFilePath outP)
    let standaloneFn = makeAbsFile (filenameProper <> "Doc")
    liftIO $ putIOwords ["\nproduceMD2HTML - return from bakeOneFile2pdf", showT resTex]    
    resPdf <- runErr2action $ bakeOneFile2pdf True flags outP layout outP
    -- the handling of the extension is by the file types
    -- but the result must not be the same name as the md file
    liftIO $ putIOwords ["\nproduceMD2HTML - return from bakeOneFile2pdf", showT resTex]
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
        else produceMD2HTML True doughP bakedP flags layout out
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


