------------------------------------------------------------------------------
--
-- Module      :  with Path  the  process to convert
--              files in any input format to html
--              orginals are found in dire doughDir and go to bakeDir
{-  die struktur geht von den files aus, die man braucht und 
    diese rekonstruieren die directories wieder, wenn sie kreiert werden.   

    welche directories nicht einschliessen: enthalten 
    um files aus direktories nicht einzuschliessen braucht es:
    - die files die in diesen gefunden werden, nicht zum umwandeln 
        anzumelden, indem deren namen nicht in "want" eingeschlossen 
        werden.

    
    -}
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans #-}

module Lib.Shake2 where

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
import Lib.FilesNeeded 
import Lib.ConvertFiles

shakeDelete :: SiteLayout -> FilePath -> ErrIO ()
-- ^ experimental - twich found delete of md
-- not yet used 

shakeDelete _ filepath = 
  putIOwords
    [ "\n\n*******************************************"
    , "experimental -- twich found  DELETED MD file "
    , s2t filepath]

shakeArgs2 :: Path b t -> Rules () -> IO ()
-- | set the options for shake  
shakeArgs2 bakedP = do
  -- putIOwords ["shakeArgs2", "bakedP", s2t . toFilePath $ bakedP]
    res <- shake  -- not shakeArgs, which would include the cmd line args
            shakeOptions { shakeFiles = toFilePath bakedP
                 , shakeVerbosity = Chatty -- Loud
                 , shakeLint = Just LintBasic
                 }
  -- putIOwords ["shakeArgs2", "done"]
    return res

shakeAll :: Bool -> SiteLayout -> PubFlags -> FilePath -> ErrIO ()
-- ^ bake all md files and copy the resources
-- sets the current dir to doughDir
-- copies banner image 

shakeAll debug layout flags filepath = 
  do 
    -- let debug = False
    --  where the layout is used, rest in shakeWrapped
    putIOwords  [ "\n\n=====================================shakeAll start", "\n flags"
            , showT flags , "caused by", s2t filepath, "."]
    let doughP = doughDir layout -- the regular dough
        templatesP = templatesDir layout 
        bakedP = bakedDir layout
        bannerImageFileName = (bannerImage layout)
        bannerImage2 = templatesImgDirName `addFileName` bannerImageFileName
    setCurrentDir doughP  
    callIO $ shakeMD debug layout flags doughP templatesP bakedP bannerImage2
    -- return ()

shakeMD :: Bool -> SiteLayout
        -> PubFlags
        -> Path Abs Dir
        -> Path Abs Dir
        -> Path Abs Dir
        -> Path Rel File 
        -> IO ()
-- ^ bake all md files and copy the resources
-- sets the current dir to doughDir
-- copies banner image 
-- in IO
-- TOP shake call 
shakeMD debug layout flags doughP templatesP bakedP bannerImage2 = shakeArgs2 bakedP $
  do
     
    let staticP = bakedP </> staticDirName :: Path Abs Dir
    -- should not be needed -- will be resourcesDirName
    let resourcesP = doughP </> resourcesDirName :: Path Abs Dir
    let masterTemplate = templatesP </> masterTemplateP :: Path Abs File
        masterTemplateP = makeRelFile "master4.dtpl" :: Path Rel File
        settingsYamlP = makeRelFile "settings2.yaml" :: Path Rel File
        masterSettings_yaml = doughP </> settingsYamlP :: Path Abs File
        imagesP = doughP </> resourcesDirName </> imagesDirName 
        imagesTargetP = staticP </> imagesDirName
    let bannerImageTarget = bakedP </> staticDirName </> bannerImage2
    -- let bannerImageFP =    bannerImage2
    
    liftIO $ putIOwords
        [ "\nshakeMD dirs\n"
        , "\n\tstaticP\n"
        , showT staticP
        , "\n\tbakedP\n"
        , showT bakedP
        , "\n\tresourcesDir\n"
        , showT resourcesP]
    want ["allMarkdownConversion"]

    phony "allMarkdownConversion" $ 
      do 
        -- these are functions to construct the desired results
        -- which then produce them
        pdfs <- bakePDF debug doughP bakedP
        htmls <- bakeStaticHTML debug doughP bakedP
        -- given html
        bibs <- bakeBiblio debug doughP bakedP
        imgs <- bakeImagesForBlog debug doughP bakedP
        csss <- bakeCSS debug templatesP staticP -- exception
        mds :: [Path Abs File] <-  bakeMDfiles debug doughP bakedP 
        -- given md
   
    -- convert to needs (perhaps wants better)
    -- no restriction on order    
    
        needP [bannerImageTarget]

        needP pdfs 
        needP htmls
        needP bibs 
        needP imgs
        needP csss 
        needP mds 
        -- -- moved from inside MD2HTML 
        needP [masterSettings_yaml]
        needP  [masterTemplate]
        -- need (map toFilePath yamlPageFiles2)
    return ()

    (toFilePath staticP <> "**/*.html") %> \out -- with subdirproduceHTML
            -> produceHTML debug staticP resourcesP out

 
    (toFilePath staticP <> "/*.css")  %> \out  -- insert css -- no subdir
      -> produceCSS debug templatesP staticP out 
        
    (toFilePath bakedP <> "**/*.pdf") %> \out -- insert pdfFIles1 
                                            -- with subdir
      -> producePDF debug doughP bakedP  out 
      
    [toFilePath imagesTargetP <> "/*.JPG"
      , toFilePath imagesTargetP <> "/*.jpg"]
                                    |%> \out -- insert img files 
                                            -- no subdir (for now)
      -> produceJPG debug imagesTargetP imagesP out

    toFilePath bannerImageTarget %> \out 
        -> produceBannerImage debug templatesP staticP out 
        

    -- conversion md to html (excet for what is in static) 
    (\x -> ((toFilePath bakedP <> "**/*.html") ?== x)
      && not ((toFilePath staticP <> "**/*.html") ?== x) -- with subdir
      )  ?> \out -> produceMD2HTML debug bakedP doughP 
                        -- masterSettings_yaml masterTemplate 
                        flags layout out 


