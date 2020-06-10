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
import           Development.Shake (Rules, (|%>))
-- import          Development.Shake.FilePath (replaceExtensions)
import           Uniform.Strings (putIOwords, showT)
import           Lib.Foundation (SiteLayout(..)
                             , staticDirName
                               )
import           Lib.CmdLineArgs (PubFlags(..))
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
        -- templatesP = templatesDir layout 
        bakedP = bakedDir layout
        -- bannerImageFileName = (bannerImage layout)
        -- bannerImage2 = templatesImgDirName `addFileName` bannerImageFileName
    setCurrentDir doughP  
    callIO $ shakeMD debug layout flags doughP  bakedP 
    -- return ()

shakeMD :: Bool -> SiteLayout
        -> PubFlags
        -> Path Abs Dir -- dough (source for files)
        -> Path Abs Dir -- baked (target dir for site)
        -- -> Path Abs Dir
        -- -> Path Rel File 
        -> IO ()
-- ^ bake all md files and copy the resources
-- sets the current dir to doughDir
-- copies banner image 
-- in IO
-- TOP shake call 
shakeMD debug layout flags doughP bakedP = shakeArgs2 bakedP $
  do
    -- the special filenames which are necessary
    -- because the file types are not automatically 
    -- copied 
     
    -- let staticP = bakedP </> staticDirName :: Path Abs Dir
    -- should not be needed -- will be resourcesDirName
    -- let resourcesP = doughP </> resourcesDirName :: Path Abs Dir
    -- let 
        -- masterTemplate = templatesP </> masterTemplateP :: Path Abs File
        -- masterTemplateP = makeRelFile "master4.dtpl" :: Path Rel File
        -- settingsYamlP = makeRelFile "settings2.yaml" :: Path Rel File
        -- masterSettings_yaml = doughP </> settingsYamlP :: Path Abs File
        -- imagesP = doughP </> resourcesDirName </> imagesDirName 
        -- imagesTargetP = staticP </> imagesDirName
    -- let bannerImageTarget = bakedP </> staticDirName </> bannerImage2
    -- let bannerImageFP =    bannerImage2
    
    liftIO $ putIOwords
        [ "shakeMD dirs\n"
        , "\tstaticDirName"
        , showT staticDirName
        , "\tbakedP\n"
        , showT bakedP
        -- , "\n\tresourcesDir\n"
        -- , showT resourcesP
        ]
    want ["allMarkdownConversion"]

    phony "allMarkdownConversion" $ 
      do 
        -- these are functions to construct the desired results
        -- which then produce them
        pdfs <- getNeeds debug doughP bakedP "pdf" "pdf"
        htmls <- getNeeds debug doughP bakedP "html" "html"
        -- given html
        bibs <- getNeeds debug doughP bakedP "bib" "bib"
        imgs <- getNeeds debug doughP bakedP "jpg" "jpg"
        imgs2 <- getNeeds debug doughP bakedP "JPG" "JPG"
                 
        csss <- getNeeds debug doughP bakedP "css" "css"
                -- templatesP 
                -- (bakedP </> staticDirName) -- exception
        mds :: [Path Abs File] <-  getNeeds debug doughP bakedP "md" "html"
        -- given md
        csls <- getNeeds debug doughP bakedP "csl" "csl"
    -- convert to needs (perhaps wants better)
    -- no restriction on order    
    
        -- needP [bannerImageTarget]

        needP pdfs 
        needP htmls
        needP bibs 
        needP imgs
        needP imgs2
        needP csss 
        needP csls
        needP mds 
        --  
        -- needP [masterSettings_yaml] -- checks only that file exists
        -- needP  [masterTemplate]
        -- need (map toFilePath yamlPageFiles2)
    return ()

    let debug2 = True 

    (toFilePath bakedP <> "**/*.html") %> \out 
        -- calls the copy html and the conversion from md
        -> produceHTML debug doughP bakedP flags layout out

 
    (toFilePath (bakedP) <> "/*.css")  %> \out  -- insert css -- no subdir
      -> copyFileToBaked debug2 doughP bakedP out 
    (toFilePath (bakedP) <> "/*.csl")  %> \out  -- insert css -- no subdir
        -> copyFileToBaked debug2 doughP bakedP out 
                -- templatesP 
                -- (bakedP </> staticDirName) out 
        
    (toFilePath bakedP <> "**/*.pdf") %> \out -- insert pdfFIles1 
                                            -- with subdir
        -> copyFileToBaked debug2 doughP bakedP  out 
      
    [toFilePath bakedP <> "/*.JPG"
      , toFilePath bakedP <> "/*.jpg"]
            |%> \out -- insert img files 
                                            -- no subdir (for now)
        -> copyFileToBaked debug2 doughP bakedP out

    (toFilePath bakedP <> "**/*.bib") %> \out 
        -> copyFileToBaked debug2 doughP bakedP out 
        

    -- -- conversion md to html (excet for what is in static) 
    -- (\x -> ((toFilePath bakedP <> "**/*.html") ?== x)
    --   && not ((toFilePath staticP <> "**/*.html") ?== x) -- with subdir
    --   )  ?> \out -> produceMD2HTML debug2 bakedP doughP 
    --                     -- masterSettings_yaml masterTemplate 
    --                     flags layout out 


getNeeds :: Bool -> Path Abs Dir -> Path Abs Dir -> Text -> Text -> Action [Path Abs File]
-- ^ find the files which are needed
--  from source with extension ext
getNeeds debug doughP bakedP extSource extTarget = do
    let sameExt  = extSource == extTarget
    when debug $ liftIO
        $ putIOwords
            ["===================\ngetNeeds extSource", extSource
                , "extTarget", extSource
                , "sameExt", showT sameExt]

    filesWithSource :: [Path Rel File]
        <- getDirectoryToBake "DNB" doughP [ ("**/*." <> t2s extSource)] 
                -- subdirs
    let filesWithTarget = if sameExt 
        then
             [bakedP </> c | c <- filesWithSource]
        else map 
            (replaceExtension' extTarget . (bakedP </>)) filesWithSource 
                    :: [Path Abs File]
    when debug $ liftIO $ do 
        putIOwords
            ["===================\nbakePDF -  source files 1"
                , "for ext", extSource, "files\n", showT filesWithSource]
        putIOwords ["\nbakePDF -  target files 2"
                , "for ext", extTarget, "files\n", showT filesWithTarget]
    return filesWithTarget


