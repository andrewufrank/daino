------------------------------------------------------------------------------
--
-- Module      :  with Path  the  process to convert
--              files in any input format to html
--              orginals are found in dire doughDir and go to bakeDir
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans #-}

module Lib.Shake2 where

import           Uniform.Error (ErrIO, callIO, liftIO)
import           Uniform.Shake 
import           Development.Shake -- (Rules(..))
import          Uniform.Shake.Path
import           Uniform.Strings (putIOwords, showT)
import           Lib.Foundation (SiteLayout(..), resourcesDirName, staticDirName
                               , templatesDir, templatesImgDirName
                               , imagesDirName)
import           Lib.CmdLineArgs (PubFlags(..))
import           Lib.Bake (bakeOneFile)

shakeDelete :: SiteLayout -> FilePath -> ErrIO ()
-- ^ experimental - twich found delete of md
-- not yet used 

shakeDelete _ filepath = 
  putIOwords
    [ "\n\n*******************************************"
    , "experimental -- twich found  DELETED MD file "
    , s2t filepath]

shakeArgs2 :: Path b t -> Rules () -> IO ()

-- | set the arguments for shake and call the ruls 
shakeArgs2 bakedP = do
  -- putIOwords ["shakeArgs2", "bakedP", s2t . toFilePath $ bakedP]
    res <- shake  -- not shakeArgs, which would include the cmd line args
            shakeOptions { shakeFiles = toFilePath bakedP
                 , shakeVerbosity = Chatty -- Loud
                 , shakeLint = Just LintBasic
                 }
  -- putIOwords ["shakeArgs2", "done"]
    return res

shakeAll :: SiteLayout -> PubFlags -> FilePath -> ErrIO ()
-- ^ bake all md files and copy the resources
-- sets the current dir to doughDir
-- copies banner image 

shakeAll layout flags filepath = 
  do 
    --  where the layout is used, rest in shakeWrapped
    putIOwords  [ "\n\n=====================================shakeAll start", "\n flags"
            , showT flags , "caused by", s2t filepath]
    let doughP = doughDir layout -- the regular dough
        templatesP = templatesDir layout 
        bakedP = bakedDir layout
        bannerImageFileName = (bannerImage layout)
        bannerImage2 = templatesImgDirName `addFileName` bannerImageFileName
    setCurrentDir doughP  
    callIO $ shakeMD layout flags doughP templatesP bakedP bannerImage2
    -- return ()

shakeMD :: SiteLayout
        -> PubFlags
        -> Path Abs Dir
        -> Path Abs Dir
        -> Path Abs Dir
        -> Path Rel File 
        -> IO ()
-- ^ process all md files (currently only the MD)
-- in IO

shakeMD layout flags doughP templatesP bakedP bannerImage2 = 
  shakeArgs2 bakedP $
    do
      let debug = True
      let staticP = bakedP </> staticDirName :: Path Abs Dir
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
          [ "\nshakeMD dirs"
          , "\n\tstaticP"
          , showT staticP
          , "\n\tbakedP"
          , showT bakedP
          , "\nresourcesDir"
          , showT resourcesP]
      want ["allMarkdownConversion"]
      phony "allMarkdownConversion" $ 
        do
     
          

          pdfFiles1 :: [Path Rel File]
            <- getDirectoryFilesP resourcesP ["**/*.pdf"] -- subdirs
          let pdfFiles2 = [staticP </> c | c <- pdfFiles1]
          when debug $ liftIO
            $ putIOwords
              ["===================\nshakeMD - pdf files1", showT pdfFiles1]
          when debug $ liftIO $ putIOwords ["\nshakeMD - pdf files 2", showT pdfFiles2]
          needP pdfFiles2
          -- --
          -- static html files 
          htmlFiles11 :: [Path Rel File]
            <- getDirectoryFilesP resourcesP ["**/*.html"] -- subdirs
          let htmlFiles22 = [staticP </> c | c <- htmlFiles11]
          when debug $ liftIO
            $ putIOwords
              ["===================\nshakeMD - html 11 files", showT htmlFiles11]
          when debug $ liftIO $ putIOwords ["\nshakeMD - html 22 files", showT htmlFiles22]
          needP htmlFiles22

          biblio :: [Path Rel File] <- getDirectoryFilesP resourcesP ["*.bib"]
          let biblio2 = [resourcesP </> b | b <- biblio] :: [Path Abs File]
          when debug $ putIOwords ["shake bakedP", "biblio", showT biblio2]
          needP biblio2

          yamlPageFiles <- getDirectoryFilesP templatesP ["*.yaml"]
          let yamlPageFiles2 = [templatesP </> y | y <- yamlPageFiles]
          -- when debug $ 
          putIOwords ["===================\nshakeMD", "yamlPages", showT yamlPageFiles2]

          -- images for blog 
          imgFiles :: [Path Rel File]
              <- getDirectoryFilesP imagesP ["*.JPG", "*.jpg"]  -- no subdirs (may change in future)
          let imagesFiles2 = [imagesTargetP </> i  | i <- imgFiles]
          when debug $ putIOwords ["===================\nshakeMD", "shake imgFiles", showT imagesP, "found", showT imagesFiles2]
          needP imagesFiles2

          cssFiles1 :: [Path Rel File]
            <- getDirectoryFilesP templatesP ["*.css"] -- no subdirs
          let cssFiles2 = [staticP </> c | c <- cssFiles1] :: [Path Abs File]
          when debug $ liftIO
            $ putIOwords
              [ "========================\nshakeMD - css files 1"
              , showT cssFiles1]
          when debug $ liftIO $ putIOwords ["\nshakeMD - css files", showT cssFiles2]
          needP cssFiles2

          -- cssFiles22 :: [Path Rel File]
          --   <- getDirectoryFilesP templatesP ["*.css"] -- no subdirs
          -- liftIO
          --   $ putIOwords
          --     ["===================\nshakeMD - cssFiles1 ", showT cssFiles22]
          -- -- let cssFiles2 = [replaceDirectoryP templatesP staticP c | c <- cssFiles1]  -- flipped args
          -- let cssFiles3 = [staticP </> c | c <- cssFiles22] -- flipped args
          -- liftIO $ putIOwords ["***", if cssFiles3 == cssFiles2 then "" 
          --                 else "******************************************"] 
          -- files which are copied and not influence the bake 
          needP [bannerImageTarget]


          mdFiles1 :: [Path Rel File]
            <- getDirectoryFilesP doughP ["**/*.md"] -- subfiledirectories
          let htmlFiles3 = map (replaceExtension' "html" . (bakedP </>)) mdFiles1
                :: [Path Abs File]
                -- [( bakedP </>  md) -<.> "html" | md <- mdFiles1] 
                
          -- , not $ isInfixOf' "index.md" md]
          -- let htmlFiles3 = map (replaceExtension "html") htmlFiles2 :: [Path Abs File]
          when debug $ liftIO
            $ putIOwords
              [ "============================\nshakeMD - htmlFiles3 1"
              -- , showT mdFiles1]
          -- when debug $ liftIO $ putIOwords ["\nshakeMD - htmlFile "
                , showT htmlFiles3]
          -- needP mdFiles1
          needP htmlFiles3  -- includes the index files 
   

      (toFilePath staticP <> "**/*.html")
        %> \out -- with subdir
        -> do
          let outP = makeAbsFile out :: Path Abs File
          liftIO
            $ putIOwords
              [ "\nshakeMD - staticP ok - *.html"
              , showT staticP
              , "file"
              , showT outP
              , "out"
              , showT out]
          let fromfile = resourcesP </> makeRelativeP staticP outP
          liftIO $ putIOwords ["\nshakeMD - staticP - fromfile ", showT fromfile]
          --copyFileChangedP fromfile outP

      (toFilePath staticP <> "/*.css")
        %> \out                  -- insert css -- no subdir
        -> do
          let outP = makeAbsFile out :: Path Abs File
          liftIO
            $ putIOwords
              [ "\nshakeMD - staticP - *.css\n"
              , showT outP
              , "\nTemplatesP"
              , showT templatesP]
          let fromfile = templatesP </> makeRelativeP staticP outP
          liftIO
            $ putIOwords ["\nshakeMD - staticP css- fromfile ", showT fromfile]
          copyFileChangedP fromfile outP
          
      (toFilePath staticP <> "**/*.pdf")
        %> \out                  -- insert pdfFIles1 -- with subdir
        -> do
          let outP = makeAbsFile out :: Path Abs File
          liftIO $ putIOwords ["\nshakeMD - staticP - *.pdf", showT outP]
          let fromfile = resourcesP </> makeRelativeP staticP outP
          liftIO
            $ putIOwords ["\nshakeMD - staticP  pdf - fromfile ", showT fromfile]
          copyFileChangedP fromfile outP
      -- return ()

      [toFilePath imagesTargetP <> "/*.JPG"
        , toFilePath imagesTargetP <> "/*.jpg"]
        |%> \out                  -- insert img files -- no subdir (for now)
        -> do
          let outP = makeAbsFile out :: Path Abs File
          liftIO $ putIOwords ["\nshakeMD - image jpg", showT outP]
          let fromfile = imagesP </> makeRelativeP imagesTargetP outP
          liftIO
            $ putIOwords ["\nshakeMD - staticP  img=age jpg- fromfile ", showT fromfile]
          copyFileChangedP fromfile outP
      -- return ()

      toFilePath bannerImageTarget %> \out -> do 
          -- let bannerImage3 = makeRelFile out
          let outP = makeAbsFile out 
          liftIO $ putIOwords ["\nshakeMD - bannerImage TargetF", showT outP]
          let fromfile = templatesP `addFileName` makeRelativeP staticP outP
          liftIO
            $ putIOwords ["\nshakeMD - bannerImage fromfile ", showT fromfile]
          copyFileChangedP fromfile outP
    
    
      (\x -> ((toFilePath bakedP <> "**/*.html") ?== x)
        && not ((toFilePath staticP <> "**/*.html") ?== x) -- with subdir
        )  ?> \out ->
        -- liftIO $ putIOwords ["\nshakeMD - bakedP html -  out ", showT out]
        -- hakeMD - bakedP html -  out  "/home/frank/.SSG/bakedTest/SSGdesign/index.html"
        do
          let outP = makeAbsFile out :: Path Abs File
          -- --needs to know if this is abs or rel file !
          -- --liftIO $ putIOwords ["\nshakeMD - bakedP html -  out2 ", showT outP] 
          let md = replaceExtension' "md" outP :: Path Abs File --  <-    out2 -<.> "md"  
          -- liftIO $ putIOwords ["\nshakeMD - bakedP html 2 -  md ", showT md]
          -- --let md1 =  stripProperPrefixP bakedP md :: Path Rel File 
          -- l--iftIO $ putIOwords ["\nshakeMD - bakedP html 3 - md1 ", showT md1]
          let md2 = doughP </> stripProperPrefixP bakedP md :: Path Abs File
          -- liftIO $ putIOwords ["\nshakeMD - bakedP html 4 - md2 ", showT md2]
          need [toFilePath md2]  
          liftIO $ putIOwords ["\nshakeMD - bakedP - *.html", showT outP, showT md2]
          
          need [toFilePath masterSettings_yaml]
          need [toFilePath masterTemplate]
          -- need (map toFilePath yamlPageFiles2)

          res <- runErr2action $ bakeOneFile False flags md2 layout outP
          return ()


  -- return ()
  -- copyFileChangedP source destDir = copyFileChanged (toFilePath source) (toFilePath destDir)
