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
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans #-}

module Lib.Shake2 where

import Uniform.Error (ErrIO, callIO, liftIO)
import Uniform.Shake
import Uniform.Strings (putIOwords, showT)

import Lib.Foundation
  ( SiteLayout(..)
  , resourcesDirName
  , staticDirName
  , templatesDirName
  , templatesImgDirName
  )

import Lib.Bake

bakeAll :: Path Rel File -> SiteLayout -> ErrIO ()
-- ^ bake all md files and copy the resources
-- sets the current dir to doughDir
bakeAll bannerImageFileName layout
         -- where the layout is used, rest in shakeWrapped
 = do
  let doughP = doughDir layout -- the regular dough
      templatesP = themeDir layout `addFileName` templatesDirName
      bakedP = bakedDir layout
  setCurrentDir doughP
  deleteDirRecursive bakedP
    -- copy resources and banner   not easy to do with shake
    -- only the html and the pdf files (possible the jpg) are required
  let bannerImage = templatesImgDirName `addFileName` bannerImageFileName
  copyOneFile
    (templatesP `addFileName` bannerImage)
    (bakedP </> staticDirName </> bannerImage)
    -- convert md files and copy css
  callIO $ shakeMD layout doughP templatesP bakedP
  return ()

--    copyDirRecursive (doughP `addDir` resourcesDirName)   (bakedP `addDir` staticDirName)
shakeMD :: SiteLayout -> Path Abs Dir -> Path Abs Dir -> Path Abs Dir -> IO ()
-- ^ process all md files (currently only the MD)
-- in IO
shakeMD layout doughP templatesP bakedP
--    shakeArgs2 bakedP $ do
 =
  shakeArgs
    shakeOptions
      { shakeFiles = toFilePath bakedP -- TODO
      , shakeVerbosity = Chatty -- Loud -- Diagnostic --
      , shakeLint = Just LintBasic
--                , shakeRebuild=[(RebuildNow,"allMarkdownConversion")]
--                  seems not to produce an effect
      } $ -- in Rule () 
   do
    let staticP = bakedP </> staticDirName -- ok
    let resourcesDir = doughP </> resourcesDirName
    liftIO $
      putIOwords
        [ "\nshakeMD dirs"
        , "\n\tstaticP"
        , showT staticP
        , "\n\tbakedP"
        , showT bakedP
        , "\nresourcesDir"
        , showT resourcesDir
        ]
    want ["allMarkdownConversion"]
    phony "allMarkdownConversion" $ do
      mdFiles1 :: [Path Rel File] <- getDirectoryFilesP doughP ["**/*.md"] -- subfiledirectories
      let htmlFiles3 =
            map ((replaceExtension' "html") . (\f -> bakedP </> f)) mdFiles1
                        -- [( bakedP </>  md) -<.> "html" | md <- mdFiles1] 
             :: [Path Abs File]
                            -- , not $ isInfixOf' "index.md" md]
            -- let htmlFiles3 = map (replaceExtension "html") htmlFiles2 :: [Path Abs File]
      liftIO $
        putIOwords
          ["============================\nshakeMD - mdFile 1", showT mdFiles1]
      liftIO $ putIOwords ["\nshakeMD - htmlFile 2", showT htmlFiles3]
      needP htmlFiles3
--             cssFiles1 <- getDirectoryFiles templatesD ["*.css"] -- no subdirs
--             let cssFiles2 = [replaceDirectory c staticP  | c <- cssFiles1]
--             liftIO $ putIOwords ["========================\nshakeMD - css files 1",  showT   cssFiles1]
--             liftIO $ putIOwords ["\nshakeMD - css files" ,  showT  cssFiles2]
--             need cssFiles2
--             pdfFiles1 <- getDirectoryFiles resourcesDir ["**/*.pdf"] -- subdirs
--             let pdfFiles2 = [ staticP </> c  | c <- pdfFiles1]
--             liftIO $ putIOwords ["===================\nshakeMD - pdf files1",  showT   pdfFiles1]
--             liftIO $ putIOwords ["\nshakeMD - pdf files 2",  showT  pdfFiles2]
--             need pdfFiles2
-- --
--             htmlFiles11<- getDirectoryFiles resourcesDir ["**/*.html"] -- subdirs
--             let htmlFiles22 = [  staticP </> c | c <- htmlFiles11]
--             liftIO $ putIOwords ["===================\nshakeMD - html 11 files",  showT   htmlFiles11]
--             liftIO $ putIOwords ["\nshakeMD - html 22 files", showT htmlFiles22]
--             need htmlFiles22
    (\x ->
       (((toFilePath bakedP) <> "**/*.html") ?== x) &&
       not (((toFilePath staticP) <> "**/*.html") ?== x) -- with subdir
     ) ?> \out
            -- liftIO $ putIOwords ["\nshakeMD - bakedP html -  out ", showT out]
            -- hakeMD - bakedP html -  out  "/home/frank/.SSG/bakedTest/SSGdesign/index.html"
     -> do
      let outP = makeAbsFile out :: Path Abs File
            -- --needs to know if this is abs or rel file !
            -- --liftIO $ putIOwords ["\nshakeMD - bakedP html -  out2 ", showT outP] 
      let md = replaceExtension' "md" outP :: Path Abs File --  <-    out2 -<.> "md"  
            -- liftIO $ putIOwords ["\nshakeMD - bakedP html 2 -  md ", showT md]
            -- --let md1 =  stripProperPrefixP bakedP md :: Path Rel File 
            -- l--iftIO $ putIOwords ["\nshakeMD - bakedP html 3 - md1 ", showT md1]
      let md2 = doughP </> (stripProperPrefixP bakedP md) :: Path Abs File
            -- liftIO $ putIOwords ["\nshakeMD - bakedP html 4 - md2 ", showT md2]
      res <- runErr2action $ bakeOneFile True md2 doughP templatesP outP
      return ()
    (toFilePath staticP <> "**/*.html") %> \out -- with subdir
     -> do
      let outP = makeAbsFile out :: Path Abs File
      liftIO $
        putIOwords
          [ "\nshakeMD - staticP ok - *.html"
          , showT staticP
          , "file"
          , showT outP
          , "out"
          , showT out
          ]
      let fromfile = resourcesDir </> (makeRelativeP staticP outP)
      liftIO $ putIOwords ["\nshakeMD - staticP - fromfile ", showT fromfile]
      copyFileChanged (toFilePath fromfile) out
        -- (staticP </> "*.css") %> \out ->  do           -- insert css -- no subdir
        --     liftIO $ putIOwords ["\nshakeMD - staticP - *.css", showT out]
        --     copyFileChanged (replaceDirectory out templatesD) out
        -- (staticP <> "**/*.pdf") %> \out ->  do           -- insert pdfFIles1 -- with subdir
        --     liftIO $ putIOwords ["\nshakeMD - staticP - *.pdf", showT out]
        --     let fromfile = resourcesDir </> (makeRelative staticP out)
        --     liftIO $ putIOwords ["\nshakeMD - staticP - fromfile ", showT fromfile]
        --     copyFileChanged fromfile out
-- /home/frank/bakedHomepageSSG/SSGdesign/index.html
    return ()
