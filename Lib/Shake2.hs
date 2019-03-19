
------------------------------------------------------------------------------
--
-- Module      :  with Path  the  process to convert
--              files in any input format to html
--              orginals are found in dire doughDir and go to bakeDir
--

-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans #-}

module Lib.Shake2
     where

import qualified Uniform.FileIO  as FIO -- hiding ((<.>), (</>))
import Uniform.FileIO (Path, Abs, Rel, File, Dir, (<.>), (</>), toFilePath)
import Uniform.Shake.Path (replaceExtension, needP, getDirectoryFilesP)
import Uniform.Shake 
import Uniform.Error (ErrIO, callIO, liftIO)
import Uniform.Strings (showT, putIOwords)
-- import Path 
import  Development.Shake ((?>), (?==), phony, want )
import qualified Development.Shake as Sh --  hiding ((<.>), (</>), (-<.>))
import qualified Development.Shake.FilePath  as ShD-- (toFilePath, makeAbsFile
--                , makeRelFile, makeRelDir, stripProperPrefix')
         hiding ((<.>), (</>), (-<.>) , replaceExtension)

import Lib.Foundation (SiteLayout (..)
        , templatesDirName,  templatesImgDirName
        , staticDirName, resourcesDirName)

import Lib.Bake

bakeAll :: Path Rel File  -> SiteLayout -> ErrIO ()
-- ^ bake all md files and copy the resources
-- sets the current dir to doughDir
bakeAll bannerImageFileName layout = do
    let  -- where the layout is used, rest in shakeWrapped
          doughP      =    doughDir  layout  -- the regular dough
          templatesP =   themeDir layout
                               `FIO.addFileName` templatesDirName
          bakedP =  bakedDir  layout
    FIO.setCurrentDir doughP
    FIO.deleteDirRecursive bakedP

    -- copy resources and banner   not easy to do with shake
    -- only the html and the pdf files (possible the jpg) are required
--    copyDirRecursive (doughP `addDir` resourcesDirName)   (bakedP `addDir` staticDirName)

    let bannerImage = templatesImgDirName `FIO.addFileName` bannerImageFileName

    FIO.copyOneFile (templatesP `FIO.addFileName` bannerImage)
        (bakedP `FIO.addDir` staticDirName `FIO.addDir` bannerImage)

    -- convert md files and copy css
    callIO $ shakeMD layout  doughP templatesP bakedP

    return ()

shakeMD :: SiteLayout -> Path Abs Dir  -> Path Abs Dir -> Path Abs Dir  -> IO ()
-- ^ process all md files (currently only the MD)
-- in IO
shakeMD layout  doughP templatesP bakedP =
--    shakeArgs2 bakedP $ do
    Sh.shakeArgs Sh.shakeOptions {Sh.shakeFiles=toFilePath bakedP -- TODO
                , Sh.shakeVerbosity=Sh.Chatty -- Loud -- Diagnostic --
                , Sh.shakeLint=Just Sh.LintBasic
--                , shakeRebuild=[(RebuildNow,"allMarkdownConversion")]
--                  seems not to produce an effect
                } $ do  -- in Rule () 

        -- let doughD = toFilePath doughP
        --     templatesD = toFilePath templatesP
        --     bakedP = toFilePath bakedP
        let staticP =   bakedP `FIO.addFileName`  staticDirName  -- ok
        let resourcesDir =   doughP `FIO.addFileName`  resourcesDirName

        liftIO $ putIOwords ["\nshake dirs", "\n\tstaticP", showT staticP, "\n\tbakedP", showT bakedP
                        ,"\nresourcesDir", showT resourcesDir]

        want ["allMarkdownConversion"]
        phony "allMarkdownConversion" $ do

            mdFiles1 :: [Path Rel File] <- getDirectoryFilesP  doughP ["**/*.md"]   -- subfiledirectories
            let htmlFiles2 = map (\f -> bakedP </> f) mdFiles1
                        -- [( bakedP </>  md) -<.> "html" | md <- mdFiles1] 
                                    :: [Path Abs File]
                            -- , not $ isInfixOf' "index.md" md]
            let htmlFiles3 = map (replaceExtension "html") htmlFiles2 :: [Path Abs File]
            liftIO $ putIOwords ["============================\nshakeWrapped - mdFile 1",  showT   mdFiles1]
            liftIO $ putIOwords ["\nshakeWrapped - htmlFile 2",  showT  htmlFiles3]
            needP htmlFiles3

--             cssFiles1 <- getDirectoryFiles templatesD ["*.css"] -- no subdirs
--             let cssFiles2 = [replaceDirectory c staticP  | c <- cssFiles1]
--             liftIO $ putIOwords ["========================\nshakeWrapped - css files 1",  showT   cssFiles1]
--             liftIO $ putIOwords ["\nshakeWrapped - css files" ,  showT  cssFiles2]
--             need cssFiles2

--             pdfFiles1 <- getDirectoryFiles resourcesDir ["**/*.pdf"] -- subdirs
--             let pdfFiles2 = [ staticP </> c  | c <- pdfFiles1]
--             liftIO $ putIOwords ["===================\nshakeWrapped - pdf files1",  showT   pdfFiles1]
--             liftIO $ putIOwords ["\nshakeWrapped - pdf files 2",  showT  pdfFiles2]
--             need pdfFiles2
-- --
--             htmlFiles11<- getDirectoryFiles resourcesDir ["**/*.html"] -- subdirs
--             let htmlFiles22 = [  staticP </> c | c <- htmlFiles11]
--             liftIO $ putIOwords ["===================\nshakeWrapped - html 11 files",  showT   htmlFiles11]
--             liftIO $ putIOwords ["\nshakeWrapped - html 22 files", showT htmlFiles22]
--             need htmlFiles22

        (\x -> (((toFilePath bakedP) <> "**/*.html") ?== x) 
                            && not  (((toFilePath staticP) <> "**/*.html") ?== x)) -- with subdir
                  ?> \out -> do
            let out2 = FIO.makeRelFile out  
            liftIO $ putIOwords ["\nshakeWrapped - bakedP html -  out ", showT out]
            let out3 = bakedP </> FIO.makeRelFile out :: Path Abs File
            let md = replaceExtension "md" out2 :: Path Rel File  --  <-    out2 -<.> "md"  
            let md2 = doughP </> md :: Path Abs File 
            liftIO $ putIOwords ["\nshakeWrapped - bakedP html - c ", showT bakedP, "file", showT md]
            let outP = FIO.makeAbsFile out 
            res <- runErr2action $ bakeOneFile True  md2  doughP templatesP outP
            return ()
        -- (staticP <> "**/*.html" ) %> \out -> do  -- with subdir
        --     liftIO $ putIOwords ["\nshakeWrapped - staticP ok - *.html", showT staticP, "file", showT out]
        --     let fromfile = resourcesDir </> (makeRelative staticP out)
        --     liftIO $ putIOwords ["\nshakeWrapped - staticP - fromfile ", showT fromfile]
        --     copyFileChanged fromfile out

        -- (staticP </> "*.css") %> \out ->  do           -- insert css -- no subdir
        --     liftIO $ putIOwords ["\nshakeWrapped - staticP - *.css", showT out]
        --     copyFileChanged (replaceDirectory out templatesD) out

        -- (staticP <> "**/*.pdf") %> \out ->  do           -- insert pdfFIles1 -- with subdir
        --     liftIO $ putIOwords ["\nshakeWrapped - staticP - *.pdf", showT out]
        --     let fromfile = resourcesDir </> (makeRelative staticP out)
        --     liftIO $ putIOwords ["\nshakeWrapped - staticP - fromfile ", showT fromfile]
        --     copyFileChanged fromfile out


-- /home/frank/bakedHomepageSSG/SSGdesign/index.html
        return ()

