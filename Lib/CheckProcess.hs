------------------------------------------------------------------------------
--
-- Module      :  the process to check the input files 
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

module Lib.CheckProcess where

import           Uniform.Error (ErrIO, callIO, liftIO)
-- import           Uniform.Shake 
-- import           Development.Shake -- (Rules(..))
-- import          Uniform.Shake.Path
import           Uniform.Strings (putIOwords, showT)
import           Lib.Foundation (SiteLayout(..))
                --   resourcesDirName, staticDirName
                --             --    , templatesDir, templatesImgDirName
                --                , imagesDirName)
import Lib.ReadSettingFile (readSettings)
-- import           Lib.Bake (bakeOneFile)
-- import qualified Pipes as Pipe
-- import  Pipes ((>->))
-- import qualified Pipes.Prelude as PipePrelude
-- import qualified Path.IO  (searchable, readable)
import Uniform.Filenames
import Uniform.Piped (pipedDoIO)
-- import Uniform.FileStrings
import Uniform.FileStrings (readFile2)
import Lib.CheckInput (getTripleDoc, TripleDoc)

checkProcess :: Bool -> FilePath  -> ErrIO ()
-- ^ checking all md files 

checkProcess debug filepath = do 
    let settingsFileName =  makeAbsFile filepath  
    (layout2, _) <- readSettings settingsFileName
    let doughP = doughDir layout2 -- the regular dough
    putIOwords ["\nstart with \n", "settingsFileName", showT settingsFileName 
                , "\ndoughDir", showT doughP
                , "\nfilepath", showT filepath]

    fns <- allFilenames3 doughP
    putIOwords ["the filenames\n", showList' . lines' $ fns]

    -- trp <- allTriples layout2 doughP 
    -- putIOwords ["the triples\n", showT . take' 100 .  lines' $ trp]

    when debug $ putIOwords
        [ "\n\n*******************************************"
        , "all md files checked\n"
        , s2t filepath]


allFilenames3 :: Path Abs Dir -> ErrIO (Text) 
allFilenames3 dirname = do 
        pipedDoIO resfil2 dirname showT
        readFile2 resfil2 
resfil2 = makeAbsFile "/home/frank/Workspace8/ssg/docs/site/resfile2.txt" :: Path Abs File

-- allTriples :: SiteLayout -> Path Abs Dir -> ErrIO (Text) 
-- allTriples layout dirname = do 
--         pipedDoIO alltriplesfile dirname (opx layout)
--         readFile2 alltriplesfile 
-- alltriplesfile = makeAbsFile "/home/frank/Workspace8/ssg/docs/site/alltriples.txt" :: Path Abs File

-- -- opx :: (SiteLayout -> Path Abs File -> Text) 
-- opx layout fn = showList' . map showTriple . getTripleDoc layout $ fn

showTriple :: TripleDoc -> Text
showTriple (a,b,c) =  concat' abc 
    where abc = [showT a, showT b, showT c] :: [Text] 
-- -- using pipe to go recursively through all the files
-- -- started with the code from uniform-fileio - pipes
-- -- which I did not see how to generalize, but should be possible

-- getRecursiveContents2 :: -- (Path Abs File-> Pipe.Proxy Pipe.X () () String (ErrorT Text IO) ())
--                   Path Abs Dir
--                       -> Pipe.Proxy Pipe.X () () (Path Abs File) (ErrorT Text IO) ()
-- getRecursiveContents2  fp = do
-- --    putIOwords ["recurseDir start", showT fp]
--     perm <-Pipe.lift $ getPermissions' fp
--     if not (Path.IO.readable perm && Path.IO.searchable perm)
--         then Pipe.lift $ putIOwords ["recurseDir not readable or not searchable", showT fp]
--         else do
--             symLink <- Pipe.lift $ checkSymbolicLink fp -- callIO $ xisSymbolicLink fp
--             if symLink
--                 then  Pipe.lift $ putIOwords ["recurseDir symlink", showT fp]
--                 else do
--                     (dirs, files) <- Pipe.lift $ listDir'  fp
--                     when False $ do
--                         Pipe.lift $ putIOwords ["recurseDir files\n", showT files]
--                         Pipe.lift $ putIOwords ["recurseDir directories\n", showT dirs]

--                     -- Prelude.mapM_ Pipe.yield (sort files)
-- --                                (Path.IO.sort (map unPath files))
--                     -- Prelude.mapM_ getRecursiveContents (sort dirs)
-- --                            (Path.IO.sort (map unPath dirs))
--                     return ()--    where processOneFile fp = Pipe.yield fp


-- -- | set the arguments for shake and call the ruls 
-- shakeArgs2 bakedP = do
--   -- putIOwords ["shakeArgs2", "bakedP", s2t . toFilePath $ bakedP]
--     res <- shake  -- not shakeArgs, which would include the cmd line args
--             shakeOptions { shakeFiles = toFilePath bakedP
--                  , shakeVerbosity = Chatty -- Loud
--                  , shakeLint = Just LintBasic
--                  }
--   -- putIOwords ["shakeArgs2", "done"]
--     return res

-- shakeAll :: SiteLayout -> PubFlags -> FilePath -> ErrIO ()
-- -- ^ bake all md files and copy the resources
-- -- sets the current dir to doughDir
-- -- copies banner image 

-- shakeAll layout flags filepath = 
--   do 
--     let debug = False
--     --  where the layout is used, rest in shakeWrapped
--     putIOwords  [ "\n\n=====================================shakeAll start", "\n flags"
--             , showT flags , "caused by", s2t filepath, "."]
--     let doughP = doughDir layout -- the regular dough
--         templatesP = templatesDir layout 
--         bakedP = bakedDir layout
--         bannerImageFileName = (bannerImage layout)
--         bannerImage2 = templatesImgDirName `addFileName` bannerImageFileName
--     setCurrentDir doughP  
--     callIO $ shakeMD layout flags doughP templatesP bakedP bannerImage2
--     -- return ()

-- shakeMD :: SiteLayout
--         -> PubFlags
--         -> Path Abs Dir
--         -> Path Abs Dir
--         -> Path Abs Dir
--         -> Path Rel File 
--         -> IO ()
-- -- ^ process all md files (currently only the MD)
-- -- in IO

-- shakeMD layout flags doughP templatesP bakedP bannerImage2 = 
--   shakeArgs2 bakedP $
--     do
--       let debug = False
--       let staticP = bakedP </> staticDirName :: Path Abs Dir
--       let resourcesP = doughP </> resourcesDirName :: Path Abs Dir
--       let masterTemplate = templatesP </> masterTemplateP :: Path Abs File
--           masterTemplateP = makeRelFile "master4.dtpl" :: Path Rel File
--           settingsYamlP = makeRelFile "settings2.yaml" :: Path Rel File
--           masterSettings_yaml = doughP </> settingsYamlP :: Path Abs File
--           imagesP = doughP </> resourcesDirName </> imagesDirName 
--           imagesTargetP = staticP </> imagesDirName
--       let bannerImageTarget = bakedP </> staticDirName </> bannerImage2
--         -- let bannerImageFP =    bannerImage2
      
--       liftIO $ putIOwords
--           [ "\nshakeMD dirs"
--           , "\n\tstaticP"
--           , showT staticP
--           , "\n\tbakedP"
--           , showT bakedP
--           , "\n\tresourcesDir"
--           , showT resourcesP]
--       want ["allMarkdownConversion"]
--       phony "allMarkdownConversion" $ 
--         do
     
          

--           pdfFiles1 :: [Path Rel File]
--             <- getDirectoryFilesP resourcesP ["**/*.pdf"] -- subdirs
--           let pdfFiles2 = [staticP </> c | c <- pdfFiles1]
--           when debug $ liftIO
--             $ putIOwords
--               ["===================\nshakeMD - pdf files1", showT pdfFiles1]
--           when debug $ liftIO $ putIOwords ["\nshakeMD - pdf files 2", showT pdfFiles2]
--           needP pdfFiles2
--           -- --
--           -- static html files 
--           htmlFiles11 :: [Path Rel File]
--             <- getDirectoryFilesP resourcesP ["**/*.html"] -- subdirs
--           let htmlFiles22 = [staticP </> c | c <- htmlFiles11]
--           when debug $ liftIO
--             $ putIOwords
--               ["===================\nshakeMD - html 11 files", showT htmlFiles11]
--           when debug $ liftIO $ putIOwords ["\nshakeMD - html 22 files", showT htmlFiles22]
--           needP htmlFiles22

--           biblio :: [Path Rel File] <- getDirectoryFilesP resourcesP ["*.bib"]
--           let biblio2 = [resourcesP </> b | b <- biblio] :: [Path Abs File]
--           when debug $ putIOwords ["shake bakedP", "biblio", showT biblio2]
--           needP biblio2

--           yamlPageFiles <- getDirectoryFilesP templatesP ["*.yaml"]
--           let yamlPageFiles2 = [templatesP </> y | y <- yamlPageFiles]
--           when debug $ 
--             putIOwords ["===================\nshakeMD", "yamlPages", showT yamlPageFiles2]

--           -- images for blog 
--           imgFiles :: [Path Rel File]
--               <- getDirectoryFilesP imagesP ["*.JPG", "*.jpg"]  -- no subdirs (may change in future)
--           let imagesFiles2 = [imagesTargetP </> i  | i <- imgFiles]
--           when debug $ putIOwords ["===================\nshakeMD", "shake imgFiles", showT imagesP, "found", showT imagesFiles2]
--           needP imagesFiles2

--           cssFiles1 :: [Path Rel File]
--             <- getDirectoryFilesP templatesP ["*.css"] -- no subdirs
--           let cssFiles2 = [staticP </> c | c <- cssFiles1] :: [Path Abs File]
--           when debug $ liftIO
--             $ putIOwords
--               [ "========================\nshakeMD - css files 1"
--               , showT cssFiles1]
--           when debug $ liftIO $ putIOwords ["\nshakeMD - css files", showT cssFiles2]
--           needP cssFiles2

--           -- cssFiles22 :: [Path Rel File]
--           --   <- getDirectoryFilesP templatesP ["*.css"] -- no subdirs
--           -- liftIO
--           --   $ putIOwords
--           --     ["===================\nshakeMD - cssFiles1 ", showT cssFiles22]
--           -- -- let cssFiles2 = [replaceDirectoryP templatesP staticP c | c <- cssFiles1]  -- flipped args
--           -- let cssFiles3 = [staticP </> c | c <- cssFiles22] -- flipped args
--           -- liftIO $ putIOwords ["***", if cssFiles3 == cssFiles2 then "" 
--           --                 else "******************************************"] 
--           -- files which are copied and not influence the bake 
--           needP [bannerImageTarget]


--           mdFiles1 :: [Path Rel File]
--             <- getDirectoryFilesP doughP ["**/*.md"] -- includes subfiledirectories
--           let htmlFiles3 = map (replaceExtension' "html" . (bakedP </>)) mdFiles1
--                 :: [Path Abs File]
--                 -- [( bakedP </>  md) -<.> "html" | md <- mdFiles1] 
                
--           -- , not $ isInfixOf' "index.md" md]
--           -- let htmlFiles3 = map (replaceExtension "html") htmlFiles2 :: [Path Abs File]
--           when debug $  liftIO
--             $ putIOwords
--               [ "============================\nshakeMD - mdFiles1"
--               , showT mdFiles1]
--           when debug $ 
--             liftIO $ putIOwords ["\nshakeMD - htmlFile3 x"
--                 , showT htmlFiles3]
--           -- needP mdFiles1
--           needP htmlFiles3  -- includes the index files 
   

--       (toFilePath staticP <> "**/*.html")
--         %> \out -- with subdir
--         -> do
--           let outP = makeAbsFile out :: Path Abs File
--           when debug $ liftIO
--             $ putIOwords
--               [ "\nshakeMD - staticP ok - *.html"
--               , showT staticP
--               , "file"
--               , showT outP
--               , "out"
--               , showT out]
--           let fromfile = resourcesP </> makeRelativeP staticP outP
--           when debug $ liftIO $ putIOwords ["\nshakeMD - staticP - fromfile ", showT fromfile]
--           copyFileChangedP fromfile outP
--           when debug $ liftIO $ putIOwords ["\n DONE shakeMD - staticP - fromfile ", showT fromfile]

--       (toFilePath staticP <> "/*.css")
--         %> \out                  -- insert css -- no subdir
--         -> do
--           let outP = makeAbsFile out :: Path Abs File
--           when debug $ liftIO
--             $ putIOwords
--               [ "\nshakeMD - staticP - *.css\n"
--               , showT outP
--               , "\nTemplatesP"
--               , showT templatesP]
--           let fromfile = templatesP </> makeRelativeP staticP outP
--           when debug $ liftIO
--             $ putIOwords ["\nshakeMD - staticP css- fromfile ", showT fromfile]
--           copyFileChangedP fromfile outP
          
--       (toFilePath staticP <> "**/*.pdf")
--         %> \out                  -- insert pdfFIles1 -- with subdir
--         -> do
--           let outP = makeAbsFile out :: Path Abs File
--           when debug $ liftIO $ putIOwords ["\nshakeMD - staticP - *.pdf", showT outP]
--           let fromfile = resourcesP </> makeRelativeP staticP outP
--           when debug $ liftIO
--             $ putIOwords ["\nshakeMD - staticP  pdf - fromfile ", showT fromfile]
--           copyFileChangedP fromfile outP
--       -- return ()

--       [toFilePath imagesTargetP <> "/*.JPG"
--         , toFilePath imagesTargetP <> "/*.jpg"]
--         |%> \out                  -- insert img files -- no subdir (for now)
--         -> do
--           let outP = makeAbsFile out :: Path Abs File
--           when debug $ liftIO $ putIOwords ["\nshakeMD - image jpg", showT outP]
--           let fromfile = imagesP </> makeRelativeP imagesTargetP outP
--           when debug $ liftIO
--             $ putIOwords ["\nshakeMD - staticP  img=age jpg- fromfile ", showT fromfile]
--           copyFileChangedP fromfile outP
--       -- return ()

--       toFilePath bannerImageTarget %> \out -> do 
--           -- let bannerImage3 = makeRelFile out
--           let outP = makeAbsFile out 
--           when debug $ liftIO $ putIOwords ["\nshakeMD - bannerImage TargetF", showT outP]
--           let fromfile = templatesP `addFileName` makeRelativeP staticP outP
--           when debug $ liftIO $ putIOwords ["\nshakeMD - bannerImage fromfile ", showT fromfile]
--           copyFileChangedP fromfile outP
    
--       -- conversion md to html (excet for what is in static) 
--       (\x -> ((toFilePath bakedP <> "**/*.html") ?== x)
--         && not ((toFilePath staticP <> "**/*.html") ?== x) -- with subdir
--         )  ?> \out ->
--         -- liftIO $ putIOwords ["\nshakeMD - bakedP html -  out ", showT out]
--         -- hakeMD - bakedP html -  out  "/home/frank/.SSG/bakedTest/SSGdesign/index.html"
--         do
--           let outP = makeAbsFile out :: Path Abs File
--           -- --needs to know if this is abs or rel file !
--           -- --liftIO $ putIOwords ["\nshakeMD - bakedP html -  out2 ", showT outP] 
--           let md = replaceExtension' "md" outP :: Path Abs File --  <-    out2 -<.> "md"  
--           -- liftIO $ putIOwords ["\nshakeMD - bakedP html 2 -  md ", showT md]
--           -- --let md1 =  stripProperPrefixP bakedP md :: Path Rel File 
--           -- l--iftIO $ putIOwords ["\nshakeMD - bakedP html 3 - md1 ", showT md1]
--           let md2 = doughP </> stripProperPrefixP bakedP md :: Path Abs File
--           -- liftIO $ putIOwords ["\nshakeMD - bakedP html 4 - md2 ", showT md2]
--           need [toFilePath md2]  
--           when debug $ liftIO $ putIOwords ["\nshakeMD - bakedP - *.html", showT outP, showT md2]
          
--           need [toFilePath masterSettings_yaml]
--           need [toFilePath masterTemplate]
--           -- need (map toFilePath yamlPageFiles2)

--           res <- runErr2action $ bakeOneFile False flags md2 layout outP
--           liftIO $ putIOwords ["\nshake2 - return from bakeOneFile", showT res]
--           return ()


--   -- return ()
--   -- copyFileChangedP source destDir = copyFileChanged (toFilePath source) (toFilePath destDir)
