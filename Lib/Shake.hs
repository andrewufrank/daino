
------------------------------------------------------------------------------
--
-- Module      :   the  process to convert
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

module Lib.Shake
     where

import Development.Shake -- hiding ((<.>), (</>))
import Development.Shake.FilePath -- (toFilePath, makeAbsFile
--                , makeRelFile, makeRelDir, stripProperPrefix')
         hiding ((<.>), (</>))
import Lib.Bake -- for instances
import Lib.Foundation (SiteLayout (..), templatesDirName, staticDirName)
import Uniform.FileIO

import Uniform.FileStrings ()
import Uniform.Strings (putIOwords)

shakeDelete :: SiteLayout ->  FilePath ->   ErrIO ()
-- ^ experimental - twich found delete of md
shakeDelete _ filepath = do
    putIOwords ["\n\n*******************************************"
            ,"experimental -- twich found  DELETED MD file ", s2t filepath]

shake :: SiteLayout ->  FilePath ->   ErrIO ()
-- the second argument is the file which has changed
shake layout filepath  = do
    putIOwords ["\n\n=====================================shake start"
--            , shownice layout
            ,"caused by", s2t filepath]
    let  -- where the layout is used, rest in shakeWrapped
          doughP      =    doughDir $ layout  -- the regular dough
          templatesP =   (themeDir $ layout)
                               `addFileName` ( templatesDirName)
          bakedP =  bakedDir $ layout
    setCurrentDir (doughDir layout)

    -- delete old baked files  -- should not be needed when needs correct
    fs <- getDirectoryDirs' (toFilePath bakedP)
    putIOwords ["shakeTesting", "could be to delete", showT fs]
--  mapM_ removeDirectoryRecursive fs

    callIO $ shakeWrapped doughP templatesP bakedP

    putIOwords ["\n--------------------------------------------shake done", "\n"]

    return ()

shakeArgs2 bakedP = shakeArgs shakeOptions {shakeFiles=toFilePath bakedP
                , shakeVerbosity=Chatty -- Loud
                , shakeLint=Just LintBasic
--                , shakeRebuild=[(RebuildNow,"allMarkdownConversion")]
--                  seems not to produce an effect
                }

shakeWrapped :: Path Abs Dir  -> Path Abs Dir  -> Path Abs Dir ->  IO  ()
shakeWrapped doughP templatesP bakedP = shakeArgs2 bakedP $ do
--    shakeArgs shakeOptions {shakeFiles=toFilePath bakedP
--                , shakeVerbosity=Chatty -- Loud
--                , shakeLint=Just LintBasic
----                , shakeRebuild=[(RebuildNow,"allMarkdownConversion")]
----                  seems not to produce an effect
--                } $ do

    let doughD = toFilePath doughP
        templatesD = toFilePath templatesP
        bakedD = toFilePath bakedP
    let staticD =   bakedD </>  (toFilePath staticDirName)
        -- where all the static files go
        resourcesD =  doughD </> "resources"
        masterTemplateFn =  "master4.dtpl"
        settingsYamlFn =  "settings2.yaml"

--    phony "clean" $ do
--        putNormal "delete all files in "
--        removeFiles bakedD<>"*"
            -- need one for ssg and one for baked

    want ["allMarkdownConversion"]
    phony "allMarkdownConversion" $ do

        liftIO $ putIOwords ["\nshakeWrapped phony allMarkdonwConversion" ]

--        yamllint  -- issue with create process, error
--        yamllint: createProcess: runInteractive


        -- get markdown files
        mdFiles1 <- getDirectoryFiles  doughD ["//*.md"]
            -- , "//*.markdown"]
            -- todo markdown files are not found ?
                -- found ok, but not indexed

        let htmlFiles2 = [bakedD </> md -<.> "html"
                        | md <- mdFiles1, not $ isInfixOf' "index.md" md]
        liftIO $ putIOwords ["\nshakeWrapped - htmlFile"
                ,  showT (map (makeRelative  doughD) htmlFiles2)]

-- TODO missing static resources from dough
-- what else needs to be copied ?

    -- get html files from dough (not yet done, html are in resources)
--        htmlFilesStatic <- getDirectoryFiles  doughD ["//*.html"]
--            -- todo markdown files are not found ?
--
--        let htmlFilesStatic2 = [bakedD </> f | f <- htmlFilesStatic]
--        liftIO $ putIOwords ["\nshakeWrapped - htmlFilesStatic2"
--                ,  showT (map (makeRelative  doughD) htmlFilesStatic2)]
--        need htmlFilesStatic2

--        -- get css
--        cssFiles1 <- getDirectoryFiles templatesD ["*.css"] -- no subdirs
--        let cssFiles2 = [replaceDirectory c staticD  | c <- cssFiles1]
--        liftIO $ putIOwords ["\nshakeWrapped - phony cssFiles2", showT cssFiles2]

--        need cssFiles2
        need htmlFiles2
        -- the templates static files are copied with watch
        -- process the index files after all others are done
        indexFiles1 <- getDirectoryFiles doughD ["//index.md"]
        let indexFiles2 = [bakedD </> ix -<.> "html" | ix <- indexFiles1]
        liftIO $ putIOwords ["\nshakeWrapped - indexFiles2"
                ,  showT indexFiles2]
        need indexFiles2


    (bakedD <> "//*.html") %> \out -> do

        liftIO $ putIOwords ["\nshakeWrapped - bakedD html -  out ", showT out]
        let md =   doughD </>  (makeRelative bakedD $ out -<.> "md")
        liftIO $ putIOwords ["\nshakeWrapped - bakedD html - c ", showT md]

        let masterTemplate = templatesD </> masterTemplateFn
            masterSettings_yaml = doughD </> settingsYamlFn

        biblio <- getDirectoryFiles resourcesD ["*.bib"]
        let biblio2 = [resourcesD </> b | b <- biblio]
        putIOwords ["shake bakedD", "biblio", showT biblio2]

        yamlPageFiles <- getDirectoryFiles templatesD ["*.yaml"]
        let yamlPageFiles2 = [templatesD </> y | y <- yamlPageFiles]

        cssFiles1 <- getDirectoryFiles templatesD ["*.css"] -- no subdirs
        let cssFiles2 = [replaceDirectory c staticD  | c <- cssFiles1]

--        when (takeBaseName md == "index")  $
--            do
----        -- for index rebake
--                let mdDir = md -<.> ""  -- should be directory
--                ixLikely <- doesDirectoryExist mdDir
--                liftIO $ putIOwords ["shake bakedD", "ixLikely ", showT ixLikely, s2t mdDir]
--                when ixLikely $
--                    do
--                        submds <- getDirectoryFiles mdDir ["*.md"]
--                        putIOwords ["shake bakedD", "submds", showT submds]
--                        need submds
--
--        liftIO $ putIOwords ["shake bakedD", "ixLikely passed", showT ixLikely, s2t mdDir]

        -- the list of needs is too large and forces
        -- baking when any biblio,css,page.yaml or .dtpl changes
        need biblio2
        need cssFiles2
        need yamlPageFiles2
        need [masterSettings_yaml]
        need [masterTemplate]
        need [md]

        runErr2action $ bakeOneFileFPs  md  doughD templatesD out
            -- c relative to dough/



    (staticD </> "*.css") %> \out ->  do           -- insert css
        liftIO $ putIOwords ["\nshakeWrapped - staticD - *.css", showT out]
        copyFileChanged (replaceDirectory out templatesD) out


instance Exception Text

runErr2action :: ErrIO a -> Action a
runErr2action op = liftIO $ do
    res <- runErr  op
    case res of
        Left msg -> throw msg
        Right a -> return a

--makeOneMaster :: FilePath -> FilePath -> FilePath -> IO ()
---- makes the master plus page style in template
--makeOneMaster master page result = do
--    putIOwords ["makeOneMaster", showT result]
--
--    res <- runErrorVoid $ putPageInMaster
--                     (makeAbsFile page) (makeAbsFile master)
--                    "body" (makeAbsFile result)
--    putIOwords ["makeOneMaster done", showT res ]
--    return ()

