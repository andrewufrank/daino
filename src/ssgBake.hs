-----------------------------------------------------------------------------
--
-- Module      :   ssgBake
-- the main for the sgg - no UI yet
-- uses shake only to convert the md files
-- copies all resources
-- must start in dir with settings2.yaml
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main     where      -- must have Main (main) or Main where

import Uniform.Convenience.StartApp
import Uniform.FileIO hiding ((<.>), (</>))

import Lib.Bake
import Lib.Shake
--import Lib.Foundation (layoutDefaults, SiteLayout (..))
import Lib.ReadSettingFile
import Lib.Foundation (SiteLayout (..), templatesDirName, staticDirName, resourcesDirName, templatesImgDirName)

import Development.Shake
import Development.Shake.FilePath

import Web.Scotty
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Network.Wai.Handler.Warp  (Port) -- .Warp.Types


programName = "SSG10" :: Text
progTitle = "constructing a static site generator" :: Text

settingsfileName = makeRelFile "settings2" -- the yaml file
bannerImageFileName = makeRelFile "cropped-DSC05127-1024x330.jpg"

main :: IO ()
main = startProg programName progTitle
             (do
                (layout2, port2)  <- readSettings settingsfileName
                bakeAll layout2
                callIO $ scotty port2 (site (bakedDir layout2))
                return ()
                )

bakeAll :: SiteLayout -> ErrIO ()
-- ^ bake all md files and copy the resources
-- sets the current dir to doughDir
bakeAll layout = do
    let  -- where the layout is used, rest in shakeWrapped
          doughP      =    doughDir $ layout  -- the regular dough
          templatesP =   (themeDir $ layout)
                               `addFileName` ( templatesDirName)
          bakedP =  bakedDir $ layout
    setCurrentDir (doughP)
    deleteDirRecursive bakedP

    -- copy resources and banner   not easy to do with shake
    copyDirRecursive (doughP `addDir` resourcesDirName)   (bakedP `addDir` staticDirName)

    let bannerImage = templatesImgDirName `addFileName` bannerImageFileName

    copyOneFile (templatesP `addFileName` bannerImage)
        (bakedP `addDir` staticDirName `addDir` bannerImage)

    -- convert md files and copy css
    callIO $ shakeMD layout  doughP templatesP bakedP

    return ()

shakeMD :: SiteLayout -> Path Abs Dir  -> Path Abs Dir -> Path Abs Dir  -> IO ()
-- ^ process all md files
-- in IO
shakeMD layout  doughP templatesP bakedP=
    shakeArgs2 bakedP $ do
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

        want ["allMarkdownConversion"]
        phony "allMarkdownConversion" $ do

            mdFiles1 <- getDirectoryFiles  doughD ["//*.md"]
            let htmlFiles2 = [bakedD </> md -<.> "html" | md <- mdFiles1] -- , not $ isInfixOf' "index.md" md]
            liftIO $ putIOwords ["\nshakeWrapped - htmlFile"
                        ,  showT (map (makeRelative  doughD) htmlFiles2)]
            need htmlFiles2

            cssFiles1 <- getDirectoryFiles templatesD ["*.css"] -- no subdirs
            let cssFiles2 = [replaceDirectory c staticD  | c <- cssFiles1]
            need cssFiles2


        (bakedD <> "//*.html") %> \out -> do

            liftIO $ putIOwords ["\nshakeWrapped - bakedD html -  out ", showT out]
            let md =   doughD </>  (makeRelative bakedD $ out -<.> "md")
            liftIO $ putIOwords ["\nshakeWrapped - bakedD html - c ", showT md]

            runErr2action $ bakeOneFileFPs  md  doughD templatesD out

        (staticD </> "*.css") %> \out ->  do           -- insert css
            liftIO $ putIOwords ["\nshakeWrapped - staticD - *.css", showT out]
            copyFileChanged (replaceDirectory out templatesD) out

        return ()

site :: Path Abs Dir -> ScottyM  ()
-- for get, return the page from baked
-- for post return error
site bakedPath = do
    get "/" $ file (landingPage bakedPath)
    middleware $ staticPolicy $ addBase (toFilePath bakedPath)


landingPage bakedPath = toFilePath $ addFileName bakedPath (makeRelFile "landingPage.html")
