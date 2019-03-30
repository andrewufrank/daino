-----------------------------------------------------------------------------
--
-- Module      :   ssgBake
-- the main for the sgg - no UI yet
-- uses shake only to convert the md files
-- copies all resources
-- must start in dir with settings2.yaml

-- experimental: only the md files are baked 
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where      -- must have Main (main) or Main where

import           Uniform.Convenience.StartApp
import           Lib.CmdLineArgs
-- import           Uniform.FileIO          hiding ( (<.>)
                                                -- , (</>)
                                                -- )
-- import Uniform.Shake.Path
-- import           Uniform.Shake
import           Uniform.Error                  ( )
import           Uniform.WebServer

-- import           Lib.Bake
import           Lib.Shake2
--import Lib.Foundation (layoutDefaults, SiteLayout (..))
import           Lib.ReadSettingFile
import           Lib.Foundation                 ( SiteLayout(..)
                                                -- , templatesDirName
                                                -- , staticDirName
                                                -- , resourcesDirName
                                                -- , templatesImgDirName
                                                , settingsFileName
                                                )

-- import Development.Shake
-- import Development.Shake.FilePath
--import Development.Shake.Path hiding (setCurrentDir, toFilePath)

-- import Web.Scotty
-- import Network.Wai.Middleware.Static (staticPolicy, addBase)
-- import Network.Wai.Handler.Warp  (Port) -- .Warp.Types


programName, progTitle :: Text
programName = "SSG10" :: Text
progTitle = "constructing a static site generator x6" :: Text


-- bannerImageFileName = makeRelFile "cropped-DSC05127-1024x330.jpg"
-- where should this be fixed? duplicate in serverSG


main :: IO ()
main = startProg
   programName
   progTitle
   (do
      inp :: Inputs <- parseArgs2input
         settingsFileName
         (unlinesT
            [ "the flags to select what is included:"
            , "\n -p publish"
            , "\n -d drafts"
            , "\n -o old"
            , "\n -t test (use data in package)"
            , "default is nothing included"
            ]
         )
         "list flags to include"
      (layout2, port2) <- readSettings (settingsFile inp)
      shakeAll (bannerImage layout2) layout2  ""
      -- the last is the filename that caused the shake call
      --  let landing = makeRelFile "landingPage.html"
      runScotty port2 (bakedDir layout2) (landingPage layout2)
       -- callIO $ scotty port2 (site (bakedDir layout2))
      return ()
   )

-- shakeAll :: SiteLayout -> ErrIO ()
-- -- ^ bake all md files and copy the resources
-- -- sets the current dir to doughDir
-- shakeAll layout = do
--     let  -- where the layout is used, rest in shakeWrapped
--           doughP      =    doughDir  layout  -- the regular dough
--           templatesP =   themeDir layout
--                                `addFileName` templatesDirName
--           bakedP =  bakedDir  layout
--     setCurrentDir doughP
--     deleteDirRecursive bakedP

--     -- copy resources and banner   not easy to do with shake
--     -- only the html and the pdf files (possible the jpg) are required
-- --    copyDirRecursive (doughP `addDir` resourcesDirName)   (bakedP `addDir` staticDirName)

--     let bannerImage = templatesImgDirName `addFileName` bannerImageFileName

--     copyOneFile (templatesP `addFileName` bannerImage)
--         (bakedP `addDir` staticDirName `addDir` bannerImage)

--     -- convert md files and copy css
--     callIO $ shakeMD layout  doughP templatesP bakedP

--     return ()


-- site :: Path Abs Dir -> ScottyM  ()
-- -- for get, return the page from baked
-- -- for post return error
-- site bakedPath = do
--     get "/" $ file (landingPage bakedPath)
--     middleware $ staticPolicy $ addBase (toFilePath bakedPath)


-- landingPage bakedPath = toFilePath $ addFileName bakedPath (makeRelFile "landingPage.html")
