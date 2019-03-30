
------------------------------------------------------------------------------
--
-- Module      :   serving the baked site
        -- must be in the dough directory - uses current directory to find
        -- the settings.yaml file
        -- set watches for changing - uses shake

-- content moved to Lib.Watch
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE PartialTypeSignatures     #-}

module Main where

import           Uniform.Strings         hiding ( (</>) )
import           Uniform.Filenames
import           Uniform.FileStrings
-- import           Uniform.Error
import           Uniform.Convenience.StartApp

-- import           Web.Scotty
-- import           Network.Wai.Middleware.Static  ( staticPolicy
--                                                 , addBase
--                                                 )
-- import           Network.Wai.Handler.Warp       ( Port ) -- .Warp.Types

--import System.Directory
--import System.IO

import           Control.Concurrent
import           Lib.Foundation                 ( SiteLayout(..)
                                                -- , layoutDefaults
                                                , templatesDirName
                                                , staticDirName
                                                , resourcesDirName
                                                , bannerImageFileName 
                                                , settingsFileName
                                                -- , landingPageName
                                                )


import           Lib.Shake2                      ( shakeAll
                                                -- , shakeDelete
                                                )
import           Lib.ReadSettingFile
import Uniform.Watch (mainWatch2)
import Uniform.WebServer 

--import qualified Path.IO as Pathio
--import Distribution.Simple.Utils (copyDirectoryRecursive)
--import Distribution.Verbosity (Verbosity(..), normal)

programName, progTitle :: Text
programName = "SSG" :: Text
progTitle =
    unwords' ["constructing a static site generator"
--                , "on port ", showT bakedPort
                                                    ] :: Text
-- settingsfileName = makeRelFile "settings2"

--bakedPort = 3099

-- bannerImageFileName = makeRelFile "cropped-DSC05127-1024x330.jpg"
-- where should this be fixed? duplicated in ssgBake 

main :: IO ()
main = startProg programName progTitle main2

main2 :: ErrIO ()
main2 = do
    putIOwords ["main2 start"]
    wd          <- currentDir
    (layout, port) <- readSettings (wd </> settingsFileName)
    putIOwords ["main2", showT layout, showT port]

    let bakedPath = bakedDir layout :: Path Abs Dir
    createDirIfMissing' bakedPath
    -- the directory can be missing or deleted intentionally

    -- let templatesPath = themeDir layout </> templatesDirName :: Path Abs Dir
    let resourcesPath = doughDir layout </> resourcesDirName :: Path Abs Dir
    -- copy static resources (templates and dough)
    copyDirRecursive resourcesPath (bakedPath </> staticDirName)
    putIOwords
        [ programName
        , "copied all templates  files from"
        , showT resourcesPath
        , "to"
        , showT bakedPath
        ]
    -- resources in dough are not needed for baked
--    let templatesPath =  (themeDir layout) </> templatesDirName :: Path Abs Dir
--    Pathio.copyDirRecur
--                         (unPath templatesPath) (unPath $ bakedPath </> staticDirName )
--    putIOwords [programName, "copied all templates  files"]

    -- let landing = makeRelFile "landingPage.html"
    mainWatch layout port bakedPath 

mainWatch :: SiteLayout -> Port -> Path Abs Dir ->   ErrIO ()
-- the landing page must be given here because it is special for scotty 
-- and the name of the banner imgage which must be copied by shake
mainWatch layout bakedPort bakedPath  = bracketErrIO
    (do  -- first
        shakeAll bannerImageFileName layout ""
        watchDoughTID     <- callIO $ forkIO (runErrorVoid $ watchDough layout)
        watchTemplatesTID <- callIO $ forkIO (runErrorVoid $ watchThemes layout )
        runScotty bakedPort bakedPath (landingPage layout)
        return (watchDoughTID, watchTemplatesTID)
    )
    (\(watchDoughTID, watchTemplatesTID) -> do -- last
        putIOwords ["main2 end"]
        callIO $ killThread (watchDoughTID)
        callIO $ killThread (watchTemplatesTID)
        return ()
    )
    (\watch -> do   -- during
        putIOwords ["main2 run"]
--                        mainWatch
        -- could here the watch for bake be included ?
        putIOwords ["main2 run end "]
        return ()
    )



watchDough :: SiteLayout -> ErrIO ()
watchDough layout  = mainWatch2 (shakeAll (bannerImage layout) layout) 
                (doughDir layout)    -- :: Path Abs Dir
                ["md", "bib", "yaml"]  :: ErrIO ()

-- themesDir = (themeDir layout) </> templatesDirName :: Path Abs Dir

watchThemes :: SiteLayout -> ErrIO ()
watchThemes layout  = mainWatch2 (shakeAll  (bannerImage layout) layout)
                (themeDir layout </> templatesDirName )  -- :: Path Abs Dir
                ["yaml", "dtpl", "css", "jpg"] :: ErrIO () 
-- add copy static files ...

--showLandingPage :: ActionM ()
--showLandingPage   = do
--  setHeader "Content-Type" "text/html"
--  txt <-  liftIO $ readFile landingPage
----  let x = " Sdsf" :: _
--  html . t2tl . s2t  $ txt






