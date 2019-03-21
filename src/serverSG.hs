
------------------------------------------------------------------------------
--
-- Module      :   serving the baked site
        -- must be in the dough directory - uses current directory to find
        -- the settings.yaml file
        -- set watches for changing - uses shake


-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Uniform.Strings         hiding ( (</>) )
import           Uniform.Filenames
import           Uniform.FileStrings
import           Uniform.Error
import           Uniform.Convenience.StartApp

import           Web.Scotty
import           Network.Wai.Middleware.Static  ( staticPolicy
                                                , addBase
                                                )
import           Network.Wai.Handler.Warp       ( Port ) -- .Warp.Types

--import System.Directory
--import System.IO

import           Twitch                  hiding ( Options
                                                , log
                                                )
import qualified Twitch
--import Control.Concurrent.Spawn
import           Control.Concurrent
import           Lib.Foundation                 ( SiteLayout(..)
                                                , layoutDefaults
                                                , templatesDirName
                                                , staticDirName
                                                , resourcesDirName
                                                )


import           Lib.Shake                      ( shake
                                                , shakeDelete
                                                )
import           Lib.ReadSettingFile

--import qualified Path.IO as Pathio
--import Distribution.Simple.Utils (copyDirectoryRecursive)
--import Distribution.Verbosity (Verbosity(..), normal)

programName = "SSG" :: Text
progTitle =
    unwords' ["constructing a static site generator"
--                , "on port ", showT bakedPort
                                                    ] :: Text
settingsfileName = makeRelFile "settings2"

--bakedPort = 3099


main :: IO ()
--main = quickHttpServe site
main = startProg programName progTitle main2

main2 :: ErrIO ()
main2 = do
    putIOwords ["main2 start"]
    (layout, port) <- readSettings settingsfileName
    putIOwords ["main2", showT layout, showT port]

    let bakedPath = bakedDir layout :: Path Abs Dir
    createDirIfMissing' bakedPath
    -- the directory can be missing or deleted intentionally

    let templatesPath = (themeDir layout) </> templatesDirName :: Path Abs Dir
    let resourcesPath = (doughDir layout) </> resourcesDirName :: Path Abs Dir
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
    mainWatch layout port bakedPath

mainWatch :: SiteLayout -> Port -> Path Abs Dir -> ErrIO ()
mainWatch layout bakedPort bakedPath = bracketErrIO
    (do  -- first
        shake layout ""
        watchDough     <- callIO $ forkIO (mainWatchDough layout)
        watchTemplates <- callIO $ forkIO (mainWatchThemes layout)
        callIO $ scotty bakedPort (site bakedPath)
        return (watchDough, watchTemplates)
    )
    (\(watchDough, watchTemplates) -> do -- last
        putIOwords ["main2 end"]
        callIO $ killThread watchDough
        callIO $ killThread watchTemplates
        return ()
    )
    (\watch -> do   -- during
        putIOwords ["main2 run"]
--                        mainWatch
        -- could here the watch for bake be included ?
        putIOwords ["main2 run end "]
        return ()
    )


site :: Path Abs Dir -> ScottyM ()
-- for get, return the page from baked
-- for post return error
site bakedPath = do
    get "/" $ file (landingPage bakedPath)
    middleware $ staticPolicy $ addBase (toFilePath bakedPath)


landingPage bakedPath =
    toFilePath $ addFileName bakedPath (makeRelFile "landingPage.html")

--showLandingPage :: ActionM ()
--showLandingPage   = do
--  setHeader "Content-Type" "text/html"
--  txt <-  liftIO $ readFile landingPage
----  let x = " Sdsf" :: _
--  html . t2tl . s2t  $ txt


twichDefault4ssg = Twitch.Options { Twitch.log                = NoLogger
                                  , logFile                   = Nothing
                                  , root                      = Nothing
                                  , recurseThroughDirectories = True
                                  , debounce                  = Debounce
                                  , debounceAmount            = 1  -- second? NominalTimeDifference
                                  , pollInterval              = 10 ^ (6 :: Int) -- 1 second
                                  , usePolling                = False
                                  }


mainWatchDough, mainWatchThemes :: SiteLayout -> IO ()

mainWatchDough layout = do
    let doughPath = (doughDir layout) :: Path Abs Dir
    putIOwords [programName, progTitle, "mainWatchDough"]
    Twitch.defaultMainWithOptions
            (twichDefault4ssg { Twitch.root = Just . toFilePath $ doughPath
                              , Twitch.log  = Twitch.NoLogger
                              }
            )
        $ do
              Twitch.addModify
                  (\filepath -> runErrorVoid $ shake layout filepath)
                  "**/*.md"
              Twitch.delete
                  (\filepath -> runErrorVoid $ shakeDelete layout filepath)
                  "**/*.md"
              Twitch.addModify
                  (\filepath -> runErrorVoid $ shake layout filepath)
                  "**/*.yaml"
              Twitch.addModify
                  (\filepath -> runErrorVoid $ shake layout filepath)
                  "**/*.bib"
            -- add and modify event

mainWatchThemes layout = do
    let templatesPath = (themeDir layout) </> templatesDirName :: Path Abs Dir
    let bakedPath     = bakedDir layout
    putIOwords [programName, progTitle, "mainWatchThemes"]
    -- copy the static files, not done by shake yet
    runErrorVoid $ copyDirRecursive templatesPath (bakedPath </> staticDirName)
    putIOwords [programName, "copied templates all files"]
    Twitch.defaultMainWithOptions
            (twichDefault4ssg { Twitch.root = Just . toFilePath $ templatesPath
                              , Twitch.log  = Twitch.NoLogger
                              }
            )
        $ do
--            verbosity from Cabal
              Twitch.addModify
                  (\filepath -> runErrorVoid $ shake layout filepath)
                  "**/*.yaml"
              Twitch.addModify
                  (\filepath -> runErrorVoid $ shake layout filepath)
                  "**/*.dtpl"
              Twitch.addModify
                  (\filepath -> runErrorVoid $ shake layout filepath)
                  "**/*.css"
              Twitch.addModify
                  (\filepath -> runErrorVoid $ shake layout filepath)
                  "**/*.jpg"
            -- add and modify event
                --  "*.html" |> \_ -> system $ "osascript refreshSafari.AppleScript"


runErrorRepl :: (Show a) => a -> IO ()
-- just for testing when an event is triggered
runErrorRepl a = do
    putIOwords ["runErrorRepl", "input is", showT a]
    return ()



