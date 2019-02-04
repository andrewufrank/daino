
------------------------------------------------------------------------------
--
-- Module      :   serving the baked site
        -- must be in the dough directory - uses current directory to find
        -- the settings.yaml file

-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main
     where

import Uniform.Strings hiding ((</>))
import Uniform.Filenames
import Uniform.FileStrings
import Uniform.Error
import Uniform.Convenience.StartApp

import Web.Scotty
import Network.Wai.Middleware.Static

import System.Directory
import System.IO

import  Twitch hiding (Options, log)
import qualified Twitch
--import Control.Concurrent.Spawn
import Control.Concurrent
import Lib.Foundation (SiteLayout (..), layoutDefaults, templatesDirName, staticDirName)


import Lib.Shake (shake)
import  Lib.ReadSettingFile

import Distribution.Simple.Utils (copyDirectoryRecursive)
import Distribution.Verbosity (Verbosity(..), normal)

programName = "SSG" :: Text
progTitle = unwords' ["constructing a static site generator"
                , "on port ", showT bakedPort]  :: Text

bakedPort = 3099


main :: IO ()
--main = quickHttpServe site
main = startProg programName progTitle
                main2

main2 :: ErrIO ()
main2 = do
            layout2 <- readSettings
            let bakedPath =   (bakedDir layout2) :: Path Abs Dir
            mainWatch layout2 bakedPath
    where      --  layout = layoutDefaults
--                doughPath =  (doughDir layout) :: Path Abs Dir
--                bakedPath =   (bakedDir layout2) :: Path Abs Dir

mainWatch :: SiteLayout -> Path Abs Dir ->  ErrIO ()
mainWatch layout  bakedPath = bracketErrIO
        (do  -- first
            shake layoutDefaults
            watchDough <- callIO $ forkIO (mainWatchDough layout)
            watchTemplates <- callIO $ forkIO (mainWatchThemes layout)
            callIO $ scotty bakedPort (site bakedPath)
            return (watchDough,watchTemplates) )
        (\(watchDough,watchTemplates) -> do -- last
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


site :: Path Abs Dir -> ScottyM  ()
-- for get, return the page from baked
-- for post return error
site bakedPath = do
    get "/" $ file (landingPage bakedPath)
    middleware $ staticPolicy $ addBase (toFilePath bakedPath)


landingPage bakedPath = toFilePath $ addFileName bakedPath (makeRelFile "landingPage.html")

--showLandingPage :: ActionM ()
--showLandingPage   = do
--  setHeader "Content-Type" "text/html"
--  txt <-  liftIO $ readFile landingPage
----  let x = " Sdsf" :: _
--  html . t2tl . s2t  $ txt


twichDefault4ssg = Twitch.Options
    { Twitch.log                       = NoLogger
    , logFile                   = Nothing
    , root                      = Nothing
    , recurseThroughDirectories = True
    , debounce                  = DebounceDefault
    , debounceAmount            = 0
    , pollInterval              = 10^(6 :: Int) -- 1 second
    , usePolling                = False
    }


mainWatchDough, mainWatchThemes :: SiteLayout  ->  IO ()

mainWatchDough layout  =  do
    let doughPath =  (doughDir layout) :: Path Abs Dir
    putIOwords [programName, progTitle, "mainWatchDough"]
    Twitch.defaultMainWithOptions (twichDefault4ssg
                    {Twitch.root = Just . toFilePath $ doughPath
                     , Twitch.log = Twitch.NoLogger
                    }) $ do
            Twitch.addModify (\filepath -> runErrorVoid $ shake layout) "**/*.md"     -- add and modify event

mainWatchThemes layout =  do
    let templatesPath =  (themeDir layout) </> templatesDirName :: Path Abs Dir
    let bakedPath = bakedDir layout
    putIOwords [programName, progTitle,"mainWatchThemes"]
    -- copy the static files, not done by shake yet
    copyDirectoryRecursive normal
                         (toFilePath templatesPath) (toFilePath $ bakedPath </> staticDirName )
    Twitch.defaultMainWithOptions (twichDefault4ssg
                    {Twitch.root = Just . toFilePath $ templatesPath
                     , Twitch.log = Twitch.NoLogger
                    }) $ do
--            verbosity from Cabal
            Twitch.addModify (\filepath -> runErrorVoid $ shake layout) "**/*.html"
            Twitch.addModify (\filepath -> runErrorVoid $ shake layout) "**/*.*tpl"
            Twitch.addModify (\filepath -> runErrorVoid $ shake layout) "**/*.css"
            Twitch.addModify (\filepath -> runErrorVoid $ shake layout) "**/*.jpg"
            -- add and modify event
                --  "*.html" |> \_ -> system $ "osascript refreshSafari.AppleScript"


runErrorRepl :: (Show a) => a -> IO ()
-- just for testing when an event is triggered
runErrorRepl a = do
                    putIOwords ["runErrorRepl", "input is", showT a]
                    return ()



