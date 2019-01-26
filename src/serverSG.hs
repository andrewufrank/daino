
------------------------------------------------------------------------------
--
-- Module      :   serving the baked site
        -- started from treetagger where i used it with post -

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

import Uniform.Strings
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
import Lib.Foundation
--import Lib.Bake (bake, bakeOneFileVoid)
import Lib.Shake (shake)

programName = "SSG" :: Text
progTitle = unwords' ["constructing a static site generator"
                , "on port ", showT bakedPort]  :: Text

bakedPort = 3099


main :: IO ()
--main = quickHttpServe site
main = startProg programName progTitle
        $ bracketErrIO
            (do  -- first
                shake
                watchDough <- callIO $ forkIO mainWatchDough
                watchTemplates <- callIO $ forkIO mainWatchTemplates
                callIO $ scotty bakedPort site
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


site :: ScottyM  ()
-- for get, return the page from baked
-- for post return error
site = do
    get "/" $ file landingPage  -- showLandingPage
    middleware $ staticPolicy $ addBase (toFilePath bakedPath)


landingPage = toFilePath $ addFileName bakedPath (makeRelFile "landingPage.html")

showLandingPage :: ActionM ()
showLandingPage   = do
  setHeader "Content-Type" "text/html"
  txt <-  liftIO $ readFile landingPage
--  let x = " Sdsf" :: _
  html . t2tl . s2t  $ txt


mydef = Twitch.Options
    { Twitch.log                       = NoLogger
    , logFile                   = Nothing
    , root                      = Nothing
    , recurseThroughDirectories = True
    , debounce                  = DebounceDefault
    , debounceAmount            = 0
    , pollInterval              = 10^(6 :: Int) -- 1 second
    , usePolling                = False
    }


mainWatchDough, mainWatchTemplates :: IO ()

mainWatchDough =  do
    putIOwords [programName, progTitle, "mainWatchDough"]
    Twitch.defaultMainWithOptions (mydef
                    {Twitch.root = Just . toFilePath $ doughPath
                     , Twitch.log = Twitch.NoLogger
                    }) $ do
            Twitch.addModify (\filepath -> runErrorVoid $ shake) "**/*.md"     -- add and modify event

mainWatchTemplates =  do
    putIOwords [programName, progTitle,"mainWatchTemplates"]
    Twitch.defaultMainWithOptions (mydef
                    {Twitch.root = Just . toFilePath $ templatePath
                     , Twitch.log = Twitch.NoLogger
                    }) $ do
            Twitch.addModify (\filepath -> runErrorVoid $ shake) "**/*.html"
            Twitch.addModify (\filepath -> runErrorVoid $ shake) "**/*.*tpl"
            Twitch.addModify (\filepath -> runErrorVoid $ shake) "**/*.css"
            Twitch.addModify (\filepath -> runErrorVoid $ shake) "**/*.jpg"
            -- add and modify event
                --  "*.html" |> \_ -> system $ "osascript refreshSafari.AppleScript"


runErrorRepl :: (Show a) => a -> IO ()
-- just for testing when an event is triggered
runErrorRepl a = do
                    putIOwords ["runErrorRepl", "input is", showT a]
                    return ()



