
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
import Lib.Bake (bake, bakeOneFileVoid)

programName = "SSG" :: Text
progTitle = unwords' ["constructing a static site generator"
                , "on port ", showT bakedPort]  :: Text

bakedPort = 3099


main :: IO ()
--main = quickHttpServe site
main = startProg programName progTitle
        $ bracketErrIO
            (do  -- first
--                callIO $ spawn mainWatch
                watch <- callIO $ forkIO mainWatch
                callIO $ scotty bakedPort site
                return watch )
            (\watch -> do -- last
                        putIOwords ["main2 end"]
                        callIO $ killThread watch
                        return ()
                )
            (\watch -> do   -- during
                        putIOwords ["main2 run"]
--                        mainWatch
                        -- could here the watch for bake be included ?
                        putIOwords ["main2 run end "]
                        return ()
                )
--                wd <- inotifyTest
--                getLine
--                removeWatch wd
--                putIOwords ["remved watch", showT wd]
--            return ()

--            bake
--            putIOwords ["serverSG started", "for dir", showT bakedPath, "on port", showT bakedPort, "\n"]
--            callIO $ scotty bakedPort site


site :: ScottyM  ()
-- for get, return the page from baked
-- for post return error
site = do
    get "/" $ file landingPage  -- showLandingPage
    middleware $ staticPolicy $ addBase (toFilePath bakedPath)


landingPage = toFilePath $ addFileName bakedPath (makeRelFile "index.html")

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

mainWatch :: IO ()
mainWatch =  do
    putIOwords [programName, progTitle]
    Twitch.defaultMainWithOptions (mydef
                    {Twitch.root = Just . toFilePath $ doughPath
                     , Twitch.log = Twitch.NoLogger
                    }) $ do
            Twitch.addModify (\filepath -> runErrorVoid $ bakeOneFileVoid  filepath) "**/*.md"     -- add and modify event
    Twitch.defaultMainWithOptions (mydef
                    {Twitch.root = Just . toFilePath $ templatePath
                     , Twitch.log = Twitch.NoLogger
                    }) $ do
            Twitch.addModify (\filepath -> runErrorVoid $ bake) "*.html"     -- add and modify event
                --  "*.html" |> \_ -> system $ "osascript refreshSafari.AppleScript"


runErrorRepl :: (Show a) => a -> IO ()
-- just for testing when an event is triggered
runErrorRepl a = do
                    putIOwords ["runErrorRepl", "input is", showT a]
                    return ()

runErrorVoid :: ErrIO () -> IO ()
runErrorVoid a = do
                    res <- runErr a
                    putIOwords ["runErrorVoid", "result", showT res]
                    case res of
                        Left msg -> error (t2s msg)
                        Right _ -> return ()


