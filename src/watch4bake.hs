
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
--import Filesystem.Path.CurrentOS

import Lib.Foundation
--import Lib.Bake (bake, bakeOneFileVoid)
import Lib.Shake

programName = "SSG" :: Text
progTitle = unwords' ["continuously constructing a static site generator"
                ]  :: Text


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
main = do
    putIOwords [programName, progTitle]
    Twitch.defaultMainWithOptions (mydef
                    {Twitch.root = Just . toFilePath $ doughPath
                     , Twitch.log = Twitch.NoLogger
                    }) $ do
--            Twitch.addModify (\filepath -> runErrorVoid $ bakeOneFileVoid  filepath) "**/*.md"     -- add and modify event
            Twitch.addModify (\filepath -> runErrorVoid $ shake) "**/*.md"     -- add and modify event
                --  "*.html" |> \_ -> system $ "osascript refreshSafari.AppleScript"


runErrorRepl :: (Show a) => a -> IO ()
-- just for testing when an event is triggered
runErrorRepl a = do
                    putIOwords ["runErrorVoid", "input is", showT a]
                    return ()




