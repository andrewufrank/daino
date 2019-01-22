
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

import qualified Twitch
--import Filesystem.Path.CurrentOS

import Lib.Foundation
import Lib.Bake (bake)

programName = "SSG" :: Text
progTitle = unwords' ["constructing a static site generator"
                , "on port ", showT bakedPort]  :: Text

bakedPort = 3099


main = Twitch.defaultMain  $ do
--        WithOptions (Twitch.Options {Twitch.root = Just . toFilePath $ doughPath}) $ do
  Twitch.addModify (\filePath -> runErrorRepl filePath) "*.md"     -- add and modify event
--  "*.html" |> \_ -> system $ "osascript refreshSafari.AppleScript"

runErrorRepl :: (Show a) => a -> IO ()
runErrorRepl a = do
                    putIOwords ["runErrorVoid", "input is", showT a]
                    return ()

runErrorVoid :: ErrIO () -> IO ()
runErrorVoid a = do
                    res <- runErr a
                    putIOwords ["runErrorVoid", showT res]
                    case res of
                        Left msg -> error (t2s msg)
                        Right _ -> return ()

main2 :: IO ()
--main = quickHttpServe site
main2 = startProg programName progTitle
        $ bracketErrIO
            (do  -- first
                callIO $ scotty bakedPort site
                return "X")
            (\x -> do -- last
                        return ()
                )
            (\x -> do   -- during
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


--inotifyTest = do
--    wdx <- do
--              inotify <- initINotify
--              print inotify
--              wd <- addWatch
--                      inotify
--                      [Close,Modify,Move]
--                      (s2b . toFilePath $ doughPath)
--                      print
--              return wd
--    print wdx
--    putIOwords ["Listens to your directory", showT doughPath, "with watch", showT wdx
--                    , " Hit enter to terminate."]
--    return wdx

