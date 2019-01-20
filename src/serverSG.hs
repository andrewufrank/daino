
------------------------------------------------------------------------------
--
-- Module      :   serving the baked site
--

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

import Web.Scotty
import Network.Wai.Middleware.Static

import Lib.Foundation

bakedPort = 3000

main :: IO ()
--main = quickHttpServe site
main = do
    putIOwords ["serverSG started", "for dir", showT bakedPath]
    scotty bakedPort site

--  middleware (static )) -- serves the source directory
--  notFound (text "404: Not found!")

site :: ScottyM  ()
-- for get, return the page from baked
-- for post return error
site = do
    get "/" showLandingPage
    middleware $ staticPolicy $ addBase (toFilePath bakedPath)
--    does not open the index for /
-- should the baked be included or not - included or not in relative path

--    post "/" showNoPostPage
--    delete "/" showNoPostPage
--    put "/" showNoPostPage
--    post "/test" ttgermanTest
-- test with : curl -d "Das ist ein einfacher Satz." localhost:17701/test
{- result is
TTdata {ttwf = "Das", ttpos = "PDS", ttlemma = "die"}
TTdata {ttwf = "ist", ttpos = "VAFIN", ttlemma = "sein"}
TTdata {ttwf = "ein", ttpos = "ART", ttlemma = "eine"}
TTdata {ttwf = "einfacher", ttpos = "ADJA", ttlemma = "einfach"}
TTdata {ttwf = "Satz.", ttpos = "NN", ttlemma = "<unknown>"}
-}

showLandingPage :: ActionM ()
showLandingPage   = do
  setHeader "Content-Type" "text/html"
  txt <-  liftIO $ readFile (toFilePath $ addFileName bakedPath (makeRelFile "index.html"))
--  let x = " Sdsf" :: _
  html . t2tl . s2t  $ txt

--showBakedPage :: ActionM ()
---- get the page asked for
--showBakedPage = do


--ttgermanTest :: ActionM ()
---- body are tokens separated by blanks, to be easy to test
---- real thing is tokens separated by newline
--ttgermanTest  = do
--  sent  <- body
--  let sentT = bb2t . BL.toStrict $ sent
--  let toks = words' sentT :: [Text]
--  putIOwords ["ttgermanTest sentT:", sentT]
--  putIOwords toks
--  r1 <- liftIO (runErr $ ttProcess German toks  )
--  let res2 =   either (const zero)  (id) r1
--  putIOwords ["ttgermanTest res2:", res2]
--  text (L.fromStrict res2)
--
--ttgerman :: ActionM ()
--ttgerman  = do
--  sent  <- body
--  let sentT = bb2t . BL.toStrict $ sent
--  let toks = lines' sentT :: [Text]
--  putIOwords ["ttgerman sentT:", sentT]
--  putIOwords toks
--  r1 <- liftIO (runErr $ ttProcess German toks  )
--  let res2 =   either (const zero)  (id) r1
--  putIOwords ["ttgerman res2:", res2]
--  text (L.fromStrict res2)





