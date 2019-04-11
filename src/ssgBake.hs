-----------------------------------------------------------------------------
--
-- Module      :   ssgBake
-- the main for the sgg 
-- uses shake only to convert the md files
-- copies all resources
-- must start in dir with settings2.yaml
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where      -- must have Main (main) or Main where

import           Uniform.Convenience.StartApp (startProg)
import           Uniform.Error  
import           Uniform.WebServer (runScotty)

import           Lib.CmdLineArgs (PubFlags(..), parseArgs2input)
import           Lib.Shake2 (shakeAll)
import           Lib.ReadSettingFile (readSettings)
import           Lib.Foundation (SiteLayout(..), settingsFileName)
import           Lib.Watch (mainWatch)
import Uniform.Ftp 
import Uniform.FileIO

programName, progTitle :: Text
programName = "SSG10" :: Text

progTitle = "constructing a static site generator x6" :: Text

main :: IO ()
main = startProg
  programName
  progTitle
  (do 
      flags :: PubFlags <- parseArgs2input
        settingsFileName
        --  add a delete flag
        (unlinesT
            [ "the flags to select what is included:"
            , "default is nothing included"
            , "\n -p publish"
            , "\n -d drafts"
            , "\n -o old"
            , "\n -t test (use data in package)"
            , "\n -w start to watch the files for changes and rebake (implies -s s cancels -u"
            , "\n -s start local server (port is fixed in settings)"
            , "\n -u upload to external server"])
        "list flags to include"
      (layout2, port2) <- readSettings (settingsFile flags)
      if watchFlag flags  -- implies server
        then  
          mainWatch layout2 flags port2
        else do  
          shakeAll layout2 flags ""
          -- the last is the filename that caused the shake call
          --  let landing = makeRelFile "landingPage.html"
          when (serverFlag flags) $  
            runScotty port2 (bakedDir layout2) (landingPage layout2)
          when (uploadFlag flags) $ do 
                (a,s) <- runStateT
                    (ftpUploadDirsRecurse (bakedDir layout2) 
                        (if (testFlag flags) then (makeAbsDir "/ssg.gerastree.at/"))
                                  else (makeAbsDir "/frank.gerastree.at/")
                    ftp0
                return ()
        
      putIOwords ["ssgBake done"]
      return ())

