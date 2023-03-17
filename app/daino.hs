-------------------------------------------------------------------
--
-- Module      :   daino
----------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- | the main for the sgg
 uses shake only to convert the md files
 copies all resources
 must start in dir with settings3.yaml
-}
module Main where -- must have Main (main) or Main where

import ShakeBake.CmdLineArgs ( parseArgs2input )  
import Foundational.CmdLineFlags ( PubFlags, progName, progTitle  )
-- import Foundational.SettingsPage (sourceDirTestSite )  

import ShakeBake.StartDainoProcess ( dainoProcess )  
-- import Uniform.StartApp ( startProgWithTitle )  
import UniformBase 
-- ( Text, NoticeLevel(NoticeLevel0), unlinesT )  
import Data.Version

-- programName, progTitle :: Text
-- programName = "daino" :: Text
-- progTitle = "constructing a static site generator" :: Text
progVersion = "0.1.5.2":: Text

-- the process is centered on the current working dir

main :: IO ()
main =
    startProg
        (unwords' [progName, progTitle, progVersion])
        ( do
            flags :: PubFlags <-
                parseArgs2input
                    -- sourceDirTestSite 
                    --  add a delete flag
                    ( unlinesT
                        [ "the flags to select what is included:"
                        , "default is publish and public included"
                        , "\n -p private"
                        , "\n -d drafts"
                        -- , "\n -o old"
                        , "\n -t test (use data in package, continue)"
                        , "\n -T test (use data in package, fresh start)"
                        , "\n -q quick (not producing the pdfs, which is slow down)"
                        , "\n -w start to watch the files for changes and rebake (implies -s)"
                        , "\n -s start local server (port is fixed in siteHeader)"
                        -- , "\n -u upload to external server (not yet implemented"
                        ]
                    )
                    "list flags to include"
            dainoProcess NoticeLevel2 flags
        )
