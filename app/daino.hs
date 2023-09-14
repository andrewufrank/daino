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

import ShakeBake.StartDainoProcess ( dainoProcess )  
import UniformBase 
import Paths_daino (version)

progVersion = showT version  -- "0.1.5.2":: Text

-- the process is centered on the current working dir

main :: IO ()
main =
    startProg
        (unwords' [progName, progTitle, "\n", progVersion])
        ( do
            flags :: PubFlags <-
                parseArgs2input
                    -- sourceDirTestSite 
                    --  add a delete flag
                    ( unlinesT
                        [ "no flags give baseline, add"
                        , "\n -e for edward tufte style"
                        , "\n -n for nice pdf output"
                        , "\nthe flags to select what is included:"
                        , "default is publish and public included"
                        , "\n -p private"
                        , "\n -d drafts"
                        -- , "\n -o old"
                        , "\n -t test (use data in dainoSite, continue)"
                        , "\n -T test (use data in dainoSite, fresh start)"
                        , "\n -R to start fresh start with local directory"
                        -- , "\n -q quick (not producing the pdfs, which is slow)"
                        , "\n -w start to watch the files for changes and rebake (implies -s)"
                        , "\n -s start local server (port is fixed in siteHeader)"
                        , "\n -v make processing more verbose"

                        , "\n -l location of settingsFile"
                        -- , "\n -u upload to external server (not yet implemented"
                        ]
                    )
                    "list flags to include"
            -- dainoProcess debug flags -- produces debug output
            dainoProcess NoticeLevel0 flags
        )
