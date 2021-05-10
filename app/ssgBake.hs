-------------------------------------------------------------------
--
-- Module      :   ssgBake
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
 must start in dir with settings2.yaml
-}
module Main where -- must have Main (main) or Main where

import Lib.CmdLineArgs (PubFlags (..), parseArgs2input)
import Lib.Foundation (
    settingsFileName,
 )
import Lib.StartSSGprocess (ssgProcess)
import Uniform.Convenience.StartApp (startProg)
import UniformBase (Text, unlinesT)

programName, progTitle :: Text
programName = "ssgBake" :: Text
progTitle = "constructing a static site generator x6 0.0.2.1" :: Text

-- the process is centered on the current working dir

main :: IO ()
main =
    startProg
        programName
        progTitle
        ( do
            flags :: PubFlags <-
                parseArgs2input
                    settingsFileName
                    --  add a delete flag
                    ( unlinesT
                        [ "the flags to select what is included:"
                        , "default is nothing included"
                        , "\n -p publish"
                        , "\n -d drafts"
                        , "\n -o old"
                        , "\n -t test (use data in package)"
                        , "\n -w start to watch the files for changes and rebake (implies -s s cancels -u"
                        , "\n -s start local server (port is fixed in settings)"
                        , "\n -u upload to external server"
                        ]
                    )
                    "list flags to include"
            ssgProcess False flags
        )
