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
 must start in dir with settings3.yaml
-}
module Main where -- must have Main (main) or Main where

import ShakeBake.CmdLineArgs  
import Foundational.LayoutFlags  
import ShakeBake.StartSSGprocess  
import Uniform.Convenience.StartApp  
import UniformBase  

programName, progTitle :: Text
programName = "ssgBake" :: Text
progTitle = "constructing a static site generator 0.0.4.4" :: Text

-- the process is centered on the current working dir

main :: IO ()
main =
    startProgWithTitle
        programName
        progTitle
        ( do
            flags :: PubFlags <-
                parseArgs2input
                    sourceDirTestSite 
                    --  add a delete flag
                    ( unlinesT
                        [ "the flags to select what is included:"
                        , "default is nothing included"
                        , "\n -p publish"
                        , "\n -d drafts"
                        , "\n -o old"
                        , "\n -t test (use data in package)"
                        , "\n -q quick (not producing the pdfs)"
                        , "\n -w start to watch the files for changes and rebake (implies -s s cancels -u"
                        , "\n -s start local server (port is fixed in settings)"
                        , "\n -u upload to external server (not yet implemented"
                        ]
                    )
                    "list flags to include"
            ssgProcess NoticeLevel0 flags
        )
