-----------------------------------------------------------------------------
--
-- Module      :   ssgBake
-- the main for the sgg - no UI yet
-- uses shake only to convert the md files
-- copies all resources
-- must start in dir with settings2.yaml

-- experimental: only the md files are baked 
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where      -- must have Main (main) or Main where

import           Uniform.Convenience.StartApp
import           Lib.CmdLineArgs
-- import           Uniform.FileIO          hiding ( (<.>)
                                                -- , (</>)
                                                -- )
-- import Uniform.Shake.Path
-- import           Uniform.Shake
import           Uniform.Error                  ( )
import           Uniform.WebServer

-- import           Lib.Bake
import           Lib.Shake2
import           Lib.ReadSettingFile
import           Lib.Foundation                 ( SiteLayout(..)
                                                -- , templatesDirName
                                                -- , staticDirName
                                                -- , resourcesDirName
                                                -- , templatesImgDirName
                                                , settingsFileName
                                                )


programName, progTitle :: Text
programName = "SSG10" :: Text
progTitle = "constructing a static site generator x6" :: Text


main :: IO ()
main = startProg programName progTitle
   (do
      inp :: Inputs <- parseArgs2input
         settingsFileName
         (unlinesT
            [ "the flags to select what is included:"
            , "default is nothing included"
            , "\n -p publish"
            , "\n -d drafts"
            , "\n -o old"
            , "\n -t test (use data in package)"
            , "\n -w start to watch the files for changes and rebake"
            , "\n -s start server (port is fixed in settings)"
            ]
         )
         "list flags to include"
      (layout2, port2) <- readSettings (settingsFile inp)
      shakeAll (bannerImage layout2) layout2  ""
      -- the last is the filename that caused the shake call
      --  let landing = makeRelFile "landingPage.html"
      runScotty port2 (bakedDir layout2) (landingPage layout2)
       -- callIO $ scotty port2 (site (bakedDir layout2))
      return ()
   )

