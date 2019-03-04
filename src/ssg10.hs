-----------------------------------------------------------------------------
--
-- Module      :   the main for the sgg - no UI yet
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main     where      -- must have Main (main) or Main where

import Uniform.Convenience.StartApp
import Uniform.Filenames

import Lib.Bake
import Lib.Shake
--import Lib.Foundation (layoutDefaults, SiteLayout (..))
import Lib.ReadSettingFile

programName = "SSG10" :: Text
progTitle = "constructing a static site generator" :: Text

settingsfileName = makeRelFile "settings2"


main :: IO ()
main = startProg programName progTitle
             (do
                (layout2, _)  <- readSettings settingsfileName
                shake layout2 ""
                return ()
                )

