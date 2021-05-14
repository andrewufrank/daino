----------------------------------------------------------------------
--
-- Module      :   watching files for changes
-- restart bake
-- include running server
----------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module ShakeBake.Watch where

import Lib.CmdLineArgs (PubFlags (..))
import Foundational.Foundation (SiteLayout (..), landingPageName)
import ShakeBake.Shake2 (shakeAll)
import Uniform.Watch
  ( Glob (..),
    WatchOpType,
    makeWatch, --  mainWatch2, forkIO, killThread)
    watchMain,
  )
import Uniform.WebServer (Port, runScotty)
import UniformBase

mainWatch :: Bool -> SiteLayout -> PubFlags -> Port -> ErrIO ()
-- | the landing page must be given here because it is special for scotty
-- and the name of the banner imgage which must be copied by shake
mainWatch debug layout flags bakedPort = do
  let bakedPath = bakedDir layout
      doughPath = doughDir layout
  let watchDough2, watchThemes2 :: WatchOpType
      watchDough2 =
        makeWatch
          doughPath
          (shakeAll debug layout flags)
          [Glob "**/*.md", Glob "**/*.bib", Glob "**/*.yaml"]

      watchThemes2 =
        makeWatch
          doughPath
          (shakeAll debug layout flags)
          [ Glob "**/*.yaml",
            Glob "**/*.dtpl",
            Glob "**/*.css",
            Glob "**/*.jpg",
            Glob "**/*.JPG"
          ]

  watchMain
    [watchDough2, watchThemes2]
    ( runScotty
        bakedPort
        bakedPath
        landingPageName -- default  (landingPage layout)
    )

