
------------------------------------------------------------------------------
--
-- Module      :   watching files for changes
-- restart bake 
-- include running server 
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- {-# LANGUAGE PartialTypeSignatures     #-}
module Lib.Watch where

-- import           Uniform.Strings -- hiding ((</>))
import UniformBase
-- import           Uniform.Filenames
import           Lib.Foundation (SiteLayout(..),landingPageName)
--                      , templatesDirName
--                     , )
import           Lib.Shake2 (shakeAll)
import           Lib.CmdLineArgs (PubFlags(..))
import           Uniform.Watch (watchMain, Glob (..), WatchOpType
      , makeWatch) --  mainWatch2, forkIO, killThread)
import           Uniform.WebServer (Port, runScotty)

mainWatch :: Bool -> SiteLayout -> PubFlags -> Port -> ErrIO ()

-- the landing page must be given here because it is special for scotty 
-- and the name of the banner imgage which must be copied by shake
mainWatch debug layout flags bakedPort = do
  let bakedPath = bakedDir layout
      doughPath = doughDir layout
  -- bannerImageFileName = bannerImage layout
  let watchDough2, watchThemes2 :: WatchOpType
      watchDough2 = makeWatch doughPath (shakeAll debug layout flags) 
            [Glob "**/*.md", Glob "**/*.bib", Glob "**/*.yaml"]

      watchThemes2 = makeWatch doughPath (shakeAll debug layout flags) 
          [Glob "**/*.yaml", Glob "**/*.dtpl", Glob "**/*.css"
          , Glob "**/*.jpg", Glob "**/*.JPG"]

  watchMain [watchDough2, watchThemes2] 
            (runScotty bakedPort bakedPath 
                landingPageName -- default  (landingPage layout)
                )
  -- [WatchOpType] -> ErrIO () ->  ErrIO () 
  -- bracketErrIO
  --   (do
  --      -- first
  --      putIOwords ["mainWatch started"]
  --      shakeAll layout flags ""
  --      watchDoughTID <- callIO
  --        $ forkIO (runErrorVoid $ watchDough layout flags)
  --      watchTemplatesTID <- callIO
  --        $ forkIO (runErrorVoid $ watchThemes layout flags)
  --      runScotty bakedPort bakedPath (landingPage layout)
  --      return (watchDoughTID, watchTemplatesTID))
  --   (\(watchDoughTID, watchTemplatesTID)       -- last
  --    -> do
  --      putIOwords ["main watch  end"]
  --      callIO $ killThread (watchDoughTID)
  --      callIO $ killThread (watchTemplatesTID)
  --      return ())
  --   (\_         -- during
  --    -> do
  --      putIOwords ["shake run"]
  --      -- brackets the runs of shake runs 
  --      putIOwords ["shake run end "]
  --      return ())

-- watchDough :: SiteLayout -> PubFlags -> ErrIO ()
-- watchDough layout flags = mainWatch2
--   (shakeAll layout flags)
--   (doughDir layout)    -- :: Path Abs Dir
--   ["md", "bib", "yaml"] :: ErrIO ()

-- -- themesDir = (themeDir layout) </> templatesDirName :: Path Abs Dir
-- watchThemes :: SiteLayout -> PubFlags -> ErrIO ()
-- watchThemes layout flags = mainWatch2
--   (shakeAll layout flags)
--   (themeDir layout </> templatesDirName)  -- :: Path Abs Dir
--   ["yaml", "dtpl", "css", "jpg"] :: ErrIO ()
-- -- add copy static files ...


-- mainWatch 
-- testWatch2 = makeWatch  
--   (makeAbsDir "/home/frank/Workspace8/uniform/uniform-watch")
--   (\f -> putIOwords ["testWatch2", showT f]) 
--   [Glob "*.html", Glob "*.md"]




