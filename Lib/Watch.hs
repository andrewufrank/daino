
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

import           Uniform.Strings hiding ((</>))
import           Uniform.Filenames
import           Lib.Foundation (SiteLayout(..), templatesDirName)
import           Lib.Shake2 (shakeAll)
import           Lib.CmdLineArgs (PubFlags(..))
import           Uniform.Watch (mainWatch2, forkIO, killThread)
import           Uniform.WebServer (Port, runScotty)

mainWatch :: SiteLayout -> PubFlags -> Port -> ErrIO ()

-- the landing page must be given here because it is special for scotty 
-- and the name of the banner imgage which must be copied by shake
mainWatch layout flags bakedPort = do
  let bakedPath = bakedDir layout
  -- bannerImageFileName = bannerImage layout
  bracketErrIO
    (do
       -- first
       putIOwords ["mainWatch started"]
       shakeAll layout flags ""
       watchDoughTID <- callIO
         $ forkIO (runErrorVoid $ watchDough layout flags)
       watchTemplatesTID <- callIO
         $ forkIO (runErrorVoid $ watchThemes layout flags)
       runScotty bakedPort bakedPath (landingPage layout)
       return (watchDoughTID, watchTemplatesTID))
    (\(watchDoughTID, watchTemplatesTID)       -- last
     -> do
       putIOwords ["main watch  end"]
       callIO $ killThread (watchDoughTID)
       callIO $ killThread (watchTemplatesTID)
       return ())
    (\_         -- during
     -> do
       putIOwords ["shake run"]
       -- brackets the runs of shake runs 
       putIOwords ["shake run end "]
       return ())

watchDough :: SiteLayout -> PubFlags -> ErrIO ()
watchDough layout flags = mainWatch2
  (shakeAll layout flags)
  (doughDir layout)    -- :: Path Abs Dir
  ["md", "bib", "yaml"] :: ErrIO ()

-- themesDir = (themeDir layout) </> templatesDirName :: Path Abs Dir
watchThemes :: SiteLayout -> PubFlags -> ErrIO ()
watchThemes layout flags = mainWatch2
  (shakeAll layout flags)
  (themeDir layout </> templatesDirName)  -- :: Path Abs Dir
  ["yaml", "dtpl", "css", "jpg"] :: ErrIO ()
-- add copy static files ...






