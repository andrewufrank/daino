
------------------------------------------------------------------------------
--
-- Module      :   watching files for changes
    -- restart bake 
    -- include running server 


-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE PartialTypeSignatures     #-}

module Lib.Watch  where

import           Uniform.Strings         hiding ( (</>) )
import           Uniform.Filenames
import           Uniform.FileStrings
import           Uniform.Convenience.StartApp

import           Control.Concurrent
import           Lib.Foundation                 ( SiteLayout(..)
                                                -- , layoutDefaults
                                                , templatesDirName
                                                , staticDirName
                                                , resourcesDirName
                                                , bannerImageFileName 
                                                , settingsFileName
                                                -- , landingPageName
                                                )


import           Lib.Shake2                      ( shakeAll
                                                -- , shakeDelete
                                                )
import           Lib.ReadSettingFile
import Lib.CmdLineArgs (PubFlags(..))
import Uniform.Watch (mainWatch2)
import Uniform.WebServer 



mainWatch :: SiteLayout -> PubFlags -> Port ->   ErrIO ()
-- the landing page must be given here because it is special for scotty 
-- and the name of the banner imgage which must be copied by shake
mainWatch layout flags bakedPort   = 
    do 
        let bakedPath = bakedDir layout
            bannerImageFileName = bannerImage layout
        bracketErrIO
            (do  -- first
                shakeAll layout flags ""
                watchDoughTID     <- callIO 
                        $ forkIO (runErrorVoid $ watchDough layout flags)
                watchTemplatesTID <- callIO 
                        $ forkIO (runErrorVoid $ watchThemes layout flags)
                runScotty bakedPort bakedPath (landingPage layout)
                return (watchDoughTID, watchTemplatesTID)
            )
            (\(watchDoughTID, watchTemplatesTID) -> do -- last
                putIOwords ["main watch  end"]
                callIO $ killThread (watchDoughTID)
                callIO $ killThread (watchTemplatesTID)
                return ()
            )
            (\_ -> do   -- during
                putIOwords ["shake run"]
                -- brackets the runs of shake runs 
                putIOwords ["shake run end "]
                return ()
            )



watchDough :: SiteLayout -> PubFlags -> ErrIO ()
watchDough  layout flags = mainWatch2 (shakeAll  layout flags) 
                (doughDir layout)    -- :: Path Abs Dir
                ["md", "bib", "yaml"]  :: ErrIO ()

-- themesDir = (themeDir layout) </> templatesDirName :: Path Abs Dir

watchThemes :: SiteLayout -> PubFlags -> ErrIO ()
watchThemes  layout flags = mainWatch2 (shakeAll  layout flags)
                (themeDir layout </> templatesDirName )  -- :: Path Abs Dir
                ["yaml", "dtpl", "css", "jpg"] :: ErrIO () 
-- add copy static files ...






