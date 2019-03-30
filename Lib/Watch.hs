
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
import Uniform.Watch (mainWatch2)
import Uniform.WebServer 


-- main2 :: ErrIO ()
-- main2 = do
--     putIOwords ["main2 start"]
--     wd          <- currentDir
--     (layout, port) <- readSettings (wd </> settingsFileName)
--     putIOwords ["main2", showT layout, showT port]

--     let bakedPath = bakedDir layout :: Path Abs Dir
--     createDirIfMissing' bakedPath
--     -- the directory can be missing or deleted intentionally

--     -- let templatesPath = themeDir layout </> templatesDirName :: Path Abs Dir
--     let resourcesPath = doughDir layout </> resourcesDirName :: Path Abs Dir
--     -- copy static resources (templates and dough)
--     copyDirRecursive resourcesPath (bakedPath </> staticDirName)
--     putIOwords
--         [ programName
--         , "copied all templates  files from"
--         , showT resourcesPath
--         , "to"
--         , showT bakedPath
--         ]
--     -- resources in dough are not needed for baked
-- --    let templatesPath =  (themeDir layout) </> templatesDirName :: Path Abs Dir
-- --    Pathio.copyDirRecur
-- --                         (unPath templatesPath) (unPath $ bakedPath </> staticDirName )
-- --    putIOwords [programName, "copied all templates  files"]

--     -- let landing = makeRelFile "landingPage.html"
--     mainWatch layout port bakedPath 

mainWatch :: SiteLayout -> Port ->   ErrIO ()
-- the landing page must be given here because it is special for scotty 
-- and the name of the banner imgage which must be copied by shake
mainWatch layout bakedPort   = 
    do 
        let bakedPath = bakedDir layout
            bannerImageFileName = bannerImage layout
        bracketErrIO
            (do  -- first
                shakeAll layout ""
                watchDoughTID     <- callIO 
                        $ forkIO (runErrorVoid $ watchDough layout)
                watchTemplatesTID <- callIO 
                        $ forkIO (runErrorVoid $ watchThemes layout )
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



watchDough :: SiteLayout -> ErrIO ()
watchDough layout  = mainWatch2 (shakeAll  layout) 
                (doughDir layout)    -- :: Path Abs Dir
                ["md", "bib", "yaml"]  :: ErrIO ()

-- themesDir = (themeDir layout) </> templatesDirName :: Path Abs Dir

watchThemes :: SiteLayout -> ErrIO ()
watchThemes layout  = mainWatch2 (shakeAll  layout)
                (themeDir layout </> templatesDirName )  -- :: Path Abs Dir
                ["yaml", "dtpl", "css", "jpg"] :: ErrIO () 
-- add copy static files ...






