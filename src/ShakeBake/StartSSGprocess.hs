----------------------------------------------------------------------
--
-- Module      :   the  process
----------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports 
            -fno-warn-unused-imports 
            -fno-warn-unused-matches #-}

{- | to convert
              files in any input format to html
              orginals are found in dire doughDir and go to bakeDir
-}
module ShakeBake.StartSSGprocess (ssgProcess) where

import Lib.CmdLineArgs (PubFlags (..))
import Lib.Foundation (
    SiteLayout (..),
    landingPageName,
    testLastUploadFileName,
 )
import ShakeBake.ReadSettingFile (readSettings)
import ShakeBake.Shake2 (shakeAll)
import ShakeBake.Watch (mainWatch)
import Uniform.WebServer (runScotty)
import UniformBase

ssgProcess :: Bool -> PubFlags -> ErrIO ()
ssgProcess debug flags = do
    (layout2, port2) <- readSettings (settingsFile flags)

    -- read the time of the last upload
    uploadFileExist <- doesFileExist' testLastUploadFileName
    lastUpload <-
        if uploadFileExist
            then do
                lastUpload1 <- readFile2 testLastUploadFileName
                let lastUpload = read lastUpload1 :: UTCTime
                return lastUpload
            else return year2000

    -- let testWithLastTime  = testNewerModTime  lastUpload
    -- compare with year2000 if all should be uploaded

    if watchFlag flags -- implies server
        then mainWatch debug layout2 flags port2
        else do
            shakeAll debug layout2 flags ""
            -- the last is the filename that caused the shake call
            when (serverFlag flags) $
                runScotty port2 (bakedDir layout2) landingPageName
    -- sollte default index.html sein (landingPage layout2)
    -- when (uploadFlag flags) $ do
    --     (_,_) <- runStateT
    --         (ftpUploadDirsRecurse testWithLastTime (bakedDir layout2)
    --             (if testFlag flags then makeAbsDir "/ssg.gerastree.at/"
    --                         else makeAbsDir "/frank.gerastree.at/")
    --         )
    --             ftp0
    --     currentTime <- getCurrentTimeUTC
    --     writeFile2 testLastUploadFileName (show currentTime)

    --     putIOwords ["uploadTest completed", showT currentTime]

    putIOwords ["ssgBake done"]
    return ()