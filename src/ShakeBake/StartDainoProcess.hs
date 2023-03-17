----------------------------------------------------------------------
--
-- Module      :   convert a homepage
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
              files in any input format (primarily md) 
                to html and to pdf 
              orginals are found in dire doughDir and go to bakeDir
-}

module ShakeBake.StartDainoProcess (dainoProcess) where

import ShakeBake.ReadSettingFile (readSettings)
import ShakeBake.Shake2 (shakeAll)
import ShakeBake.Watch (mainWatch)
import Uniform.WebServer (runScotty)
import Foundational.SettingsPage
import Foundational.CmdLineFlags
import Paths_daino  
import UniformBase

dainoProcess :: NoticeLevel -> PubFlags -> ErrIO ()
dainoProcess debug flags = do
    currDir :: Path Abs Dir  <- currentDir 
    dough4test <- callIO $ getDataFileName "docs/site/dough"
    let dough4testAbsDir = makeAbsDir dough4test
    putIOwords ["dainoProcess docs site dough", showT dough4testAbsDir]
    putIOwords ["dainoProcess test flags", showT (testFlag flags)]

    let settfn = if (testFlag flags || testNewFlag flags)   
            then  
                    -- getDataFileName :: FilePath -> IO FilePath
                    dough4testAbsDir </> settingsFileName 
            else currDir </> settingsFileName

    putIOwords ["dainoProcess 2 settfn", showT settfn]
    sett3 <- readSettings debug settfn 

-- set the currentWorkingDir CWD to doughDir
    let doughP = doughDir (siteLayout sett3)
-- it should be allways be the same, independent of start 
-- when started to convert the tests the CWD is not 
-- the same then when starting in a directory to convert

    putIOwords ["dainoProcess", "currDir"
        , showT currDir, "\nwill beomce doughP", showT doughP
        ]
    setCurrentDir doughP

    if watchFlag flags -- implies server
        then mainWatch debug sett3 flags 
        else do
            when (testNewFlag flags) $ do
                let bakedP = bakedDir (siteLayout sett3)
                deleteDirRecursive bakedP 
            shakeAll debug sett3 flags ""
            -- the last is the filename that caused the shake call
            when (serverFlag flags) $ do 
                runScotty (localhostPort sett3) (bakedDir (siteLayout sett3)) (makeRelFile "index.html") 
                    -- was landingPageName
                putIOwords ["server started on ", showT (localhostPort sett3)]

-- return the dir as set before
    setCurrentDir currDir
    putIOwords ["dainoProcess", "again currDir as before", showT currDir, "\nwas doughP", showT doughP] 
    putIOwords ["dainoProcess done"]
    return ()

-- settingsFileName :: Path Rel File
-- -- ^ the yaml file in which the siteHeader are fixec
-- settingsFileName = makeRelFile "settings3" -- the yaml file
-- testNew bakes all test data, test alone continue the previous test

-- idea to automate upload (before call to shakeAll)
    -- read the time of the last upload
    -- uploadFileExist <- doesFileExist' testLastUploadFileName
    -- lastUpload <-
    --     if uploadFileExist
    --         then do
    --             lastUpload1 <- readFile2 testLastUploadFileName
    --             let lastUpload = read lastUpload1 :: UTCTime
    --             return lastUpload
    --         else return year2000

    -- let testWithLastTime  = testNewerModTime  lastUpload
    -- compare with year2000 if all should be uploaded
   -- let layout2 = siteLayout sett3 
    -- let port2 = localhostPort sett3 

-- nach call to shake all 
   -- sollte default index.html sein (landingPage layout2)
    -- when (uploadFlag flags) $ do
    --     (_,_) <- runStateT
    --         (ftpUploadDirsRecurse testWithLastTime (bakedDir layout2)
    --             (if testFlag flags then makeAbsDir "/daino.gerastree.at/"
    --                         else makeAbsDir "/frank.gerastree.at/")
    --         )
    --             ftp0
    --     currentTime <- getCurrentTimeUTC
    --     writeFile2 testLastUploadFileName (show currentTime)

