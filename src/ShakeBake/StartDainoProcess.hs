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
-- import Path.IO (getHomeDir, createDirLink, getSymlinkTarget, removeDirLink)
-- import System.Posix.Files (readSymbolicLink,createSymbolicLink)

dainoProcess :: NoticeLevel -> PubFlags -> ErrIO ()
dainoProcess debug flags = do
    let useTestSite = (testFlag flags || testNewFlag flags)
    putIOwords ["dainoProcess 1 useTestSite", showT useTestSite]
    currDir :: Path Abs Dir  <- currentDir 

    sett4 <- if useTestSite 
        then do
            sett4test <- callIO $ getDataFileName "docs/site/dough/settings3.yaml"  -- no error if not existing
            putIOwords ["dainoProcess 2test settingsFile",  showT sett4test]

            let sett4testP = makeAbsFile sett4test
            -- existSett <- doesFileExist' (sett4testP) 
            -- let dough4testAbsDir = makeAbsDir dough4test
            -- putIOwords ["dainoProcess 2test settingsFile",  showT (dough4testAbsDir </> settingsFileName)]

            sett2 <- readSettings debug sett4testP
        -- check if dough is present (not available if build from hackage)
            let doughPtest = currDir </> (makeRelDir "docs/site/dough")
            doughExist <- doesDirExist' doughPtest 
            unless doughExist $
                    errorT ["dainoProcess 2", "error dough not present", "must install on copy, test site data not available when installed from Hackage"]

            homeDir4 <- getHomeDir
            return sett2 {siteLayout = layoutDefaults doughPtest homeDir4}

        else do
            readSettings debug (currDir </> settingsFileName) 


-- put a link to theme into dough/resources
    let themeDir1 = themeDir (siteLayout sett4) :: Path Abs Dir
    let doughP = doughDir (siteLayout sett4) :: Path Abs Dir
    let link1 =  doughP </> (makeRelDir resourcesName) </> (makeRelDir themeName) :: Path Abs Dir
    let target1 = themeDir1  :: Path Abs Dir
    putIOwords ["dainoProcess 3 check simlink \n    target   ",  showT target1
                                            , "\n    linked to", showT link1]
    linkExists <- doesDirExist' link1
    targetOK <- if linkExists 
        then do
            targetNow <- getSymlinkTarget link1
            putIOwords ["dainoProcess 5 current \n    target   ",  showT targetNow]
            if (makeAbsDir targetNow) == target1 then return True
                else do 
                    removeDirLink link1
                    putIOwords ["dainoProcess remove previous link"]

                    return False

        else do
            return False 

    unless targetOK $ do 
            putIOwords ["dainoProcess 4 create simlink \n    target   ",  showT target1
                                            , "\n    linked to", showT link1]
            createDirLink  ( target1) ( link1)


-- set the currentWorkingDir CWD to doughDir

    putIOwords ["\n dainoProcess"
        , "currDir is doughP", showT currDir
        ]
    putIOwords ["\ndainoProcess starts baking with"
        , "siteLayout" , showT (siteLayout sett4) 
        ]
    setCurrentDir doughP

    if watchFlag flags -- implies server
        then mainWatch debug sett4 flags 
        else do
            when (testNewFlag flags) $ do
                let bakedP = bakedDir (siteLayout sett4)
                deleteDirRecursive bakedP 
            shakeAll debug sett4 flags ""
            -- the last is the filename that caused the shake call
            when (serverFlag flags) $ do 
                runScotty (localhostPort sett4) 
                    (bakedDir (siteLayout sett4)) 
                    (makeRelFile "index.html") 
                    -- was landingPageName
                putIOwords ["server started on "
                            , showT (localhostPort sett4)]

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

