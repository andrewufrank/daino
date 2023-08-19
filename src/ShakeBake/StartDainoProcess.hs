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
import qualified Path.Posix as Path
import qualified System.FilePath.Posix as P
-- import System.Directory (canonicalizePath)
-- import Filesystem.Path.CurrentOS (decodeString, encodeString, collapse)

import Path.IO (resolveDir, getHomeDir)
-- , createDirLink, getSymlinkTarget, removeDirLink) -- now in fileio
-- import System.Posix.Files (readSymbolicLink,createSymbolicLink)

dainoProcess :: NoticeLevel -> PubFlags -> ErrIO ()
dainoProcess debug1 flags = do
    -- let debug1 = NoticeLevel2
    putIOwords ["dainoProcess 0 debug flags", showT debug1, showT flags]
    let useTestSite = (testFlag flags || testNewFlag flags)
    putIOwords ["dainoProcess 1 useTestSite", showT useTestSite]
    currDir :: Path Abs Dir  <- currentDir 
    let relSettingsFile :: Path Rel File = makeRelFile "settings3.yaml"
    let debug = if verboseFlag flags then NoticeLevel1 else debug1
    sett4dir <- if useTestSite 
        then do
            putInform debug ["dainoProcess 2test useTestSite"]
            return $   makeAbsDir "/home/frank/Workspace11/dainoSite"  
            -- return $ (Path.parent currDir) </> makeRelDir "dainoSite"  
  
        else if (P.isAbsolute (locationDir flags)) 
            then do 
                putIOwords ["dainoProcess 2test location abs dir",  showT (locationDir flags)]
                return $  (makeAbsDir . locationDir $ flags)  
            else if (P.isRelative (locationDir flags))
                then do 
                    putIOwords ["dainoProcess 5 location relative",  showT (locationDir flags)]
                    absdir <- resolveDir currDir (locationDir flags)
                    -- problem initial .. bad hack!
                     
                    -- canonFP <- liftIO $ canonicalizePath (locationDir flags)
                    -- let    absdir = makeAbsDir canonFP 
                        -- combPath = toFilePath currDir </> (locationDir $ flags) :: FilePath 
                        -- collPath = collapse . decodeString $ combPath   
                        -- absdir = makeAbsDir . encodeString $ collPath :: Path Abs Dir 
                    return absdir 
                    -- return . makeAbsDir . collapse $ (toFilePath currDir </> (locationDir $ flags))  -- path starting with .. possible 

            else 
                errorT ["dainoProcess 5 location not valid",  showT $ locationDir flags]
                 
    putInform debug ["dainoProcess 5 dir of settings file",  showT sett4dir]
    let sett4file = sett4dir </> relSettingsFile 
    putInform debug ["dainoProcess 5  settings file",  showT sett4file]
 
    existSett <- doesFileExist' (sett4file) 
    sett4 <- if existSett 
        then readSettings debug sett4file -- (currDir </> settingsFileName) 
        else  errorT ["dainoProcess 1", "error settingsFile not present in"
                    , showT sett4file 
                    , "perhaps need install dainoSite with `git clone https://github.com/andrewufrank/dainoSite.git"]            


-- put a link to theme into dough/resources
    let themeDir1 = themeDir (siteLayout sett4) :: Path Abs Dir
    let doughP = doughDir (siteLayout sett4) :: Path Abs Dir

    doughExist <- doesDirExist' doughP
    unless doughExist $
            errorT ["dainoProcess 2", "error dough not present", "install dainoSite with `git clone https://github.com/andrewufrank/dainoSite.git"]



    let link1 =  doughP </> (makeRelDir resourcesName) </> (makeRelDir themeName) :: Path Abs Dir
    let target1 = themeDir1  :: Path Abs Dir
    putInform debug ["dainoProcess 3 check simlink \n    target   ",  showT target1
                                            , "\n    linked to", showT link1]
    linkExists <- doesDirExist' link1
    targetOK <- if linkExists 
        then do
            targetNow <- getSymlinkTarget link1
            putInform debug ["dainoProcess 5 current \n    target for theme  ",  showT targetNow]
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
    putInform debug ["\ndainoProcess starts baking with"
        , "siteLayout" , showT (siteLayout sett4) 
        ]
    setCurrentDir doughP

    if watchFlag flags -- implies server
        then mainWatch debug sett4 flags 
        else do
            when (testNewFlag flags || restartFlag flags) $ do
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
    putInform debug ["dainoProcess", "again currDir as before", showT currDir, "\nwas doughP", showT doughP] 
    putInform debug ["dainoProcess done"]
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

