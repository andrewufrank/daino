 
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

-- | tests foundation.hs and readSettingFile
module Lib.ReadSettingFile_test where

import           Test.Framework
import           Uniform.Test.TestHarness
import UniformBase 
import Foundational.LayoutFlags
import Foundational.Filetypes4sites

import Uniform.Yaml

-- import           Lib.Templating  
import ShakeBake.ReadSettingFile
-- import Data.Aeson
import Uniform.Json  

programName = "ssg"

-- settingsFile3 = sourceDirTestDocs </> settingsFileName

-- (layout7, port7) <- readSettings True settingsFile 

test_settings = 
    testVar0FileIO programName  
        (sourceDirTestSite </> settingsFileName) 
        "settingsFile" 
        (fmap fst . readSettings NoticeLevel0 )  

test_checkSettings_def = do 
    res <- runErr $ do 
            (lay, por) <- readSettings NoticeLevel0 
                (sourceDirTestSite </> settingsFileName)
            return lay 
    assertEqual (Right layoutDefaults) res 

test_readSettings1 = do 
    res <- runErr $ do 
        let settingsfilename = (sourceDirTestSite) </> settingsFileName
        putIOwords [" settings file name", showT settingsfilename ]
        -- settingsTxt <- read8 settingsfilename yamlFileType
        s0 <- readYaml2value settingsfilename 
        putIOwords ["yaml read", showPretty s0 ]

        -- let s1 = eitherDecode   .  t2bl . unYAML $ settingsTxt  ::  Either String Settings 

        -- putIOwords ["json parsed", showT s1 ]

        return "zz"
    assertEqual (Right "x") res



instance ShowTestHarness (Path Abs File)
instance ShowTestHarness SiteLayout

test_EmptyTestTrue = assertEqual 0 0

