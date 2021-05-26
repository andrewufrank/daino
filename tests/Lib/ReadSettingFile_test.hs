 
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

-- import           Lib.Templating  
import ShakeBake.ReadSettingFile

programName = "ssg"

-- settingsFile3 = sourceDirTestDocs </> settingsFileName

-- (layout7, port7) <- readSettings True settingsFile 

test_settings = 
    testVar0FileIO programName  testSettingsFileName "settingsFile" (fmap fst . readSettings NoticeLevel0 )  

test_checkSettings_layoutDefaults = do 
    res <- runErr $ do 
            (lay, por) <- readSettings NoticeLevel0 testSettingsFileName
            return lay 
    assertEqual (Right layoutDefaults) res 

instance ShowTestHarness (Path Abs File)
instance ShowTestHarness SiteLayout

test_EmptyTestTrue = assertEqual 0 0

