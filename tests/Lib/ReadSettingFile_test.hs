 
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.ReadSettingFile_test where


import           Test.Framework
import           Uniform.Test.TestHarness
import UniformBase 
import Lib.Foundation

import           Lib.Templating  
import ShakeBake.ReadSettingFile

programName = "ssg"

settingsFile =  makeAbsFile "/home/frank/Workspace11/ssg/settings2.yaml"

-- (layout7, port7) <- readSettings True settingsFile 

test_settings = 
    testVar0FileIO programName  settingsFile "settings2File" (fmap fst . readSettings True)  

instance ShowTestHarness (Path Abs File)
instance ShowTestHarness SiteLayout

test_EmptyTestTrue = assertEqual 0 0

