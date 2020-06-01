-----------------------------------------------------------------------------
--
-- Module      :   testing the new shake 
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports#-}

module Lib.Shake2_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import Uniform.Test.TestHarness


import Lib.Foundation (progName, SiteLayout (..), templatesDirName)
import Lib.Shake2
import Lib.Foundation_test (testLayout)
import Lib.Foundation (templatesImgDirName, bannerImageFileName)
import Lib.CmdLineArgs (allFlags)

test_shakeMD = 
    do 
        res <- shakeMD testLayout allFlags
                (doughDir testLayout)
                ((themeDir $ testLayout) `addFileName` ( templatesDirName)
                            :: Path Abs Dir)
                (makeAbsDir "/home/frank/.SSG/bakedTest")
                (templatesImgDirName `addFileName` bannerImageFileName)
        assertEqual res ()
        --  "Lib.Shake_test - files written to /home/frank/.SSG/bakedTest"
