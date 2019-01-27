-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans #-}

module Lib.Foundation_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import Uniform.Test.TestHarness

--import Uniform.Strings
import Lib.Foundation (progName, SiteLayout (..), layoutDefaults)
--import Uniform.Filenames

testLayout = layoutDefaults {
            doughDir = makeAbsDir "/home/frank/.SSG/dough"
            , bakedDir = makeAbsDir "/home/frank/.SSG/baked"
            , reportFile = makeAbsFile "/home/frank/.SSG/reportBakeAll.txt"
--            , templateDir = makeAbsDir "templates"
            , themeDir = makeAbsDir "/home/frank/.SSG/theme"
            }


test_Defaults_1_A :: IO ()

test_Defaults_1_A
        =   testVar0File progName testLayout  "resultA1" showNice



