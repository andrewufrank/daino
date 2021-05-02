
------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports     #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans #-}

module Lib.Foundation_test  -- (openMain, htf_thisModuelsTests)
     where


import     "HTF"      Test.Framework
import Uniform.Test.TestHarness

import Lib.Foundation (progName, SiteLayout (..), layoutDefaults)


testLayout = layoutDefaults
--templateFile = addDir (themeDir testLayout) ("Templates/page33.dtpl" :: FilePath)

test_Defaults_1_A :: IO ()

test_Defaults_1_A
        =   testVar0File progName testLayout  "resultA1" showNice

test11, test12 ::  Path Rel File
test11 = makeRelFile "Blog/postwk.md"
test12 = makeRelFile "PublicationList/postWithReference.md"
test13 = makeRelFile "PublicationList/publistAF.md"
test14 = makeRelFile "PublicationList/index.md"
test15 = makeRelFile "Blog/SubBlog/index.md"
test16 = makeRelFile "Blog/index.md"
-- these files must be copied to .SSG/dough !!

showAbsFile ::  Path Rel File -> Text
showAbsFile f = showT $ addDir (doughDir testLayout) f

test_foundation_1_AA = testVar0File progName test11 "pageFn1" showAbsFile
test_foundation_2_AA = testVar0File progName test12 "pageFn2" showAbsFile
test_foundation_3_AA = testVar0File progName test13 "pageFn3" showAbsFile
test_foundation_4_AA = testVar0File progName test14 "pageFn4" showAbsFile
test_foundation_5_AA = testVar0File progName test15 "pageFn5" showAbsFile
test_foundation_6_AA = testVar0File progName test16 "pageFn6" showAbsFile



