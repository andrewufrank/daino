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

--templateFile = addDir (themeDir testLayout) ("Templates/page33.dtpl" :: FilePath)

test_Defaults_1_A :: IO ()

test_Defaults_1_A
        =   testVar0File progName testLayout  "resultA1" showNice

test11, test12 ::  Path Rel File
test11 = makeRelFile "Blog/postwk.md"
test12 = makeRelFile "PublicationList/postWithReference.md"
-- these files must be copied to .SSG/dough !!

showAbsFile ::  Path Rel File -> Text
showAbsFile f = shownice $ addDir (doughDir testLayout) f

test_foundation_1_AA = testVar0File progName test11 "resultAA1" showAbsFile
test_foundation_2_AA = testVar0File progName test12 "resultAA2" showAbsFile



