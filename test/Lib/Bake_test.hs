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

module Lib.Bake_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import Uniform.Test.TestHarness

--import Uniform.Strings
--import Uniform.Filenames
import Lib.Foundation (progName)
import Lib.Bake

test11, test12 ::  Path Rel File
test11 = makeRelFile "Blog/postwk.md"
test12 = makeRelFile "PublicationList/postWithReference.md"


test_bake_11_A_M, test_bake_12_A_M :: IO ()
test_bake_11_A_M = testVar0FileIO progName  test11 "resultAM11" bakeOneFile
test_bake_12_A_M = testVar0FileIO progName  test12 "resultAM12" bakeOneFile

test_bake_11_A_L, test_bake_12_A_L :: IO ()
test_bake_11_A_L = testVar0File progName  test11 "resultAL11" showT
test_bake_12_A_L = testVar0File progName  test12 "resultAL12" showT

