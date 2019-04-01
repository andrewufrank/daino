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
{-# LANGUAGE StandaloneDeriving     #-}

module Lib.CheckInputs_test  -- (openMain, htf_thisModuelsTests)
     where


import Test.Framework
import Uniform.Strings
import Uniform.Test.TestHarness
import           Uniform.Pandoc
import Lib.Pandoc 

import Lib.CheckInput
import Lib.Foundation (progName, SiteLayout (..), layoutDefaults)


psIn = ["true", "publish", "draft", "old", "", "xx", "Publish", "Draft", "OLD"]
psRes =  [Just PSpublish, Just PSpublish, Just PSdraft, Just PSold,
     Nothing, Nothing, 
     Just PSpublish, Just PSdraft, Just PSold]
     
test_PS = assertEqual psRes (map (text2publish . Just) psIn)


test_PSnothing = assertEqual Nothing (text2publish Nothing)

----
test_checkOneMdFile_1, test_checkOneMdFile_2 :: IO ()
test_checkOneMdFile_1 = test1FileIO progName  "pageFn1" "TripleDoc1" checkOneMdFile
test_checkOneMdFile_2 = test1FileIO progName  "pageFn2" "TripleDoc2" checkOneMdFile
test_checkOneMdFile_3 = test1FileIO progName  "pageFn3" "TripleDoc3" checkOneMdFile
test_checkOneMdFile_4 = test1FileIO progName  "pageFn4" "TripleDoc4" checkOneMdFile
test_checkOneMdFile_5 = test1FileIO progName  "pageFn5" "TripleDoc5" checkOneMdFile
test_checkOneMdFile_6 = test1FileIO progName  "pageFn6" "TripleDoc6" checkOneMdFile

instance  ShowTestHarness TripleDoc

instance ShowTestHarness (Path Abs File)
     --