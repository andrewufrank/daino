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

module Lib.Pandoc_test  -- (openMain, htf_thisModuelsTests)
     where

import           Test.Framework
import Uniform.Test.TestHarness
import Lib.Foundation (progName)
import Lib.FileMgt
import Lib.Pandoc
import Text.Read

markdownToHTML4xdebug = markdownToHTML4x False

test_bake_11_B_E, test_bake_12_B_E :: IO ()
test_bake_11_B_E = test1FileIO progName   "resultAK11" "resultBE11"  markdownToHTML4xdebug
test_bake_12_B_E = test1FileIO progName   "resultAK12" "resultBE12" markdownToHTML4xdebug


instance  ShowTestHarness MarkdownText where

instance  ShowTestHarness DocValue where
