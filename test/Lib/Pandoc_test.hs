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

module Lib.Pandoc_test  -- (openMain, htf_thisModuelsTests)
     where

import           Test.Framework
import Uniform.Test.TestHarness
import Lib.Foundation (progName)
import Lib.FileMgt
import Lib.Pandoc
--import Text.Read

readMarkdownFile8 :: String  -> ErrIO MarkdownText
readMarkdownFile8 fnn = read8 (makeAbsFile fnn) markdownFileType
-- uses files to be copied to dough
--
test_pandoc_11_A_K, test_pandoc_12_A_K :: IO ()
test_pandoc_11_A_K = test1FileIO progName  "resultAA1" "resultAK1" readMarkdownFile8
test_pandoc_12_A_K = test1FileIO progName  "resultAA2" "resultAK2" readMarkdownFile8

markdownToHTML4xdebug ::  MarkdownText -> ErrIO DocValue

markdownToHTML4xdebug = markdownToHTML4x False

test_pandoc_11_B_E, test_pandoc_12_B_E :: IO ()
test_pandoc_11_B_E = test1FileIO progName   "resultAK1" "resultBE1"  markdownToHTML4xdebug
test_pandoc_12_B_E = test1FileIO progName   "resultAK2" "resultBE2" markdownToHTML4xdebug


instance  ShowTestHarness MarkdownText where

instance  ShowTestHarness DocValue where
