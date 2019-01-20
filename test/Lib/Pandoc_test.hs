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

--import Uniform.Strings
--import Uniform.Filenames
import Lib.Foundation (progName)
import Lib.FileMgt
import Lib.Pandoc
import Text.Read


test_bake_11_B_E, test_bake_12_B_E :: IO ()
test_bake_11_B_E = test1FileIO progName   "resultAK11" "resultBE11"  markdownToHTML4x
test_bake_12_B_E = test1FileIO progName   "resultAK12" "resultBE12" markdownToHTML4x


instance  ShowTestHarness MarkdownText where
    -- to avoid the additional "" added when show text
    -- but the read must compensate!
    -- this is necessary that json files (and other with "") can be read
    showTestH = show -- t2s . unMarkdownText
--    readTestH = readNote "showTestHarness Text" . show
    readTestH2 msg = readNote (  msg) . show
    readTestH2e msg = readEither . show

instance  ShowTestHarness DocValue where
    -- to avoid the additional "" added when show text
    -- but the read must compensate!
    -- this is necessary that json files (and other with "") can be read
    showTestH = show -- t2s . unMarkdownText
--    readTestH = readNote "showTestHarness Text" . show
    readTestH2 msg = readNote (  msg) . show
    readTestH2e msg = readEither . show
