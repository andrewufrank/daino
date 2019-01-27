-----------------------------------------------------------------------------
--
-- ModuKe      :   a test for HTF framework
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

module Lib.Bake_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import Uniform.Test.TestHarness

import Lib.Foundation (progName, SiteLayout (..), layoutDefaults)
import Lib.Bake
import Lib.FileMgt
import Lib.Foundation_test (testLayout)

readMarkdownFile8 :: Path Abs File -> ErrIO MarkdownText
readMarkdownFile8 fnn = read8 fnn markdownFileType
--    do
--        r :: ErrOrVal MarkdownText <- runErr $
--        let r2 = fromRightNote "wewr" r :: MarkdownText
--        return r2
--
test_bake_11_A_K, test_bake_12_A_K :: IO ()
test_bake_11_A_K = test1FileIO progName  "resultAA1" "resultAK1" readMarkdownFile8
test_bake_12_A_K = test1FileIO progName  "resultAA2" "resultAK2" readMarkdownFile8

--test11, test12 ::  Path Rel File
--test11 = makeRelFile "Blog/postwk.md"
--test12 = makeRelFile "PublicationList/postWithReference.md"

--test_bake
--bakeOneFileDebug md = bakeOneFile False md templateFile
    where fn =
--
--test_bake_11_A_M, test_bake_12_A_M :: IO ()
--test_bake_11_A_M = testVar0FileIO progName  test11 "resultAM11" bakeOneFileDebug
--test_bake_12_A_M = testVar0FileIO progName  test12 "resultAM12" bakeOneFileDebug
--
--test_bake_11_A_L, test_bake_12_A_L :: IO ()
--test_bake_11_A_L = testVar0File progName  test11 "resultAL11" showT
--test_bake_12_A_L = testVar0File progName  test12 "resultAL12" showT
--


---- just testing read/write
--compareRead fn t = do
--                t2 <- readMarkdownFile fn
--                let v = t==t2
--                let res = if v then "ok" else (unwords' ["read t \n", showT t, "\nreread t2\n", showT t2])
--                return res

--test_bake_11_A_X :: IO ()
--test_bake_11_A_X = testVar1FileIO progName  test11 "resultAK11" "resultAX11"  dscompareRead

instance  ShowTestHarness MarkdownText
instance  ShowTestHarness (Path Abs File)

fromRightNote = fromRight
