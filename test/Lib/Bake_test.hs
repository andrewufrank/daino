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

module Lib.Bake_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import Uniform.Test.TestHarness

--import Uniform.Strings
--import Uniform.Filenames
import Lib.Foundation (progName)
import Lib.Bake
import Lib.FileMgt
--import Text.Read

test11, test12 ::  Path Rel File
test11 = makeRelFile "Blog/postwk.md"
test12 = makeRelFile "PublicationList/postWithReference.md"

bakeOneFileDebug = bakeOneFile False

test_bake_11_A_M, test_bake_12_A_M :: IO ()
test_bake_11_A_M = testVar0FileIO progName  test11 "resultAM11" bakeOneFileDebug
test_bake_12_A_M = testVar0FileIO progName  test12 "resultAM12" bakeOneFileDebug

test_bake_11_A_L, test_bake_12_A_L :: IO ()
test_bake_11_A_L = testVar0File progName  test11 "resultAL11" showT
test_bake_12_A_L = testVar0File progName  test12 "resultAL12" showT

--readMarkdownFile fnn = read7 doughPath fnn markdownFileType

test_bake_11_A_K, test_bake_12_A_K :: IO ()
test_bake_11_A_K = testVar0FileIO progName  test11 "resultAK11" readMarkdownFile
test_bake_12_A_K = testVar0FileIO progName  test12 "resultAK12" readMarkdownFile


-- just testing read/write
compareRead fn t = do
                t2 <- readMarkdownFile fn
                let v = t==t2
                let res = if v then "ok" else (unwords' ["read t \n", showT t, "\nreread t2\n", showT t2])
                return res

test_bake_11_A_X :: IO ()
test_bake_11_A_X = testVar1FileIO progName  test11 "resultAK11" "resultAX11"  compareRead

instance  ShowTestHarness MarkdownText
