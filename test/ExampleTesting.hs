-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main     where      -- must have Main (main) or Main where


--import System.Exit

import           Test.Framework

--import {-@ HTF_TESTS @-} ShakeStartTests
---- must run first because it produces the test values used later

--import {-@ HTF_TESTS @-} Lib.FileMgt_test

--import {-@ HTF_TESTS @-} Lib.Foundation_test
    -- writes A : testLayout
    --  pageFn :: abs pandoc filenames
--import {-@ HTF_TESTS @-} Lib.Pandoc_test
    -- test_pandoc_pageFn_pageMd_1 - pageFn -> pageMd : MarkdownText
    -- AK :: MarkdownText -> BE  DocValue
    -- AK ->AD :: Pandoc
    -- AD -> AF ::
--import {-@ HTF_TESTS @-} Lib.Bake_test
--import {-@ HTF_TESTS @-} Lib.ReadSettingFile_test
import {-@ HTF_TESTS @-} Lib.Indexing_test

--
---- main =  do  -- the local tests only
----     putStrLn "HTF ExampleTest.hs:\n"
----     r <- htfMain htf_thisModulesTests
----     putStrLn ("HTF end ExampleTesting.hs test:\n" ++ show r)
----     return ()
main ::  IO ()
main =  do  -- with tests in other modules
    putStrLn "HTF ExampleTest.hs:\n"
    p <- htfMain htf_importedTests
    putStrLn ("HTF end ExampleTest.hs test:\n" ++ show p ++ "\nEND HTF ExampleTest")
    return ()


