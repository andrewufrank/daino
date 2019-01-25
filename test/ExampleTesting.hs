-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main     where      -- must have Main (main) or Main where


--import System.Exit

import           Test.Framework

import {-@ HTF_TESTS @-} Lib.Bake_test   -- pay attention to HTF_TESTS !
import {-@ HTF_TESTS @-} Lib.Foundation_test   -- pay attention to HTF_TESTS !
import {-@ HTF_TESTS @-} Lib.Pandoc_test   -- pay attention to HTF_TESTS !
import {-@ HTF_TESTS @-} Lib.Templating_test   -- pay attention to HTF_TESTS !

-- main =  do  -- the local tests only
--     putStrLn "HTF ExampleTest.hs:\n"
--     r <- htfMain htf_thisModulesTests
--     putStrLn ("HTF end ExampleTesting.hs test:\n" ++ show r)
--     return ()
main ::  IO ()
main =  do  -- with tests in other modules
    putStrLn "HTF ExampleTest.hs:\n"
    p <- htfMain htf_importedTests
    putStrLn ("HTF end StringConversion.hs test:\n" ++ show p)
    return ()


