-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main     where      -- must have Main (main) or Main where


--import System.Exit

import           Test.Framework

import {-@ HTF_TESTS @-} Lib.Foundation_test
    -- writes A : testLayout
    --  AA :: abs pandoc filenames
import {-@ HTF_TESTS @-} Lib.Pandoc_test
    -- AA -> AK : MarkdownText
    -- AK :: MarkdownText -> BE  DocValue
    -- AK ->AD :: Pandoc
    -- AD -> AF ::
import {-@ HTF_TESTS @-} Lib.Bake_test  -- final E
        --   -> resultMasterYaml  test_bake_MasterYaml
    -- -> resultMasterTemplate  test_bake_MasterTmpl
    -- ..-> masterYaml -> AK -> AG :: Markdown   test_Splice1
--                        spliceMarkdownT :: Text -> MarkdownText -> Text
    -- test_bakeCore1 = test3FileIO progName "resultMasterYaml"  "resultAG1" "resultMasterTemplate"
--                                         "resultAE1" bakeOneFileCoreT
--          test_bake_11_A_H = test2FileIO progName  "resultAF1" "resultMasterTemplate" "resultAH1" spliceTemplates
import {-@ HTF_TESTS @-} Lib.Templating_test
--      BE -> EF ::HTMLout
--          test_templating_11_E_F = test1FileIO progName   "resultBE1" "resultEF1"  applyTemplate3x

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


