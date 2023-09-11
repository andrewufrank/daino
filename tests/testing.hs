----------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-} 
{-# LANGUAGE PackageImports     #-}

-- the main for the systematic tests A to B 
module Main where      -- must have Main (main) or Main where
  -- but file name be lower case 

import     Test.Framework

import    {-@ HTF_TESTS @-}        Lib.Md2doc_test
import    {-@ HTF_TESTS @-}        Lib.Docrep2panrep_test
-- import    {-@ HTF_TESTS @-}        Lib.Panrep2html_test
-- import    {-@ HTF_TESTS @-}        Lib.Panrep2pdf_test
-- import    {-@ HTF_TESTS @-}        Lib.Template_test
-- import    {-@ HTF_TESTS @-}        Lib.ReadSettingFile_test

-- import    {-@ HTF_TESTS @-}        Lib.IndexCollect_test
-- -- --                 -- braucht md2doc zuerst
-- import    {-@ HTF_TESTS @-}        Lib.Doc2html_test
-- import    {-@ HTF_TESTS @-}        Lib.IndexMake_test
-- import    {-@ HTF_TESTS @-}        Lib.DainoTest_test
        -- braucht doc2html zuerst


main :: IO ()
main = mainTest  -- for a different function name (main is in daino)

mainTest        -- with tests in other modules
  = do
    putStrLn "HTF ExampleTest.hs:\n"
    -- runErrorVoid $ createDirIfMissing' "/home/frank/.daino"
    -- is in siteHeader.yaml testDir  - must correspond
    p <- htfMain htf_importedTests
    putStrLn
      ("HTF end ExampleTest.hs test:\n" ++ show p ++ "\nEND HTF ExampleTest")
    return ()
