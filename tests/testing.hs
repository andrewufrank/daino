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

-- import    {-@ HTF_TESTS @-}        Lib.Example_test
-- import    {-@ HTF_TESTS @-}        Lib.ReadSettingFile_test
-- import    {-@ HTF_TESTS @-}        Lib.Md2doc_test

-- import    {-@ HTF_TESTS @-}        Lib.Indexing_test
import    {-@ HTF_TESTS @-}        Lib.Doc2html_test
-- import    {-@ HTF_TESTS @-}        Lib.Panrep2pdf_test
-- import    {-@ HTF_TESTS @-}        Lib.IndexMake_test


main :: IO ()
main = mainTest  -- for a different function name (main is in ssgbake)

mainTest        -- with tests in other modules
  = do
    putStrLn "HTF ExampleTest.hs:\n"
    -- runErrorVoid $ createDirIfMissing' "/home/frank/.SSG"
    -- is in settings.yaml testDir  - must correspond
    p <- htfMain htf_importedTests
    putStrLn
      ("HTF end ExampleTest.hs test:\n" ++ show p ++ "\nEND HTF ExampleTest")
    return ()

-------------------OLD ----------------------------
-- OLD --
-- import {-@ HTF_TESTS @-} Lib.Shake2_test 
-- tests shake for test dough
-- issue with rule not producing file

-- import {-@ HTF_TESTS @-} ShakeStartTests
-- -- must run first because it produces the test values used later
-- -- uses def, not settings2.yaml
-- -- test dir must be ~/.SSG  -- the program name in foundation

-- ordinary tests (run without shakeStartTest)

-- import   {-@ HTF_TESTS @-}        Foundational.LayoutFlags_test  
                        -- sets pageFn 
-- import    {-@ HTF_TESTS @-}       Lib.CheckInputs_test
-- -- -- ----    -- writes A : testLayout
-- -- -- ----    --  pageFn :: abs pandoc filenames
-- -- import   {-@ HTF_TESTS @-}        Lib.Pandoc_test
-- -- --             --  -> AD markdownToPandoc
-- -- --             -- -> AF pandocToContentHtml
-- -- --             -- -> AG (docValToAllVal)
-- -- -- --    -- test_pandoc_pageFn_pageMd_1 - pageFn -> pageMd : MarkdownText
-- -- -- --    -- AK :: MarkdownText -> BE  DocValue
-- -- -- --    -- Md ->AD :: Pandoc
-- -- -- --    -- AD -> AF :: DocValue
-- -- -- --import {-@ HTF_TESTS @-} Lib.ReadSettingFile_test
-- import {-@ HTF_TESTS @-} Lib.Indexing_test
-- import  {-@ HTF_TESTS @-}         Lib.IndexMake_test
-- -- import    {-@ HTF_TESTS @-}       Lib.Templating_test  -- AG -> EG 
-- -- import Uniform.Ftp 
-- -- import Lib.StartSSGprocess
-- -- -- --import {-@ HTF_TESTS @-} Lib.BibTex_test

--     ---- for ssgCheck
-- import {-@ HTF_TESTS @-} Lib.CheckProcess_test  -- (res11)
-- import Lib.CheckProcess        -- for direct calls 
--
-- import {-@ HTF_TESTS @-} Lib.Bake_test
-- import   {-@ HTF_TESTS @-}  Lib.Shake2_test  -- AG -> EG 

--
---- main =  do  -- the local tests only
----     putStrLn "HTF ExampleTest.hs:\n"
----     r <- htfMain htf_thisModulesTests
----     putStrLn ("HTF end ExampleTesting.hs test:\n" ++ show r)
----     return ()
--------------END OLD 
-- main2 :: IO ()
-- main2      -- just a simple bake for test
--    = do
--     putStrLn "main2"
--     runErrorVoid  $ do
--             res <-  res11  -- from checkProcess_test
--             putIOwords [s2t "res11", showT $ res] 
--             return ()
--     return ()

-- mainCheck :: IO ()
-- mainCheck      -- just a simple bake for test
--                 -- checks the completeness of labels 
--    = do
--     putStrLn "mainCheck"
--     runErrorVoid  $ do
--         -- sitefn :: FilePath 
--         let sitefn = "/home/frank/Workspace11/ssg/docs/site/dough/settings2" 
--         res <-  checkProcess True sitefn 
--         putIOwords [s2t "res11", showT $ res] 
--         return ()
--     return ()

    
-- defs = zero { testFlag = True
--                  , publishFlag = True
--                  , serverFlag = True
--                  , watchFlag = True
--                  , settingsFile = testSettingsFileName  
--                  , uploadFlag = False
--                  }

-- main3 = runErrorVoid $ do 
--     (a,s)  <- runStateT  
--                  (ftpUploadDirsRecurse test1 (bakedDir testLayout) 
--                       (makeAbsDir "/test.gerastree.at/"))
--                  ftp0
                 
--     return () 

-- lastUpload = read "2019-04-11 12:00:00 UTC" :: UTCTime
-- test1 = testNewerModTime lastUpload 

-- main4 = runErrorVoid $ ssgProcess testFlags