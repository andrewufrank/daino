-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where      -- must have Main (main) or Main where

--import System.Exit
-- import System.Directory (createDirectoryIfMissing)
-- keep this because it is in IO (not ErrIO)
import           Test.Framework
import           Lib.Shake2  -- just to test ghci
import           Uniform.FileIO
import           Uniform.WebServer (runScotty)
import Uniform.Time 
import Uniform.Ftp 
import           Lib.CmdLineArgs (allFlags, PubFlags(..))
import           Lib.Foundation (SiteLayout(..)
      , settingsFileName, testSettingsFileName)
-- import Uniform.Error
-- import {-@ HTF_TESTS @-} Lib.Shake2_test 
-- tests shake for test dough
-- issue with rule not producing file
-- import {-@ HTF_TESTS @-} ShakeStartTests
-- -- must run first because it produces the test values used later
-- -- uses layoutDefaults, not settings2.yaml
-- -- test dir must be ~/.SSG  -- the program name in foundation
-- ordinary tests (run without shakeStartTest)
import   {-@ HTF_TESTS @-}        Lib.Foundation_test  -- sets pageFn 
import    {-@ HTF_TESTS @-}       Lib.CheckInputs_test
-- ----    -- writes A : testLayout
-- ----    --  pageFn :: abs pandoc filenames
import   {-@ HTF_TESTS @-}        Lib.Pandoc_test
--             --  -> AD markdownToPandoc
--             -- -> AF pandocToContentHtml
--             -- -> AG (docValToAllVal)
-- --    -- test_pandoc_pageFn_pageMd_1 - pageFn -> pageMd : MarkdownText
-- --    -- AK :: MarkdownText -> BE  DocValue
-- --    -- Md ->AD :: Pandoc
-- --    -- AD -> AF :: DocValue
-- --import {-@ HTF_TESTS @-} Lib.Bake_test
-- --import {-@ HTF_TESTS @-} Lib.ReadSettingFile_test
-- -- not used -- import {-@ HTF_TESTS @-} Lib.Indexing_test
import  {-@ HTF_TESTS @-}         Lib.IndexMake_test
import    {-@ HTF_TESTS @-}       Lib.Templating_test  -- AG -> EG 
import   {-@ HTF_TESTS @-}        Lib.Shake2_test  -- AG -> EG 
import Uniform.Ftp 
import Lib.StartSSGprocess
-- --import {-@ HTF_TESTS @-} Lib.BibTex_test
--
--
---- main =  do  -- the local tests only
----     putStrLn "HTF ExampleTest.hs:\n"
----     r <- htfMain htf_thisModulesTests
----     putStrLn ("HTF end ExampleTesting.hs test:\n" ++ show r)
----     return ()
main :: IO ()
main        -- with tests in other modules
  = do
    putStrLn "HTF ExampleTest.hs:\n"
    runErrorVoid $ createDirIfMissing' "/home/frank/.SSG"
    -- is in settings.yaml testDir  - must correspond
    p <- htfMain htf_importedTests
    putStrLn
      ("HTF end ExampleTest.hs test:\n" ++ show p ++ "\nEND HTF ExampleTest")
    return ()

main2 :: IO ()
main2      -- just a simple bake for test
  = do
    putStrLn "main2"
    runErrorVoid
      $ do
        shakeAll testLayout testFlags ""
        -- the last is the filename that caused the shake call
        --  let landing = makeRelFile "landingPage.html"
        when (serverFlag testFlags) $
            runScotty 3099 (bakedDir testLayout) (landingPage testLayout)
        
        return ()

testFlags = zero { testFlag = True
                 , publishFlag = True
                 , serverFlag = False
                 , watchFlag = False
                 , settingsFile = testSettingsFileName  
                 , uploadFlag = True
                 }

main3 = runErrorVoid $ do 
    (a,s)  <- runStateT  
                 (ftpUploadDirsRecurse test1 (bakedDir testLayout) 
                      (makeAbsDir "/test.gerastree.at/"))
                 ftp0
                 
    return () 

lastUpload = read "2019-04-11 12:00:00 UTC" :: UTCTime
test1 = testNewerModTime lastUpload 

main4 = runErrorVoid $ ssgProcess testFlags