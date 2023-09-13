----------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-} 
{-# LANGUAGE PackageImports     #-}
{-# LANGUAGE OverloadedStrings #-}

-- the main for the systematic tests A to B 
module Main where      -- must have Main (main) or Main where
  -- but file name be lower case 

import     Test.Framework
import Text.Pretty.Simple
import qualified Data.Text.Lazy as L

-- import    {-@ HTF_TESTS @-}        Lib.Md2doc_test
-- import    {-@ HTF_TESTS @-}        Lib.Docrep2panrep_test
-- import    {-@ HTF_TESTS @-}        Lib.Panrep2html_test
-- import    {-@ HTF_TESTS @-}        Lib.Template_test
-- import    {-@ HTF_TESTS @-}        Lib.ReadSettingFile_test

-- import    {-@ HTF_TESTS @-}        Lib.IndexCollect_test
-- -- --                 -- braucht md2doc zuerst
-- import    {-@ HTF_TESTS @-}        Lib.Doc2html_test
-- import    {-@ HTF_TESTS @-}        Lib.Panrep2pdf_test
-- import    {-@ HTF_TESTS @-}        Lib.IndexMake_test
import    {-@ HTF_TESTS @-}        Lib.DainoTest_test
        -- braucht doc2html zuerst

import UniformBase 
import Foundational.CmdLineFlags
import ShakeBake.StartDainoProcess ( dainoProcess )

default (Text)

main :: IO ()
main = mainTest  -- for a different function name (main is in daino)

mainTest        -- with tests in other modules
  = do
    putStrLn "HTF ExampleTest.hs:\n"
    putIOwords ["baseline test for DainoSite fresh start"]
    -- runErrorVoid $ createDirIfMissing' "/home/frank/.daino"
    -- is in siteHeader.yaml testDir  - must correspond
    p <- htfMain htf_importedTests
    putStrLn
      ("HTF end ExampleTest.hs test:\n" ++ show p ++ "\nEND HTF ExampleTest")
    return ()

main4t :: IO () 
-- | run just the test for changed 
-- start s in separate process 
-- does not build pdf's if not previously done
main4t = do 
    putIOwords ["test to re-publish the changed dainoSite files"]
    p <- runErr $ do 
            dainoProcess NoticeLevel0 testFlags
                {testFlag = True 
                -- , testNewFlag = True -- T 
                -- , pdfFlag = True   -- q 
                }
    putIOwords ["main4t end", showT p]

main4T :: IO () 
-- | run the test  site base from a fresh start 
-- start s in separate process 
main4T = do 
    putIOwords ["test publish all dainoSite files"]
    p <- runErr $ do 
            dainoProcess NoticeLevel0 testFlags
                {testFlag = True 
                , testNewFlag = True -- T 
                -- , pdfFlag = True   -- q 
                }
    putIOwords ["main4T end", showT p]

main4Te :: IO () 
-- | run the test stie for tufte from fresh start 
-- start s in separate process 
main4Te = do 
    putIOwords ["test publish all dainoSite files"]
    p <- runErr $ do 
            dainoProcess NoticeLevel0 testFlags
                {testFlag = True 
                , testNewFlag = True -- T 
                , tufteFlag = True
                -- , pdfFlag = True   -- q 
                }
    putIOwords ["main4T end", showT p]

main4qT :: IO () 
-- | run just the test for changed 
-- start s in separate process 
main4qT = do 
    putIOwords ["test to publish all html dainoSite files"]
    p <- runErr $ do 
            dainoProcess NoticeLevel0 testFlags
                {testFlag = True 
                , testNewFlag = True -- T 
                -- , pdfFlag = True   -- n 
                }
    putIOwords ["main4qT end", showT p]
    
    main4qT :: IO () 

-- | run just the test for changed 
-- start s here  - do not close terminal later
main4st = do 
    putIOwords ["test to publish all html dainoSite files"]
    p <- runErr $ do 
            dainoProcess NoticeLevel0 testFlags
                {testFlag = True 
                , serverFlag = True -- s
                -- , testNewFlag = True -- T 
                -- , pdfFlag = True   -- n 
                }
    putIOwords ["main4qT end", showT p]    

-- | run all from fresh to produce base html and pdf 
--  
main4Tn = do 
    putIOwords ["test to publish all html dainoSite files"]
    p <- runErr $ do 
            dainoProcess NoticeLevel0 testFlags
                {testFlag = True 
                -- , serverFlag = True -- s
                , testNewFlag = True -- T 
                , pdfFlag = True   -- n 
                }
    putIOwords ["main4qT end", showT p] 
      
-- | run all from fresh to produce tufte html and pdf 
--  
main4Tne = do 
    putIOwords ["test to publish all html dainoSite files"]
    let tf = testFlags
                {testFlag = True 
                -- , serverFlag = True -- s
                , testNewFlag = True -- T 
                , pdfFlag = True   -- n 
                , tufteFlag = True   -- e 
                }
    p <- runErr $ do 
            dainoProcess NoticeLevel0 tf
    putIOwords ["main4Tne end", L.toStrict $ pShow p, showT tf]    