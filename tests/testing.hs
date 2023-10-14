----------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-} 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports     #-}

-- the main for the systematic tests A to B 
module Main where      -- must have Main (main) or Main where
  -- but file name be lower case 

import     Test.Framework
import Foundational.CmdLineFlags
import UniformBase
import ShakeBake.StartDainoProcess
import Text.Pretty.Simple
import qualified Data.Text.Lazy as L
import    {-@ HTF_TESTS @-}        Lib.Md2doc_test
import    {-@ HTF_TESTS @-}        Lib.Docrep2panrep_test

default (Text)
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

main4 :: IO () 
-- | run the experiment site base from a fresh start 
-- start s in separate process 
main4  = do 
    putIOwords ["test publish all dainoSite files"]
    p <- runErr $ do 
        dainoProcess NoticeLevel0 (testFlags {serverFlag = True}) 
    putIOwords ["main4 server runs! end", showT p]

main4Re  = do 
    putIOwords ["test publish all dainoSite files"]
    p <- runErr $ dainoProcess NoticeLevel0 flagRe
    putIOwords ["main4Re end", showT p]

main4Ren  = do 
    putIOwords ["pdf and tufte - test publish all dainoSite files"]
    p <- runErr $ do 
        dainoProcess NoticeLevel0 (testFlags {tufteFlag = True
                                , pdfFlag = True
                                , newStartFlag = True}) 
    putIOwords ["main4 server runs! end", L.toStrict $ pShow p]
main4en  = do 
    putIOwords ["pdf and tufte - test publish all dainoSite files"]
    p <- runErr $ do 
        dainoProcess NoticeLevel0 (testFlags {tufteFlag = True
                                , pdfFlag = True }) 
    putIOwords ["main4 server runs! end", L.toStrict $ pShow p]
flags = testFlags
                {
                serverFlag = True  
                }
flagsR = testFlags
                {
                serverFlag = True 
                , newStartFlag = True -- R
                -- , pdfFlag = True   -- q 
                }
flagRe = testFlags
                {  newStartFlag = True -- R
                , tufteFlag = True -- e 
                -- , pdfFlag = True   -- q 
                }