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
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports#-}

module Lib.Bake_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import Uniform.Test.TestHarness

import Lib.Foundation (progName, SiteLayout (..))
import Lib.Bake
-- import Lib.FileMgt
import Lib.Foundation_test (testLayout)
import Lib.Foundation (templatesDirName)
import Lib.CmdLineArgs (allFlags)
import Uniform.Json (AtKey(..), Value(..))
import Uniform.Pandoc -- (DocValue(..), unDocValue, docValueFileType)

blog1fn = makeAbsFile "/home/frank/Workspace8/ssg/docs/site/dough/Blog/blog1.md"  -- braucht extension
blog1res = makeAbsFile "/home/frank/Workspace8/ssg/docs/site/checks/Blog/blog1"  -- keine extension

test_bake2docval = testVar0FileIO "ssg" (blog1fn,blog1res) "bakeOneFile2docval" op2html
    where 
        op2html :: (Path Abs File, Path Abs File) -> ErrIO ()
        op2html (fn,resfn) = bakeOneFile2docval False allFlags fn testLayout resfn

test_docVal2html = testVar0FileIO "ssg" (blog1res,blog1res) "bakeDocValue2html" op2html
    where 
        op2html :: (Path Abs File, Path Abs File) -> ErrIO () 
        op2html (fn,resfn) = bakeDocValue2html True allFlags fn testLayout resfn

-- test_bake2html = testVar0FileIO "ssg" (blog1fn,blog1res) "bake2html" op2html
--     where 
--         op2html :: (Path Abs File, Path Abs File) -> ErrIO Text 
--         op2html (fn,resfn) = bakeOneFile2html False allFlags fn testLayout resfn
    -- writes in same dir but different extension

-- testVar0FileIO :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b)
--             => Text -> a -> FilePath -> (a-> ErrIO b) -> IO ()
-- the arguments
-- testVar0FileIO progName  a resfile op = do

test_bake2texsnip = testVar0FileIO "ssg" (blog1fn,blog1res) "bake2texsnip" op2texsnip
    where 
        op2texsnip :: (Path Abs File, Path Abs File) -> ErrIO () 
        op2texsnip (fn,resfn) = bakeOneFile2texsnip False allFlags fn testLayout resfn

-- test_bake2pdf = testVar0FileIO "ssg" (blog1res,blog1res) "bake2pdf" op2pdf
--     where 
--         op2pdf :: (Path Abs File, Path Abs File) -> ErrIO Text 
--         op2pdf (fn,resfn) = bakeOneTexSnip2pdf False allFlags fn testLayout resfn


instance ShowTestHarness () where


    --- OLD june 2020


--import Lib.Templating (Gtemplate(..), gtmplFileType, Dtemplate(..))
-- import Control.Lens
--import Data.Aeson
-- import Data.Aeson.Lens
-- import Data.Aeson
--import Data.Aeson.Encode.Pretty (encodePretty)
--import Data.ByteString.Lazy as BS (putStrLn)

-- test_findTemplate =
--     do
--         res <-  runErr $ do
--                     let source = "/home/frank/.SSG/landingPage.content.docval"
--                     val :: DocValue <- read8 (makeAbsFile source)  docValueFileType
--                     let val2 = unDocValue val  ::Value

--                     putIOwords ["test_findTemplate", "val2\n" ] -- ,  shownice $ val2]
-- --                    liftIO $ BS.putStrLn ( encodePretty$ val2)
--                     let ptemplate = getAtKey val2 "pageTemplate" 
--                         -- (val2) ^? key "pageTemplate" . _String
-- --                                :: Maybe FilePath
--                     putIOwords ["test_findTemplate", "found", showT ptemplate]
--                     return   ptemplate

--         assertEqual (Right (Just "page3"::Maybe Text))  res

--markdownToHTML4xdebug intext = do
--    pandoc <- markdownToPandoc False allFlags intext
--    pandocToContentHtml False pandoc
--
--test_pandoc_11_B_E, test_pandoc_12_B_E :: IO ()
--test_pandoc_11_B_E = test1FileIO progName   "resultB1" "resultBE1"  markdownToHTML4xdebug
--test_pandoc_12_B_E = test1FileIO progName   "resultB2" "resultBE2" markdownToHTML4xdebug
