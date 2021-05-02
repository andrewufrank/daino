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
-- {-# LANGUAGE TypeSynonymInstances  #-}
-- {-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports#-}

module Lib.CmdLineArgs_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import Uniform.Test.TestHarness

import Lib.Foundation
    ( progName, SiteLayout(..), templatesDirName )
import Lib.Bake
-- import Lib.FileMgt
import Lib.Foundation_test (testLayout)
import Uniform.Json (AtKey(..), Value(..))
import Uniform.Pandoc -- (DocValue(..), unDocValue, docValueFileType)
--import Lib.Templating (Gtemplate(..), gtmplFileType, Dtemplate(..))
-- import Control.Lens
--import Data.Aeson
-- import Data.Aeson.Lens
-- import Data.Aeson
--import Data.Aeson.Encode.Pretty (encodePretty)
--import Data.ByteString.Lazy as BS (putStrLn)

-- test_cmdLineArgs =
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
--    pandoc <- markdownToPandoc False intext
--    pandocToContentHtml False pandoc
--
--test_pandoc_11_B_E, test_pandoc_12_B_E :: IO ()
--test_pandoc_11_B_E = test1FileIO progName   "resultB1" "resultBE1"  markdownToHTML4xdebug
--test_pandoc_12_B_E = test1FileIO progName   "resultB2" "resultBE2" markdownToHTML4xdebug


htf_Lib_CmdLineArgs_test_thisModulesTests :: TestSuite
htf_Lib_CmdLineArgs_test_thisModulesTests = makeTestSuite "Lib.CmdLineArgs_test" [

  ]

