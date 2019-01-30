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

import Lib.Foundation (progName, SiteLayout (..), layoutDefaults)
import Lib.Bake
import Lib.FileMgt
import Lib.Foundation_test (testLayout)
import Lib.Foundation (templatesDirName)
--import Lib.Templating (Gtemplate(..), gtmplFileType, Dtemplate(..))
import Control.Lens
--import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson
--import Data.Aeson.Encode.Pretty (encodePretty)
--import Data.ByteString.Lazy as BS (putStrLn)

test_findTemplate =
    do
        res <-  runErr $ do
                    let source = "/home/frank/.SSG/landingPage.content.docval"
                    val :: DocValue <- read8 (makeAbsFile source)  docValueFileType
                    let val2 = unDocValue val  ::Value

                    putIOwords ["test_findTemplate", "val2\n" ] -- ,  shownice $ val2]
--                    liftIO $ BS.putStrLn ( encodePretty$ val2)
                    let ptemplate = (val2) ^? key "pageTemplate" . _String
--                                :: Maybe FilePath
                    putIOwords ["test_findTemplate", "found", showT ptemplate]
                    return   ptemplate

        assertEqual (Right (Just "Page3"::Maybe Text))  res

--test_bake_MasterYaml =
--        testVar0FileIO progName
--                    (addFileName (doughDir testLayout) (makeRelFile "master.yaml"):: Path Abs File)
--                        "resultMasterYaml"
--                        (\fn -> do
--                                   preface :: YamlText <- read8 fn yamlFileType
--                                   return . showT $ preface
--                               )
--
--test_bake_MasterTmpl =
--        testVar0FileIO progName
--                    (addFileName (addDir (themeDir testLayout) templatesDirName)
--                                (makeRelFile "Master3.gtpl"):: Path Abs File)
--                        "resultMasterTemplate"
--                        (\fn -> do
--                                   templ :: Gtemplate <- read8 fn gtmplFileType
--                                   return . showT $ templ
--                               )
--
--
--bakeOneFileCoreT ::   YamlText -> MarkdownText  -> Gtemplate ->  ErrIO Text
--bakeOneFileCoreT preface md gtpl = do
--                    res <- bakeOneFileCore False preface ( md) gtpl
--                    return . unHTMLout $ res
--
--test_bakeCore1_K_R = test3FileIO progName "resultMasterYaml"  "pageMd1" "resultMasterTemplate"
--                                        "resultR1" bakeOneFileCoreT
--test_bakeCore2 = test3FileIO progName "resultMasterYaml"  "pageMd2" "resultMasterTemplate"
--                                        "resultR2" bakeOneFileCoreT
--
----test_bake_11_A_H, test_bake_12_A_H :: IO ()
----test_bake_11_A_H = test2FileIO progName  "resultAF1" "resultMasterTemplate" "resultAH1" spliceTemplates
----test_bake_12_A_H = test2FileIO progName  "resultAF2" "resultMasterTemplate" "resultAH2" spliceTemplates
----
----
----spliceMarkdownT :: YamlText -> MarkdownText -> MarkdownText
----spliceMarkdownT y m =   spliceMarkdown (  y) m
----test_Splice1 = test2File progName "resultMasterYaml" "resultAK1" "resultAG1" spliceMarkdownT
----test_Splice2 = test2File progName "resultMasterYaml" "resultAK2" "resultAG2" spliceMarkdownT
--
----test11, test12 ::  Path Rel File
----test11 = makeRelFile "Blog/postwk.md"
----test12 = makeRelFile "PublicationList/postWithReference.md"
--
----test_bake
----bakeOneFileDebug md = bakeOneFile False md templateFile
----    where fn =
----
----test_bake_11_A_M, test_bake_12_A_M :: IO ()
----test_bake_11_A_M = testVar0FileIO progName  test11 "resultAM11" bakeOneFileDebug
----test_bake_12_A_M = testVar0FileIO progName  test12 "resultAM12" bakeOneFileDebug
----
----test_bake_11_A_L, test_bake_12_A_L :: IO ()
----test_bake_11_A_L = testVar0File progName  test11 "resultAL11" showT
----test_bake_12_A_L = testVar0File progName  test12 "resultAL12" showT
----
--
--
------ just testing read/write
----compareRead fn t = do
----                t2 <- readMarkdownFile fn
----                let v = t==t2
----                let res = if v then "ok" else (unwords' ["read t \n", showT t, "\nreread t2\n", showT t2])
----                return res
--
----test_bake_11_A_X :: IO ()
----test_bake_11_A_X = testVar1FileIO progName  test11 "resultAK11" "resultAX11"  dscompareRead
--
--instance  ShowTestHarness MarkdownText
--instance  ShowTestHarness (Path Abs File)
--instance  ShowTestHarness YamlText
--instance  ShowTestHarness Gtemplate
--instance  ShowTestHarness HTMLout
--instance  ShowTestHarness DocValue
--instance  ShowTestHarness Dtemplate
--
--
--fromRightNote = fromRight
