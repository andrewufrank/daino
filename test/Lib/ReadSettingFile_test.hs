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

module Lib.ReadSettingFile_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import Uniform.Test.TestHarness

import Lib.Foundation (progName, SiteLayout (..), layoutDefaults)
import Lib.ReadSettingFile
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

test_currentDir = do
        res <- runErr $ do
                cd <- currentDir
                putIOwords ["currentDir test", showT cd]
                return . toFilePath $ cd
        assertEqual (Right ("/home/frank/Workspace8/ssg/site/dough/")) res

test_readSettings2 =
    do
        res <-  runErr $ do
                wd <- return $  makeAbsDir "/home/frank/Workspace8/ssg/site/dough/"
                settingsTxt <- read8 (wd </> makeRelFile "settings2") yamlFileType

                layout <- readSettings2 True settingsTxt
                return . showT $ layout

        assertEqual (Right ( showT (layoutDefaults, 3000):: Text))  res



