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
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports#-}

module Lib.ReadSettingFile_test  -- (openMain, htf_thisModuelsTests)
                                where


-- import Control.Lens
-- import Data.Aeson

-- import Data.Aeson.Lens
-- import Lib.FileMgt
import Lib.Foundation                 ( templatesDirName
                                , progName, SiteLayout(..), templatesDirName)
import Lib.Foundation_test            ( testLayout )
--import Lib.Templating (Gtemplate(..), gtmplFileType, Dtemplate(..))
import Lib.ReadSettingFile
--import Data.Aeson
import Test.Framework
import Uniform.Test.TestHarness
--import Data.Aeson.Encode.Pretty (encodePretty)
--import Data.ByteString.Lazy as BS (putStrLn)
-- import Lib.YamlBlocks
import Uniform.Yaml 

test_currentDir = do
        res <- runErr $ do
                cd <- currentDir
                putIOwords ["currentDir test", showT cd]
                return . toFilePath $ cd
        assertEqual (Right "/home/frank/Workspace11/ssg/") res
--        assertEqual (Right ("/home/frank/Workspace11/ssg/site/dough/")) res
        -- is ok if test is run locally (in site/dough)

test_readSettings2 = do
        res <- runErr $ do
                let wd = makeAbsDir "/home/frank/Workspace11/ssg/site/dough/"
                settingsTxt <- read8 (wd </> makeRelFile "settings2")
                                     yamlFileType

                layout <- readSettings2 False settingsTxt
                return . showT $ layout

        assertEqual (Right (showT (testLayout, 3000) :: Text)) res



