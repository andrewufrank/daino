-----------------------------------------------------------------------------
--
-- Module      :   a for the read writes typed files
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
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans #-}

module Lib.YamlBlocks_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
--import Uniform.Strings hiding ((</>))
--import Lib.Bake
import Uniform.Filenames
import Uniform.TypedFile
import Lib.Foundation   (SiteLayout (..), sourceDir)
import Lib.FileMgt
import Lib.YamlBlocks
import Lib.FileMgt_test (testFileReadWrite, testD)

testYAML = sourceDir </> makeRelFile "site/dough/settings2.yaml"
testYAML, testYAML2 :: Path Abs File 
testYAML2 = testD </> makeRelFile "site/dough/settings2.yaml"


test_YAMLue = testFileReadWrite testYAML testYAML2 yamlFileType


