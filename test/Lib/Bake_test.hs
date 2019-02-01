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


