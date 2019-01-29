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

module ShakeStartTests
     where


import           Test.Framework
--import Uniform.Test.TestHarness          hiding ((<.>), (</>))
import Uniform.Filenames            hiding ((<.>), (</>))
import Development.Shake
--import Development.Shake.Command
import Development.Shake.FilePath
--import Development.Shake.Util

import Lib.Foundation (progName, SiteLayout (..), layoutDefaults)
import Lib.Bake
import Lib.FileMgt
import Lib.Foundation_test (testLayout)
import Lib.Foundation (templatesDirName)
import Lib.Templating (Gtemplate(..), gtmplFileType, Dtemplate(..))

test_shake =  do
                startTesting layoutDefaults
                return ()

-- bake errors are reported

startTesting :: SiteLayout -> IO ()
-- start the testing by executing the tests and building teh
-- intermediate results
startTesting layout = shakeArgs shakeOptions {shakeFiles="/home/frank/.SSG"
                , shakeVerbosity=Chatty -- Loud
                , shakeLint=Just LintBasic
--                , shakeRebuild=[(RebuildNow,"allMarkdownConversion")]
--                  seems not to produce an effect
                } $
    do
        let
              doughD      =   (toFilePath . doughDir $ layout)
              templatesD =   ((toFilePath . themeDir $ layout) </> (toFilePath templatesDirName))
              testD = "/home/frank/.SSG" :: FilePath

        want ["allTests"]
        phony "allTests" $
            do
                need [testD</>"Master3.gtpl", testD</>"master.yaml"]

        (testD</>"Master3.gtpl") %> \out ->
            copyFileChanged  (replaceDirectory out templatesD) out

        (testD</>"master.yaml") %> \out ->
            copyFileChanged  (replaceDirectory out templatesD) out


