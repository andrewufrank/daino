-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
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
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports #-}

module Lib.Templating_test where


import           Test.Framework
import           Uniform.Test.TestHarness
import           Lib.Foundation                 ( progName
                                                , SiteLayout(..)
                                                , templatesDirName
                                                , masterTemplateFileName
                                                )
import           Lib.Foundation_test            ( testLayout )
import           Lib.Pandoc
import Uniform.Pandoc 

import           Lib.Templating -- (applyTemplate2, convGmaster)

templateDir = addDir (themeDir testLayout) templatesDirName
--page3  = addDir templateDir (makeRelFile "page3")
master3 = addDir templateDir masterTemplateFileName
--page33 = addDir templateDir(makeRelFile "page33")
--
-- applyTemplate3x :: DocValue -> ErrIO Text
-- applyTemplate3x dval = do
--      template <- read8 master3 dtmplFileType
--      ht       <- applyTemplate3 template dval
--      return . unHTMLout $ ht
-- ----                        (makeRelFile "pandocDefault.html"::Path Rel File)
----
----
-- test_teplating_16_AF_EF = test1FileIO progName     "resultAF6" "resultEF6" applyTemplate3x

-- test_templating_11_AG_EG, test_templating_12_AG_EG :: IO ()
-- test_templating_11_AG_EG =
--      test1FileIO progName "resultAG1" "resultEG1" applyTemplate3x
-- test_templating_12_AG_EG =
--      test1FileIO progName "resultAG2" "resultEG2" applyTemplate3x
-- test_templating_13_AG_EG =
--      test1FileIO progName "resultAG3" "resultEG3" applyTemplate3x
-- test_templating_14_AG_EG =
--      test1FileIO progName "resultAG4" "resultEG4" applyTemplate3x
-- test_templating_15_AG_EG =
--      test1FileIO progName "resultAG5" "resultEG5" applyTemplate3x
-- test_templating_16_AG_EG =
--      test1FileIO progName "resultAG6" "resultEG6" applyTemplate3x

-- test_readAF6 = do 
--           res <-  runErr $ do 
--                     st <- readFile2 (makeAbsFile "/home/frank/.SSG/resultAG6.x")
--                     let st2 = readNote "readAF6test" st :: DocValue
--                     return st2
--           assertEqual (Left text0) res


instance  ShowTestHarness DocValue where
instance ShowTestHarness HTMLout
--
--
--fromRightNoteString ::   Text -> Either String b -> b
--fromRightNoteString msg (Left a) = errorT ["fromRight", showT a, msg]
--fromRightNoteString _ (Right a) = a
