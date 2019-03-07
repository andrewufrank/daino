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
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans #-}

module Lib.Pandoc_test  -- (openMain, htf_thisModuelsTests)
     where

import           Test.Framework
import Uniform.Test.TestHarness
import Lib.Foundation (progName)
import Lib.FileMgt
import Lib.YamlBlocks (readMd2meta)
--import Lib.Pandoc
--import Text.Pandoc.Definition as PD

--readMarkdownFile8 :: String  -> ErrIO MarkdownText
--readMarkdownFile8 fnn = fmap fst $ readMd2meta (makeAbsFile fnn)
---- uses files to be copied to dough
----
--test_pandoc_pageFn_pageMd_1, test_pandoc_pageFn_pageMd_2 :: IO ()
--test_pandoc_pageFn_pageMd_1 = test1FileIO progName  "pageFn1" "pageMd1" readMarkdownFile8
--test_pandoc_pageFn_pageMd_2 = test1FileIO progName  "pageFn2" "pageMd2" readMarkdownFile8

--
--test_pandoc_11_A_D, test_pandoc_12_A_D :: IO ()
--test_pandoc_11_A_D = test1FileIO progName  "resultAG1" "resultAD1" (markdownToPandoc False)
--test_pandoc_12_A_D = test1FileIO progName  "resultAG2" "resultAD2" (markdownToPandoc False)
--
--test_pandoc_11_A_F, test_pandoc_12_A_F :: IO ()
--test_pandoc_11_A_F = test1FileIO progName  "resultAD1" "resultAF1" (pandocToContentHtml False)
--test_pandoc_12_A_F = test1FileIO progName  "resultAD2" "resultAF2" (pandocToContentHtml False)
--
--instance Zeros Pandoc where zero = Pandoc mempty zero
----instance Zeros PD.Meta where zero = PD.Meta []
--
--markdownToHTML4xdebug ::  MarkdownText -> ErrIO DocValue
--
--markdownToHTML4xdebug intext = do
--    pandoc <- markdownToPandoc False intext
--    pandocToContentHtml False pandoc
--
--test_pandoc_11_B_E, test_pandoc_12_B_E :: IO ()
--test_pandoc_11_B_E = test1FileIO progName   "resultB1" "resultBE1"  markdownToHTML4xdebug
--test_pandoc_12_B_E = test1FileIO progName   "resultB2" "resultBE2" markdownToHTML4xdebug


instance  ShowTestHarness MarkdownText where

instance  ShowTestHarness DocValue where
--instance  ShowTestHarness Pandoc where
