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
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Lib.Pandoc_test  -- (openMain, htf_thisModuelsTests)
     where

import           Test.Framework
import Uniform.Test.TestHarness
import Lib.Foundation (progName, SiteLayout(..))
    -- , layoutDefaults, defaultPageType)
-- import Lib.FileMgt
--import Lib.YamlBlocks (readMd2meta)
import Lib.Pandoc
import Uniform.Pandoc
import Lib.Foundation_test (testLayout)
import Lib.CmdLineArgs (allFlags)
import Lib.CheckInput (TripleDoc, MetaRec(..)
    ,SortArgs(..), PublicationState(..))
import Uniform.Pointless (snd3)
--import Text.Pandoc.Definition as PD

-- readMarkdownFile8 :: Path Abs File  -> ErrIO MarkdownText
-- readMarkdownFile8 fnn =  read8   ( fnn) markdownFileType
-- ---- uses files to be copied to dough
-- ----
-- test_pandoc_pageFn_pageMd_1  :: IO ()
-- test_pandoc_pageFn_pageMd_1 = test1FileIO progName  "pageFn1" "pageMd1" readMarkdownFile8
-- test_pandoc_pageFn_pageMd_2 = test1FileIO progName  "pageFn2" "pageMd2" readMarkdownFile8
-- -- test_pandoc_pageFn_pageMd_3 = test1FileIO progName  "pageFn3" "pageMd3" readMarkdownFile8
-- -- test_pandoc_pageFn_pageMd_4 = test1FileIO progName  "pageFn4" "pageMd4" readMarkdownFile8
-- -- test_pandoc_pageFn_pageMd_5 = test1FileIO progName  "pageFn5" "pageMd5" readMarkdownFile8
-- -- test_pandoc_pageFn_pageMd_6 = test1FileIO progName  "pageFn6" "pageMd6" readMarkdownFile8

doughP = doughDir testLayout

markdownToPandoX ::  TripleDoc -> ErrIO ( Pandoc)
markdownToPandoX td = markdownToPandocBiblio True allFlags doughP   td -- (makeAbsFile s)

test_pandoc_11_A_D :: IO ()
test_pandoc_11_A_D = test1FileIO progName   "TripleDoc1" "resultAD1" markdownToPandoX
test_pandoc_12_A_D = test1FileIO progName  "TripleDoc2" "resultAD2" markdownToPandoX
-- test_pandoc_13_A_D = test1FileIO progName  "TripleDoc3" "resultAD3" markdownToPandoX
-- test_pandoc_14_A_D = test1FileIO progName  "TripleDoc4" "resultAD4" markdownToPandoX
-- test_pandoc_15_A_D = test1FileIO progName  "TripleDoc5" "resultAD5" markdownToPandoX
-- test_pandoc_16_A_D = test1FileIO progName  "TripleDoc6" "resultAD6" markdownToPandoX
            -- 13 fails
-- "/home/frank/Workspace8/ssg/site/dough/site/dough/resources/BibTexLatex.bib: openFile: does not exist (No such file or directory)"

-- pandocToContentHtmlX ::  Pandoc -> ErrIO HTMLout
-- pandocToContentHtmlX mp = pandocToContentHtml False mp -- (fromJustNote "fwerw" mp)
-- --
-- test_pandoc_11_A_F  :: IO ()
-- test_pandoc_11_A_F = test1FileIO progName  "resultAD1" "resultAF1" pandocToContentHtmlX
-- test_pandoc_12_A_F = test1FileIO progName  "resultAD2" "resultAF2" pandocToContentHtmlX
-- -- test_pandoc_13_A_F = test1FileIO progName  "resultAD3" "resultAF3" pandocToContentHtmlX
-- -- test_pandoc_14_A_F = test1FileIO progName  "resultAD4" "resultAF4" pandocToContentHtmlX
-- -- test_pandoc_15_A_F = test1FileIO progName  "resultAD5" "resultAF5" pandocToContentHtmlX
-- -- test_pandoc_16_A_F = test1FileIO progName  "resultAD6" "resultAF6" pandocToContentHtmlX
-- --
-- -- docVal2 :: HTMLout -> Path Abs File -> TripleDoc -> ErrIO DocValue
-- -- docVal2 htmlout pagefn triple = docValToAllVal False testLayout allFlags 
-- --                               htmlout  
-- --                  (snd3 triple)

-- test_pandoc_11_F_G  :: IO ()
-- test_pandoc_11_F_G = test3FileIO progName  "resultAF1" "pageFn1" "TripleDoc1" "resultAG1" docVal2
-- test_pandoc_12_F_G = test3FileIO progName  "resultAF2" "pageFn2" "TripleDoc2" "resultAG2" docVal2
-- -- test_pandoc_13_F_G = test3FileIO progName  "resultAF3" "pageFn3" "TripleDoc3" "resultAG3" docVal2
-- -- test_pandoc_14_F_G = test3FileIO progName  "resultAF4" "pageFn4" "TripleDoc4" "resultAG4" docVal2
-- -- test_pandoc_15_F_G = test3FileIO progName  "resultAF5" "pageFn5" "TripleDoc5" "resultAG5" docVal2
-- -- test_pandoc_16_F_G = test3FileIO progName  "resultAF6" "pageFn6" "TripleDoc6" "resultAG6" docVal2


-- markdownToHTML5xdebug ::  MarkdownText -> ErrIO DocValue

-- markdownToHTML5xdebug intext = do
--    pandoc <- markdownToPandoc False intext
--    pandocToContentHtml False pandoc

-- test_pandoc_11_B_E, test_pandoc_12_B_E :: IO ()
-- test_pandoc_11_B_E = test1FileIO progName   "resultB1" "resultBE1"  markdownToHTML4xdebug
-- test_pandoc_12_B_E = test1FileIO progName   "resultB2" "resultBE2" markdownToHTML4xdebug

-- test_defaultPageType =   assertEqual defPT 
--         $ pt
--     where 
--         mpageType ::  (Path Abs File)
--         mpageType =  makeAbsFile $ pageTemplate metaRecIndex1
--         pt :: Path Abs File 
--         pt = fromMaybe (defaultPageType layoutDefaults)  mpageType


-- defPT = (makeAbsFile "/home/frank/Workspace8/ssg/theme/templates/page3.yaml") :: Path Abs File 

    -- example copied from indexing_test
metaRecIndex1 = MetaRec
  {fn = "/home/frank/Workspace8/ssg/docs/site/dough/Blog/index.md"
  , relURL = "/Blog/index.md"
  , title = "primary index for Blog"
  , abstract = "The directory for experiments.", author = "AUF",
   date = "2019-01-04 00:00:00 UTC", 
   publicationState = PSpublish,
   bibliography = Nothing, 
   bibliographyGroup = Nothing,
    keywords = Just "test",
    pageTemplate = -- Nothing,
         "/home/frank/Workspace8/ssg/theme/templates/page3.yaml",
    indexPage = True, indexSort = SAreverseDate}

-- instance  ShowTestHarness MarkdownText where

-- instance  ShowTestHarness DocValue where
instance  ShowTestHarness ( Pandoc) where
instance  ShowTestHarness TripleDoc where
instance  ShowTestHarness HTMLout where
instance  ShowTestHarness (Path Abs File) where

