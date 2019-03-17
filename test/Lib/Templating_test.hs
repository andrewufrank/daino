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

--import Uniform.Strings
import           Lib.Templating -- (applyTemplate2, convGmaster)
--import Uniform.Filenames
-- import           Lib.FileMgt
--import qualified Text.Glabrous as G
--import Text.Glabrous (Template, insert) -- , insertMany)
--import Text.DocTemplates

--putInT :: Pandoc -> Gtemplate -> Dtemplate
--putInT ht tpl = do

--test_templating_put1 = test2FileIO progName "resultAF1"

--temp1 = "some start {{template2}} and some more text."
--temp2 = "xxx and yyy"

--t1 = fromRightNoteString "t1 rwe2" . G.fromText $ temp1
--t2 = fromRightNoteString "t2 wwer2" . G.fromText $ temp2
--
--c1 = G.fromList [("template2",temp2)] :: G.Context
--
--res1 = G.process  t1 c1  :: Text


--test_convMaster3 = do
--           res <- runErrorVoid $ putPageInMaster
--                     page3 master3 "body" page33
--           assertEqual () res

templateDir = addDir (themeDir testLayout) templatesDirName
--page3  = addDir templateDir (makeRelFile "page3")
master3 = addDir templateDir masterTemplateFileName
--page33 = addDir templateDir(makeRelFile "page33")
--
applyTemplate3x :: DocValue -> ErrIO Text
applyTemplate3x dval = do
     template <- read8 master3 dtmplFileType
     ht       <- applyTemplate3 template dval
     return . unHTMLout $ ht
----                        (makeRelFile "pandocDefault.html"::Path Rel File)
----
----
-- test_templating_11_AF_EF, test_templating_12_AF_EF :: IO ()
-- test_templating_11_AF_EF = test1FileIO progName   "resultAF1" "resultEF1"  applyTemplate3x
-- test_templating_12_AF_EF = test1FileIO progName     "resultAF2" "resultEF2" applyTemplate3x
-- test_templating_13_AF_EF = test1FileIO progName     "resultAF3" "resultEF3" applyTemplate3x
-- test_templating_14_AF_EF = test1FileIO progName     "resultAF4" "resultEF4" applyTemplate3x
-- test_templating_15_AF_EF = test1FileIO progName     "resultAF5" "resultEF5" applyTemplate3x
-- test_templating_16_AF_EF = test1FileIO progName     "resultAF6" "resultEF6" applyTemplate3x

test_templating_11_AG_EG, test_templating_12_AG_EG :: IO ()
test_templating_11_AG_EG =
     test1FileIO progName "resultAG1" "resultEG1" applyTemplate3x
test_templating_12_AG_EG =
     test1FileIO progName "resultAG2" "resultEG2" applyTemplate3x
test_templating_13_AG_EG =
     test1FileIO progName "resultAG3" "resultEG3" applyTemplate3x
test_templating_14_AG_EG =
     test1FileIO progName "resultAG4" "resultEG4" applyTemplate3x
test_templating_15_AG_EG =
     test1FileIO progName "resultAG5" "resultEG5" applyTemplate3x
test_templating_16_AG_EG =
     test1FileIO progName "resultAF6" "resultEG6" applyTemplate3x

instance  ShowTestHarness DocValue where
instance ShowTestHarness HTMLout
--
--
--fromRightNoteString ::   Text -> Either String b -> b
--fromRightNoteString msg (Left a) = errorT ["fromRight", showT a, msg]
--fromRightNoteString _ (Right a) = a
