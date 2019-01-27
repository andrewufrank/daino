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

module Lib.Templating_test
     where


import           Test.Framework
import Uniform.Test.TestHarness
import Lib.Foundation (progName, templatesPath)
--import Uniform.Strings
import Lib.Templating -- (applyTemplate2, convGmaster)
--import Uniform.Filenames
import Lib.FileMgt
import qualified Text.Glabrous as G
--import Text.Glabrous (Template, insert) -- , insertMany)
--import Text.DocTemplates

temp1 = "some start {{template2}} and some more text."
temp2 = "xxx and yyy"

t1 = fromRightNoteString "t1 rwe2" . G.fromText $ temp1
t2 = fromRightNoteString "t2 wwer2" . G.fromText $ temp2

c1 = G.fromList [("template2",temp2)] :: G.Context

res1 = G.process  t1 c1  :: Text


test_convMaster3 = do
           res <- runErrorVoid $ putPageInMaster templatesPath
                     (makeRelFile "Page3") (makeRelFile "Master3")
                    "body" (makeRelFile "page33")
           assertEqual () res



applyTemplate2x :: DocValue -> ErrIO HTMLout
applyTemplate2x = applyTemplate2   templatesPath (makeRelFile "page33")
--                        (makeRelFile "pandocDefault.html"::Path Rel File)


test_templating_11_E_F, test_templating_12_E_F :: IO ()
test_templating_11_E_F = test1FileIO progName   "resultBE11" "resultEF11"  applyTemplate2x
test_templating_12_E_F = test1FileIO progName   "resultBE12" "resultEF12" applyTemplate2x

instance  ShowTestHarness DocValue where
instance ShowTestHarness HTMLout
--
--
fromRightNoteString ::   Text -> Either String b -> b
fromRightNoteString msg (Left a) = errorT ["fromRight", showT a, msg]
fromRightNoteString _ (Right a) = a
