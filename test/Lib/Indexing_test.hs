-----------------------------------------------------------------------------
--
-- Module      :   indexing tests
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports #-}

module Lib.Indexing_test
     where


import           Test.Framework
import Uniform.Test.TestHarness
import Lib.Foundation (progName, SiteLayout(..), templatesDirName)
import Lib.Foundation_test (testLayout)

--import Uniform.Strings
import Lib.Templating -- (applyTemplate2, convGmaster)
--import Uniform.Filenames
import Lib.FileMgt
import Lib.Indexing
--import Text.DocTemplates

blogDir = makeAbsDir "/home/frank/Workspace8/ssg/site/dough/Blog"
blogindexfn = makeAbsFile "/home/frank/Workspace8/ssg/site/dough/Blog/index.md"

test_1 = do
            res <- runErr $ makeIndexForDir blogDir blogindexfn
            assertEqual res2  res

res2 =
    Right
      (MenuEntry{menu2 =
               [IndexEntry{text = "postwk", link = "/Blog/postwk.html"},
                IndexEntry{text = "index", link = "/Blog/index.html"},
                IndexEntry{text = "postwkTufte", link = "/Blog/postwkTufte.html"},
                IndexEntry{text = "postwk2", link = "/Blog/postwk2.html"},
                IndexEntry{text = "postTufteStyled",
                           link = "/Blog/postTufteStyled.html"}]})
