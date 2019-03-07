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
            res <- runErr $ makeIndexForDir False blogDir blogindexfn
            assertEqual res2  res

res2 =
    Right
--      (MenuEntry{menu2 = []})

  (MenuEntry{menu2 =
               [IndexEntry{text = "postwkTufte", link = "/Blog/postwkTufte.html",
                           title = "postwkTufte.md",
                           abstract = "A silly text not needing an abstract updated.",
                           author = "auf", date = "Jan. 4, 2019"},
                IndexEntry{text = "postwk9", link = "/Blog/postwk9.html",
                           title = "postwk.md",
                           abstract = "A silly text not needing an abstract.", author = "AUF",
                           date = "Jan. 4, 2019"},
                IndexEntry{text = "postTufteStyled",
                           link = "/Blog/postTufteStyled.html", title = "postwkTufte.md",
                           abstract = "A silly text not needing an abstract updated.",
                           author = "auf", date = "Jan. 4, 2019"},
                IndexEntry{text = "postwk", link = "/Blog/postwk.html",
                           title = "postwk.md",
                           abstract = "A silly text not needing an abstract.", author = "AUF",
                           date = "Jan. 4, 2019"}]})
