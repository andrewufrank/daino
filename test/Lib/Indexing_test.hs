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
import Lib.Foundation (layoutDefaults, doughDir)
import Uniform.Time (readDate3, UTCTime (..))

blogDir = (doughDir layoutDefaults) </> makeRelDir "Blog"
blogindexfn = (doughDir layoutDefaults) </> makeRelFile "Blog/index.md"

test_1 = do
            res <- runErr $ makeIndexForDir False
                    blogDir blogindexfn (doughDir testLayout) (Just "title")
            assertEqual res2  res

res2 =
--    Right
--      (MenuEntry{menu2 = []})

    Right
     (MenuEntry{menu2 =
      [IndexEntry{text =
                    "\"/home/frank/Workspace8/ssg/docs/site/dough/Blog/SubBlog/\"",
                  link = "SubBlog/index.html", title = "SubBlog (subdirectory)",
                  abstract = "", author = "", date = "", publish = ""},
       IndexEntry{text = "", link = "", title = "------", abstract = "",
                  author = "", date = "", publish = ""},
       IndexEntry{text = "postTufteStyled",
                  link = "/Blog/postTufteStyled.html", title = "postTufteStyle.md",
                  abstract = "A silly text not needing an abstract updated.",
                  author = "auf", date = "2019-01-04 00:00:00 UTC",
                  publish = "publish"},
       IndexEntry{text = "postwk", link = "/Blog/postwk.html",
                  title = "postwk.md",
                  abstract = "A silly text not needing an abstract.", author = "AUF",
                  date = "2019-01-04 00:00:00 UTC", publish = "publish"},
       IndexEntry{text = "postwk9", link = "/Blog/postwk9.html",
                  title = "postwk9.md",
                  abstract = "A silly text not needing an abstract.", author = "AUF",
                  date = "2019-01-04 00:00:00 UTC", publish = "publish"},
       IndexEntry{text = "postwkTufte", link = "/Blog/postwkTufte.html",
                  title = "postwkTufte.md",
                  abstract = "A silly text not needing an abstract updated.",
                  author = "auf", date = "2019-01-04 00:00:00 UTC",
                  publish = "publish"}]})

index1 = IndexEntry{
    text = "/home/frank/Workspace8/ssg/docs/site/dough/Blog/SubBlog/"
    , link = "SubBlog/index.html"
    , title = "SubBlog (subdirectory)"
    , abstract = ""
    , author = ""
    , date = "2019-01-03 00:00:00 UTC"
    , publish = ""
    }

instance IsString UTCTime where
    fromString = readNote "IsString UTCTime"

test_3 = assertEqual r3 (index1)

r3 = IndexEntry{text =
             "/home/frank/Workspace8/ssg/docs/site/dough/Blog/SubBlog/",
           link = "SubBlog/index.html", title = "SubBlog (subdirectory)",
           abstract = "", author = "", date = "2019-01-03 00:00:00 UTC",
           publish = ""}
