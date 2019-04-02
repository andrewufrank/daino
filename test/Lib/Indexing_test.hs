-----------------------------------------------------------------------------
--
-- Module      :   indexing tests
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports #-}

module Lib.Indexing_test where

-- import Lib.FileMgt
import           Lib.Foundation (layoutDefaults, doughDir)
import           Lib.Foundation (progName, SiteLayout(..), templatesDirName)
import           Lib.Foundation_test (testLayout)
--import Uniform.Strings
import           Lib.Indexing -- (applyTemplate2, convGmaster)
--import Uniform.Filenames
import           Lib.Templating
import           Test.Framework
--import Text.DocTemplates
import           Uniform.Test.TestHarness
import           Uniform.Time (readDate3, UTCTime(..))
import           Lib.CmdLineArgs (allFlags)
import           Lib.CheckInput (MetaRec(..), SortArgs(..))

blogDir = doughDir testLayout </> makeRelDir "Blog"

blogindexfn = doughDir testLayout </> makeRelFile "Blog/index.md"

test_parentDir = assertEqual res1a (getParentDir blogindexfn)

test_immediateParent = assertEqual res2a (getImmediateParentDir blogindexfn)

res1a = "/home/frank/Workspace8/ssg/docs/site/dough/Blog" :: FilePath

res2a = "Blog" :: FilePath

linkIn = doughDir testLayout </> makeRelFile  "Blog/postwk.md" :: Path Abs File
test_relLink = do 
        res <- runErr $ makeRelLink (doughDir testLayout) linkIn
        assertEqual (Right resLink)  res 
resLink = "/Blog/postwk.html" :: Text

metaRec1 = MetaRec
  { title = Just "index for post"
  , abstract = Just "The directory for experiments."
  , author = Just "AUF"
  , date = Just "2019-01-04 00:00:00 UTC"
  , publicationState = Nothing
  , bibliography = Nothing
  , bibliographyGroup = Nothing
  , keywords = Just "test"
  , pageTemplate = Just "page3.yaml"
  , indexPage = Just True
  , indexSort = SAtitle
  }

test_makeIndexForDir_1 = do
  res <- runErr
    $ makeIndexForDir
      True
      allFlags
      metaRec1
      (doughDir testLayout)
      blogindexfn
  -- 
  assertEqual res2 res

res2 = Right
(MenuEntry{menu2 =
             [IndexEntry{text2 =
                           "Path Abs Dir /home/frank/Workspace8/ssg/docs/site/dough/Blog/SubBlog/",
                         link2 = "SubBlog/html", title2 = "SubBlog (subdirectory)",
                         abstract2 = "", author2 = "", date2 = "", publish2 = ""},
              IndexEntry{text2 = "", link2 = "", title2 = "------",
                         abstract2 = "", author2 = "", date2 = "", publish2 = ""},
              IndexEntry{text2 = "postwk9", link2 = "/Blog/postwk9.html",
                         title2 = "index for post",
                         abstract2 = "The directory for experiments.", author2 = "AUF",
                         date2 = "2019-01-04 00:00:00 UTC", publish2 = "Nothing"},
              IndexEntry{text2 = "postTufteStyled",
                         link2 = "/Blog/postTufteStyled.html", title2 = "index for post",
                         abstract2 = "The directory for experiments.", author2 = "AUF",
                         date2 = "2019-01-04 00:00:00 UTC", publish2 = "Nothing"},
              IndexEntry{text2 = "postwk", link2 = "/Blog/postwk.html",
                         title2 = "index for post",
                         abstract2 = "The directory for experiments.", author2 = "AUF",
                         date2 = "2019-01-04 00:00:00 UTC", publish2 = "Nothing"},
              IndexEntry{text2 = "postwkTufte", link2 = "/Blog/postwkTufte.html",
                         title2 = "index for post",
                         abstract2 = "The directory for experiments.", author2 = "AUF",
                         date2 = "2019-01-04 00:00:00 UTC", publish2 = "Nothing"}]})
                         
  -- Right
  -- (MenuEntry
  --  { menu2 =
  --      [ IndexEntry
  --          { text2 = "Path Abs Dir /home/frank/Workspace8/ssg/docs/site/dough/Blog/SubBlog/"
  --          , link2 = "SubBlog/html"
  --          , title2 = "SubBlog (subdirectory)"
  --          , abstract2 = ""
  --          , author2 = ""
  --          , date2 = ""
  --          , publish2 = ""
  --          }
  --      , IndexEntry { text2 = ""
  --                   , link2 = ""
  --                   , title2 = "------"
  --                   , abstract2 = ""
  --                   , author2 = ""
  --                   , date2 = ""
  --                   , publish2 = ""
  --                   }
  --      , IndexEntry
  --          { text2 = "postTufteStyled"
  --          , link2 = "/Blog/postTufteStyled.html"
  --          , title2 = "postTufteStyle.md"
  --          , abstract2 = "A silly text not needing an abstract updated."
  --          , author2 = "auf"
  --          , date2 = "2019-01-04 00:00:00 UTC"
  --          , publish2 = "Nothing"
  --          }
  --      , IndexEntry { text2 = "postwk"
  --                   , link2 = "/Blog/postwk.html"
  --                   , title2 = "postwk.md"
  --                   , abstract2 = "A silly text not needing an abstract."
  --                   , author2 = "AUF"
  --                   , date2 = "2019-01-04 00:00:00 UTC"
  --                   , publish2 = "Nothing"
  --                   }
  --      , IndexEntry { text2 = "postwk9"
  --                   , link2 = "/Blog/postwk9.html"
  --                   , title2 = "postwk9.md"
  --                   , abstract2 = "A silly text not needing an abstract."
  --                   , author2 = "AUF"
  --                   , date2 = "2019-01-04 00:00:00 UTC"
  --                   , publish2 = "draft"
  --                   }
  --      , IndexEntry
  --          { text2 = "postwkTufte"
  --          , link2 = "/Blog/postwkTufte.html"
  --          , title2 = "postwkTufte.md"
  --          , abstract2 = "A silly text not needing an abstract updated."
  --          , author2 = "auf"
  --          , date2 = "2019-01-04 00:00:00 UTC"
  --          , publish2 = "Nothing"
  --          }]
  --  })
  -- Right
  -- (MenuEntry
  --  { menu2 =
  --      [ IndexEntry
  --          { text2 = "Path Abs Dir /home/frank/Workspace8/ssg/docs/site/dough/Blog/SubBlog/"
  --          , link2 = "SubBlog/html"
  --          , title2 = "SubBlog (subdirectory)"
  --          , abstract2 = ""
  --          , author2 = ""
  --          , date2 = ""
  --          , publish2 = ""
  --          }
  --      , IndexEntry { text2 = ""
  --                   , link2 = ""
  --                   , title2 = "------"
  --                   , abstract2 = ""
  --                   , author2 = ""
  --                   , date2 = ""
  --                   , publish2 = ""
  --                   }
  --      , IndexEntry
  --          { text2 = "postwk9"
  --          , link2 =
  --              "//home/frank/Workspace8/ssg/docs/site/dough/Blog/postwk9.html"
  --          , title2 = "index for post"
  --          , abstract2 = "The directory for experiments."
  --          , author2 = "AUF"
  --          , date2 = "2019-01-04 00:00:00 UTC"
  --          , publish2 = "Nothing"
  --          }
  --      , IndexEntry
  --          { text2 = "postTufteStyled"
  --          , link2 = "//home/frank/Workspace8/ssg/docs/site/dough/Blog/postTufteStyled.html"
  --          , title2 = "index for post"
  --          , abstract2 = "The directory for experiments."
  --          , author2 = "AUF"
  --          , date2 = "2019-01-04 00:00:00 UTC"
  --          , publish2 = "Nothing"
  --          }
  --      , IndexEntry
  --          { text2 = "postwk"
  --          , link2 =
  --              "//home/frank/Workspace8/ssg/docs/site/dough/Blog/postwk.html"
  --          , title2 = "index for post"
  --          , abstract2 = "The directory for experiments."
  --          , author2 = "AUF"
  --          , date2 = "2019-01-04 00:00:00 UTC"
  --          , publish2 = "Nothing"
  --          }
  --      , IndexEntry
  --          { text2 = "postwkTufte"
  --          , link2 = "//home/frank/Workspace8/ssg/docs/site/dough/Blog/postwkTufte.html"
  --          , title2 = "index for post"
  --          , abstract2 = "The directory for experiments."
  --          , author2 = "AUF"
  --          , date2 = "2019-01-04 00:00:00 UTC"
  --          , publish2 = "Nothing"
  --          }]
  --  })

instance IsString UTCTime where
  fromString = readNote "IsString UTCTime"
  -- test_3 = assertEqual r3 (index1)
  -- r3 = IndexEntry{text =
  --              "/home/frank/Workspace8/ssg/docs/site/dough/Blog/SubBlog/",
  --            link = "SubBlog/index.html", title = "SubBlog (subdirectory)",
  --            abstract = "", author = "", date = "2019-01-03 00:00:00 UTC",
  --            publish = ""}
