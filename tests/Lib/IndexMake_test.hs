-----------------------------------------------------------------------------
--
-- Module      :   index making tests
--      converts a set of dir and file nams to an index 
--      only files to be included in the index are included in inputs 
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports 
      -fno-warn-missing-fields #-}

module Lib.IndexMake_test where

-- import Lib.FileMgt
import           Lib.Foundation (layoutDefaults, doughDir
                                  , progName, SiteLayout(..), templatesDirName)
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
import           Lib.CheckInput (MetaRec(..), SortArgs(..), PublicationState(..)
                               , makeRelPath)
import           Lib.IndexMake

blogDir = doughDir testLayout </> makeRelDir "Blog"

dough2 = doughDir testLayout

blogindexfn = doughDir testLayout </> makeRelFile "Blog/index.md"

test_parentDir = assertEqual res1a (getParentDir blogindexfn)

test_immediateParent = assertEqual res2a (getImmediateParentDir blogindexfn)

res1a = "/home/frank/Workspace8/ssg/docs/site/dough/Blog" :: FilePath

res2a = "Blog" :: FilePath

linkIn = doughDir testLayout </> makeRelFile "Blog/postwk.md" :: Path Abs File

indexIn = doughDir testLayout </> makeRelFile "Blog/index.md" :: Path Abs File

test_relLink = assertEqual (resLink)
  $ setExtension "html" . removeExtension
  $ makeRelPath dough2 linkIn

-- (doughDir testLayout :: Path Abs Dir) linkIn
resLink = "/Blog/postwk.html" :: FilePath

metaRec1t = MetaRec
  { fn = toFilePath
      (doughDir testLayout </> makeRelFile "Blog/index.md" :: Path Abs File)
  , relURL = "/Blog/index.md"
  , title = "index for post"
  , abstract = "The directory for experiments."
  , author = "AUF"
  , date = "2066-06-06 00:00:00 UTC"
  , publicationState = PSpublish
  , bibliography = Nothing
  , bibliographyGroup = Nothing
  , keywords = Just "test"
  , pageTemplate = Just "page3.yaml"
  , indexPage = True
  , indexSort = SAtitle
  }

metaRec95 = MetaRec
  { fn = toFilePath linkIn
  , relURL = "/Blog/postwk.html"
  , title = "index for post"
  , abstract = "The directory for experiments."
  , author = "AUF"
  , date = "2066-06-06 00:00:00 UTC"
  , publicationState = PSpublish
  , bibliography = Nothing
  , bibliographyGroup = Nothing
  , keywords = Just "test"
  , pageTemplate = Just "page3.yaml"
  , indexPage = True
  , indexSort = SAtitle
  }

linkIn2 = toFilePath . fromJustNote "readMeta2rec relURL wer234c"
  $ (stripPrefix (doughDir testLayout) linkIn :: Maybe (Path Rel File))

test_url = assertEqual "/Blog/postwk.md" $ makeRelPath dough2 linkIn

-- toFilePath . fromJustNote "readMeta2rec relURL wer234c" 
--           $ (stripPrefix (doughDir layout) mdfn :: Maybe (Path Rel File)) linkIn2 
test_makeRelLink = assertEqual resmr (makeRelPath dough2 linkIn)

resmr = "/Blog/postwk.md"

test_makeOneIndexEntry =
  assertEqual resmo (makeOneIndexEntry dough2 indexIn metaRec95)

resmo = Just
  (IndexEntry { text2 = "postwk"
              , link2 = "/Blog/postwk.html"
              , title2 = "index for post"
              , abstract2 = "The directory for experiments."
              , author2 = "AUF"
              , date2 = "2066-06-06 00:00:00 UTC"
              , publish2 = "publish"
              , isIndex = False 
              })

test_getOneIndexEntryPure = assertEqual res22a (getOneIndexEntryPure metaRec95)

res22a = IndexEntry
  { text2 = "postwk"
  , link2 = "/Blog/postwk.html"
  , title2 = "index for post"
  , abstract2 = "The directory for experiments."
  , author2 = "AUF"
  , date2 = "2066-06-06 00:00:00 UTC"
  , publish2 = "publish"
  , isIndex = False 
  } :: IndexEntry

test_makeIndex_1 = do
  res <- runErr
    $ makeIndex
      False -- True
      testLayout
      allFlags  -- not include drafts!
      metaRec1t
  -- (doughDir testLayout)
  -- blogindexfn
  --
  assertEqual res2 res

res2 = Right
  (MenuEntry
   { menu2 =
       [ IndexEntry { text2 = "SubBlog"
                    , link2 = "/Blog/SubBlog/index.html"
                    , title2 = "SubBlog (subdirectory)"
                    , abstract2 = ""
                    , author2 = ""
                    , date2 = ""
                    , publish2 = ""
                    , isIndex = False 
                    }
       , IndexEntry { text2 = ""
                    , link2 = ""
                    , title2 = "------"
                    , abstract2 = ""
                    , author2 = ""
                    , date2 = ""
                    , publish2 = ""
                    , isIndex = False 
                    }
       , IndexEntry { text2 = "postTufteStyled"
                    , link2 = "/Blog/postTufteStyled.html"
                    , title2 = "postTufteStyle.md"
                    , abstract2 = "A text with two levels of title"
                    , author2 = "auf"
                    , date2 = "2019-01-04 00:00:00 UTC"
                    , publish2 = "publish"
                    }
       , IndexEntry { text2 = "postwk"
                    , link2 = "/Blog/postwk.html"
                    , title2 = "postwk.md"
                    , abstract2 = "A silly text not needing an abstract."
                    , author2 = "AUF"
                    , date2 = "2019-01-04 00:00:00 UTC"
                    , publish2 = "publish"
                    }
       , IndexEntry
           { text2 = "postwkTufte"
           , link2 = "/Blog/postwkTufte.html"
           , title2 = "postwkTufte.md"
           , abstract2 = "A silly text not needing an abstract updated."
           , author2 = "auf"
           , date2 = "2019-01-04 00:00:00 UTC"
           , publish2 = "publish"
           }
       , IndexEntry { text2 = "index3"
                    , link2 = "/Blog/index3.html"
                    , title2 = "sort by data (reversed) index 3 for Blog"
                    , abstract2 = "The directory for experiments."
                    , author2 = "AUF"
                    , date2 = "2019-01-04 00:00:00 UTC"
                    , publish2 = "publish"
                    }
       , IndexEntry { text2 = "index2"
                    , link2 = "/Blog/index2.html"
                    , title2 = "sort by date index 2 for Blog"
                    , abstract2 = "The directory for experiments."
                    , author2 = "AUF"
                    , date2 = "2019-01-04 00:00:00 UTC"
                    , publish2 = "publish"
                    }]
   })

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
