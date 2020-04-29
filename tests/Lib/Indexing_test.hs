-----------------------------------------------------------------------------
--
-- Module      :   indexing tests
-- only the tests for processing test cases with ErrIO 
-- not currently used 
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

import Uniform.Pointless
import           Lib.Foundation (layoutDefaults, doughDir)
import           Lib.Foundation (progName, SiteLayout(..), templatesDirName)
import           Lib.Foundation_test (testLayout)
import           Lib.Indexing -- (applyTemplate2, convGmaster)
import           Lib.Templating
import           Test.Framework
import           Uniform.Test.TestHarness
import           Uniform.Time (readDate3, UTCTime(..))
import           Lib.CmdLineArgs (allFlags)
import           Lib.CheckInput (MetaRec(..), SortArgs(..)
                    , PublicationState(..), makeRelPath
                    , getTripleDoc)
import          Lib.IndexMake (MenuEntry(..), IndexEntry(..))
import          Lib.CheckInputs_test (metaRecIndex1)

    -- TEST DIRS 
test_dough2 =  assertEqual dough2path dough2  
dough2 = doughDir testLayout
dough2path = makeAbsDir "/home/frank/Workspace8/ssg/docs/site/dough/"

-- resmr = "/Blog/postwk.md"
test_blogDir = assertEqual blogDirPath blogDir
blogDir = doughDir testLayout </> makeRelDir "Blog"
blogDirPath = makeAbsDir "/home/frank/Workspace8/ssg/docs/site/dough/Blog"

test_linkIn = assertEqual "/Blog/postwk.md" $ makeRelPath dough2 linkIn
linkIn = doughDir testLayout </> makeRelFile "Blog/postwk.md" :: Path Abs File

test_MetaRec_postwk = do 
    res <- runErr $   getMetaRec testLayout linkIn 
    assertEqual (Right metaRecPost1) res 

metaRecPost1 = MetaRec
  { fn = toFilePath linkIn
  , relURL = "/Blog/postwk.md"
  , title = "postwk with image"
  , abstract = "A silly text not needing an abstract."
  , author = "AUF"
  , date = "2019-01-04 00:00:00 UTC"
  , publicationState = PSpublish
  , bibliography = Nothing
  , bibliographyGroup = Nothing
  , keywords = Just "test"
  , pageTemplate = Just "/home/frank/Workspace8/ssg/theme/templates/page3.yaml"
  , indexPage = False
  , indexSort = SAzero
  }
  

test_getDirContent2metarec = do 
    res <- runErr $ getDirContent2metarec  metaRecIndex1 
                -- the metarec of the index in the current dir 
    assertEqual contentPost1 
            (cross (map toFilePath, map toFilePath) . fromRight zero  $ res) 

contentPost1 = (["/home/frank/Workspace8/ssg/docs/site/dough/Blog/SubBlog/"],
    ["/home/frank/Workspace8/ssg/docs/site/dough/Blog/postwk.md"]) 
        :: ([FilePath], [FilePath])

    
menuEntryPost1 = zero :: MenuEntry 

test_makeIndexIndex1 = do 
    res <- runErr $ makeIndex1 False testLayout allFlags metaRecIndex1
    assertEqual (Right makeIndexBlog) res 

makeIndexBlog = 
  (MetaRec{fn =
             "/home/frank/Workspace8/ssg/docs/site/dough/Blog/index.md",
           relURL = "/Blog/index.md", title = "primary index for Blog",
           abstract = "The directory for experiments." 
                    -- "index of all blogs."
           , author = "AUF",
           date = "2019-01-04 00:00:00 UTC", publicationState = PSpublish,
           bibliography = Nothing, bibliographyGroup = Nothing,
           keywords = Just "test",
           pageTemplate =
             Just "/home/frank/Workspace8/ssg/theme/templates/page3.yaml",
           indexPage = True, indexSort = SAreverseDate
           },
   [MetaRec{fn =
              "/home/frank/Workspace8/ssg/docs/site/dough/Blog/postwk.md",
            relURL = "/Blog/postwk.md", title = "postwk with image",
            abstract = "A silly text not needing an abstract.", author = "AUF",
            date = "2019-01-04 00:00:00 UTC", publicationState = PSpublish,
            bibliography = Nothing, bibliographyGroup = Nothing,
            keywords = Just "test",
            pageTemplate =
              Just "/home/frank/Workspace8/ssg/theme/templates/page3.yaml",
            indexPage = False, indexSort = SAzero}],
   [MetaRec{fn =
              "/home/frank/Workspace8/ssg/docs/site/dough/Blog/SubBlog/index.md",
            relURL = "/Blog/SubBlog/index.md", title = "index for subdir",
            abstract = "The subdirectory experiment", author = "AUF",
            date = "2019-01-04 00:00:00 UTC", publicationState = PSpublish,
            bibliography = Nothing, bibliographyGroup = Nothing,
            keywords = Just "test",
            pageTemplate =
              Just "/home/frank/Workspace8/ssg/theme/templates/page3.yaml",
            indexPage = True, indexSort = SAtitle}]) 
        :: (MetaRec, [MetaRec], [MetaRec])


test_convert2index = assertEqual menuEntryIndex1 
            (convert2index makeIndexBlog )

menuEntryIndex1 =  MenuEntry{menu2 =
        [IndexEntry{text2 = "index", link2 = "/Blog/index.html",
                        title2 = "primary index for Blog",
                        abstract2 = "The directory for experiments.", author2 = "AUF",
                        date2 = "2019-01-04 00:00:00 UTC", publish2 = "publish",
                        isIndex = True},
             IndexEntry{text2 = "", link2 = "", title2 = "--- subdir ---",
                        abstract2 = "", author2 = "", date2 = "", publish2 = "",
                        isIndex = False},
             IndexEntry{text2 = "index", link2 = "/Blog/SubBlog/index.html",
                        title2 = "index for subdir",
                        abstract2 = "The subdirectory experiment", author2 = "AUF",
                        date2 = "2019-01-04 00:00:00 UTC", publish2 = "publish",
                        isIndex = True},
             IndexEntry{text2 = "", link2 = "", title2 = "--- content ---",
                        abstract2 = "", author2 = "", date2 = "", publish2 = "",
                        isIndex = False},
             IndexEntry{text2 = "postwk", link2 = "/Blog/postwk.html",
                        title2 = "postwk with image",
                        abstract2 = "A silly text not needing an abstract.",
                        author2 = "AUF", date2 = "2019-01-04 00:00:00 UTC",
                        publish2 = "publish", isIndex = False}]
              } :: MenuEntry 




