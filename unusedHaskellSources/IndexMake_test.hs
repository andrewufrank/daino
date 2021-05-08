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
-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -fno-warn-missing-signatures 
    -fno-warn-orphans -fno-warn-unused-imports 
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
-- import           Uniform.Time (readDate3, UTCTime(..))
import           Lib.CmdLineArgs (allFlags)
import           Lib.CheckInput (MetaRec(..), SortArgs(..)
                        , PublicationState(..)
                               , makeRelPath)
import           Lib.IndexMake (IndexEntry(..)
                    , getOneIndexEntryPure, MenuEntry(..) )
import Lib.CheckInputs_test(metaRecIndex1,linkIndex1)
import Lib.Indexing_test (metaRecPost1,  blogDirPath
                             , linkIn , menuEntryIndex1)


test_getOneIndexEntryPost1 = assertEqual indexEntryPost1 
                                (getOneIndexEntryPure metaRecPost1)

test_getOneIndexEntryIndex1 = assertEqual indexEntryIndex1 
                             (getOneIndexEntryPure metaRecIndex1)

dirContentPost1 =  
    ["/home/frank/Workspace11/ssg/docs/site/dough/Blog/postwk.md",
   "/home/frank/Workspace11/ssg/docs/site/dough/Blog/index.md"] :: [FilePath]
dirsPost1 = ["/home/frank/Workspace11/ssg/docs/site/dough/Blog/SubBlog"] 
                :: [FilePath]


test_DirContentBlog = do 
    res <- runErr $ do
        fns <- getDirContentFiles (toFilePath blogDirPath)
        return  fns 
    assertEqual ( Right dirContentPost1) res

test_DirsBlog =  do 
    res <- runErr $ 
                getDirectoryDirs' (toFilePath blogDirPath)
    assertEqual (Right dirsPost1)  res

-- test_getMetaRecsAfterFilter = do 
--     res <- runErr $  
--         getMetaRecsAfterFilter layoutDefaults linkIndex1 
--                     (map makeAbsFile dirContentPost1)
--     assertEqual (Right metaRecsAfterFilter) res

metaRecsAfterFilter = [MetaRec
    {fn = toFilePath linkIn
  , relURL = "/Blog/postwk.md"
  , title = "postwk with image"
  , abstract = "A silly text not needing an abstract."
  , author = "AUF"
  , date = "2019-01-04 00:00:00 UTC"
  , publicationState = PSpublish
  , bibliography = Nothing
  , bibliographyGroup = Nothing
  , keywords = Just "test"
  , pageTemplate =   "/home/frank/Workspace11/ssg/theme/templates/page3.yaml"
  , indexPage = False
  , indexSort = SAzero}] :: [MetaRec]

test_hasExtension = assertEqual True 
                $ (hasExtension . makeExtension $ "md") linkIndex1

test_getExtension = assertEqual (makeExtension "md")
                         $ getExtension linkIndex1

-- -- | filter the content and retrieve metarecs
-- --    keep md files (but not indexpage)
-- getMetaRecsAfterFilter :: SiteLayout -> Path Abs File 
--     -> [Path Abs File] -> ErrIO [MetaRec]
-- getMetaRecsAfterFilter layout indexpageFn dirContent = do 
--         let fs4 = filter (indexpageFn /=) 
--                     . filter (hasExtension . makeExtension $ "md") 
--                      $ dirContent :: [Path Abs File]
--         metaRecs2 :: [MetaRec]
--                 <- mapM (getMetaRec layout) fs4 -- noch ok    let 
--         return metaRecs2 



-- ixEntriesDirs = [IndexEntry{text2 = "index.md",
--             link2 = "/Blog/index.md/index.html",
--             title2 = "index.md (subdirectory)", abstract2 = "", author2 = "",
--             date2 = "", publish2 = "", isIndex = False},
--  IndexEntry{text2 = "postwk.md",
--             link2 = "/Blog/postwk.md/index.html",
--             title2 = "postwk.md (subdirectory)", abstract2 = "", author2 = "",
--             date2 = "", publish2 = "", isIndex = False},
--  IndexEntry{text2 = "", link2 = "", title2 = "------",
--             abstract2 = "", author2 = "", date2 = "", publish2 = "",
--             isIndex = False}] :: [IndexEntry]   


-- indexP = MenuEntry{menu2 =
--     [IndexEntry{text2 = "SubBlog", link2 = "/Blog/SubBlog/index.html",
--                 title2 = "SubBlog (subdirectory)", abstract2 = "", author2 = "",
--                 date2 = "", publish2 = "", isIndex = False},
--         IndexEntry{text2 = "", link2 = "", title2 = "------",
--                 abstract2 = "", author2 = "", date2 = "", publish2 = "",
--                 isIndex = False},
--         -- IndexEntry{text2 = "index", link2 = "/Blog/index.html",
--         --         title2 = "primary index for Blog",
--         --         abstract2 = "The directory for experiments.", author2 = "AUF",
--         --         date2 = "2019-01-04 00:00:00 UTC", publish2 = "publish",
--         --         isIndex = True},
--                 -- TODO here should be the postwk.md file 
--         IndexEntry{text2 = "", link2 = "", title2 = "------",
--                 abstract2 = "", author2 = "", date2 = "", publish2 = "",
--                 isIndex = False}]} :: MenuEntry


indexEntryPost1 = IndexEntry { text2 = "postwk"
              , link2 = "/Blog/postwk.html"
              , title2 =  "postwk with image"
              , abstract2 = "A silly text not needing an abstract."
              , author2 = "AUF"
              , date2 = "2019-01-04 00:00:00 UTC"
              , publish2 = "publish"
              , isIndex = False 
              }
-- test_getOneIndexEntryPure = assertEqual res22a (getOneIndexEntryPure metaRec95)

indexEntryIndex1 = IndexEntry
  { text2 = "index"
  , link2 = "/Blog/index.html"
  , title2 = "primary index for Blog"
  , abstract2 = "The directory for experiments." -- "index of all blogs."
  , author2 = "AUF"
  , date2 = "2019-01-04 00:00:00 UTC"
  , publish2 = "publish"
  , isIndex = True 
  } :: IndexEntry
