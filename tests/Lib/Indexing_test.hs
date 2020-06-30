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

import           Uniform.Pointless
import           Lib.Foundation                 ( layoutDefaults
                                                , doughDir
                                                )
import           Lib.Foundation                 ( progName
                                                , SiteLayout(..)
                                                , templatesDirName
                                                )
import           Lib.Foundation_test            ( testLayout )
import           Lib.Indexing -- (applyTemplate2, convGmaster)
-- import           Lib.Templating
import           Test.Framework
import           Uniform.Test.TestHarness
import           Uniform.Time                   ( readDate3
                                                , UTCTime(..)
                                                )
import           Lib.CmdLineArgs                ( allFlags )
-- import Lib.DocRep 
import           Lib.CheckInput
                -- (MetaRec(..), SortArgs(..)
                --     , PublicationState(..), makeRelPath
                --     , getTripleDoc)
-- import          Lib.IndexMake (MenuEntry(..), IndexEntry(..))
-- import          Lib.CheckInputs_test -- (metaRecIndex1, metaRecIndexSubSub)
import           Uniform.DocRep
import Uniform.Json (shownice)
import Uniform.Markdown 

    -- TEST DIRS 
test_dough2 = assertEqual dough2path dough2
dough2 = doughDir testLayout
dough2path = makeAbsDir "/home/frank/Workspace8/ssg/docs/site/dough/"

-- resmr = "/Blog/postwk.md"
test_blogDir = assertEqual blogDirPath blogDir
blogDir = doughDir testLayout </> makeRelDir "Blog"
blogDirPath = makeAbsDir "/home/frank/Workspace8/ssg/docs/site/dough/Blog"

docrepfn =
    makeAbsFile "/home/frank/Workspace8/ssg/docs/site/baked/Blog/index.docrep"   -- the index in the top directory of the blog 

mdindexfn = makeAbsFile "/home/frank/Workspace8/ssg/docs/site/baked/Blog/index.md"

test_addIndex = do
    res <- runErr $ do
        -- corrections from readMarkdown activated
        md1 <- read8 mdindexfn markdownFileType
        dr1 <- readMarkdown2docrep md1 -- just reading, no ssg stuff
        putIOwords ["test_addIndex dr1",  showT $ dr1, "\n"]
        dr2 <- checkDocRep mdindexfn dr1 -- the ssg stuff
        putIOwords ["test_addIndex dr2",  showT $ dr2, "\n"]
        dr3 <- addIndex2yam True dr2
        putIOwords ["test_addIndex end dr3", take' 300 . showT $ dr3]
        return dr3
    assertEqual (Right res2) res

res2 = zero
-- res2 = DocRep {yam = Object (fromList [("fileEntries",Array []),("style",Null),("indexPage",Bool True),("link",String ""),("bibliography",Null),("lang",String "DLenglish"),("date",String "Jan. 4, 2019"),("indexSort",String "reverseDate"),("isIndexPage",Bool True),("keywords",String "test"),("author",String "AUF"),("dirEntries",Array []),("abstract",String "The directory for experiments."),("title",String "primary index for Blog"),("fn",String "/home/frank/Workspace8/ssg/docs/site/dough/Blog/index.md"),("pageTemplate",String "page3.yaml"),("publish",Null)]), pan = Pandoc (Meta {unMeta = fromList []}) [Para [Str "an",Space,Str "index",Space,Str "page",Space,Str "for",Space,Str "Blog"]]}


-- test_linkIn = assertEqual "/Blog/postwk.md" $ makeRelPath dough2 linkIn
-- linkIn = doughDir testLayout </> makeRelFile "Blog/postwk.md" :: Path Abs File

-- test_MetaRec_postwk = do 
--     res <- runErr $   getMetaRec testLayout linkIn 
--     assertEqual (Right metaRecPost1) res 

-- metaRecPost1 = MetaRec
--   { fn = toFilePath linkIn
--   , relURL = "/Blog/postwk.md"
--   , title = "postwk with image"
--   , abstract = "A silly text not needing an abstract."
--   , author = "AUF"
--   , date = "2019-01-04 00:00:00 UTC"
--   , publicationState = PSpublish
--   , bibliography = Nothing
--   , bibliographyGroup = Nothing
--   , keywords = Just "test"
--   , pageTemplate =   "/home/frank/Workspace8/ssg/theme/templates/page3.yaml"
--   , indexPage = False
--   , indexSort = SAzero
--   }


-- test_getDirContent2metarec = do 
--     res <- runErr $ getDirContent2metarec  metaRecIndex1 
--                 -- the metarec of the index in the current dir 
--     assertEqual contentPost1 
--             (cross (map toFilePath, map toFilePath) . fromRight zero  $ res) 

-- contentPost1 = (["/home/frank/Workspace8/ssg/docs/site/dough/Blog/SubBlog/"],
--     ["/home/frank/Workspace8/ssg/docs/site/dough/Blog/postwk.md"]) 
--         :: ([FilePath], [FilePath])


-- menuEntryPost1 = zero :: MenuEntry 

-- test_makeIndexIndex1 = do 
--     res <- runErr $ makeIndex1 False testLayout allFlags metaRecIndex1
--     assertEqual (Right makeIndexBlog) res 

-- makeIndexBlog = 
--   (MetaRec{fn =
--              "/home/frank/Workspace8/ssg/docs/site/dough/Blog/index.md",
--            relURL = "/Blog/index.md", title = "primary index for Blog",
--            abstract = "The directory for experiments." 
--                     -- "index of all blogs."
--            , author = "AUF",
--            date = "2019-01-04 00:00:00 UTC", publicationState = PSpublish,
--            bibliography = Nothing, bibliographyGroup = Nothing,
--            keywords = Just "test",
--            pageTemplate =
--                "/home/frank/Workspace8/ssg/theme/templates/page3.yaml",
--            indexPage = True, indexSort = SAreverseDate
--            },
--    [MetaRec{fn =
--               "/home/frank/Workspace8/ssg/docs/site/dough/Blog/postwk.md",
--             relURL = "/Blog/postwk.md", title = "postwk with image",
--             abstract = "A silly text not needing an abstract.", author = "AUF",
--             date = "2019-01-04 00:00:00 UTC", publicationState = PSpublish,
--             bibliography = Nothing, bibliographyGroup = Nothing,
--             keywords = Just "test",
--             pageTemplate =
--                "/home/frank/Workspace8/ssg/theme/templates/page3.yaml",
--             indexPage = False, indexSort = SAzero}],
--    [MetaRec{fn =
--               "/home/frank/Workspace8/ssg/docs/site/dough/Blog/SubBlog/index.md",
--             relURL = "/Blog/SubBlog/index.md", title = "index for subdir",
--             abstract = "The subdirectory experiment", author = "AUF",
--             date = "2019-01-04 00:00:00 UTC", publicationState = PSpublish,
--             bibliography = Nothing, bibliographyGroup = Nothing,
--             keywords = Just "test",
--             pageTemplate =
--                 "/home/frank/Workspace8/ssg/theme/templates/page3.yaml",
--             indexPage = True, indexSort = SAtitle}]) 
--         :: (MetaRec, [MetaRec], [MetaRec])


-- test_convert2index = assertEqual menuEntryIndex1 
--             (convert2index makeIndexBlog )

-- menuEntryIndex1 =  MenuEntry{menu2 =
--         [IndexEntry{text2 = "index", link2 = "/Blog/index.html",
--                         title2 = "primary index for Blog",
--                         abstract2 = "The directory for experiments.", author2 = "AUF",
--                         date2 = "2019-01-04 00:00:00 UTC", publish2 = "publish",
--                         isIndex = True},
--              IndexEntry{text2 = "", link2 = "", title2 = "--- subdir ---",
--                         abstract2 = "", author2 = "", date2 = "", publish2 = "",
--                         isIndex = False},
--              IndexEntry{text2 = "index", link2 = "/Blog/SubBlog/index.html",
--                         title2 = "index for subdir",
--                         abstract2 = "The subdirectory experiment", author2 = "AUF",
--                         date2 = "2019-01-04 00:00:00 UTC", publish2 = "publish",
--                         isIndex = True},
--              IndexEntry{text2 = "", link2 = "", title2 = "--- content ---",
--                         abstract2 = "", author2 = "", date2 = "", publish2 = "",
--                         isIndex = False},
--              IndexEntry{text2 = "postwk", link2 = "/Blog/postwk.html",
--                         title2 = "postwk with image",
--                         abstract2 = "A silly text not needing an abstract.",
--                         author2 = "AUF", date2 = "2019-01-04 00:00:00 UTC",
--                         publish2 = "publish", isIndex = False}]
--               } :: MenuEntry 

-- test_makeIndexIndexSubSub = do 
--     res <- runErr $ makeIndex1 False testLayout allFlags metaRecIndexSubSub
--     assertEqual (Right makeIndexSubSub) res 

-- makeIndexSubSub = (MetaRec{fn =
--              "/home/frank/Workspace8/ssg/docs/site/dough/Blog/SubBlog/SubSub/index.md",
--            relURL = "/Blog/SubBlog/SubSub/index.md",
--            title = "index for subsubdir",
--            abstract = "The subdirectory experiment", author = "AUF",
--            date = "2019-01-04 00:00:00 UTC", publicationState = PSpublish,
--            bibliography = Nothing, bibliographyGroup = Nothing,
--            keywords = Just "test",
--            pageTemplate =
--                "/home/frank/Workspace8/ssg/theme/templates/page3.yaml",
--            indexPage = True, indexSort = SAtitle},
--    [MetaRec{fn =
--               "/home/frank/Workspace8/ssg/docs/site/dough/Blog/SubBlog/SubSub/subsub1.md",
--             relURL = "/Blog/SubBlog/SubSub/subsub1.md", title = "postwk.md",
--             abstract = "A silly text not needing an abstract.", author = "AUF",
--             date = "2019-01-04 00:00:00 UTC", publicationState = PSpublish,
--             bibliography = Nothing, bibliographyGroup = Nothing,
--             keywords = Just "test",
--             pageTemplate =
--                 "/home/frank/Workspace8/ssg/theme/templates/page3.yaml",
--             indexPage = False, indexSort = SAzero},
--     MetaRec{fn =
--               "/home/frank/Workspace8/ssg/docs/site/dough/Blog/SubBlog/SubSub/subsubTest.md",
--             relURL = "/Blog/SubBlog/SubSub/subsubTest.md", title = "subsub z",
--             abstract = "A silly text not needing an abstract.", author = "AUF",
--             date = "2022-01-04 00:00:00 UTC", publicationState = PSpublish,
--             bibliography = Nothing, bibliographyGroup = Nothing,
--             keywords = Just "notest",
--             pageTemplate =
--                 "/home/frank/Workspace8/ssg/theme/templates/page3.yaml",
--             indexPage = False, indexSort = SAzero}],
--    [])


-- test_convert2indexSubSub = assertEqual menuEntryIndexSubSub 
--             (convert2index makeIndexSubSub )



-- menuEntryIndexSubSub = MenuEntry{menu2 =
--     [IndexEntry{text2 = "index",
--                 link2 = "/Blog/SubBlog/SubSub/index.html",
--                 title2 = "index for subsubdir",
--                 abstract2 = "The subdirectory experiment", author2 = "AUF",
--                 date2 = "2019-01-04 00:00:00 UTC", publish2 = "publish",
--                 isIndex = True},
--         IndexEntry{text2 = "", link2 = "", title2 = "--- content ---",
--                 abstract2 = "", author2 = "", date2 = "", publish2 = "",
--                 isIndex = False},
--         IndexEntry{text2 = "subsub1",
--                 link2 = "/Blog/SubBlog/SubSub/subsub1.html", title2 = "postwk.md",
--                 abstract2 = "A silly text not needing an abstract.",
--                 author2 = "AUF", date2 = "2019-01-04 00:00:00 UTC",
--                 publish2 = "publish", isIndex = False},
--         IndexEntry{text2 = "subsubTest",
--                 link2 = "/Blog/SubBlog/SubSub/subsubTest.html",
--                 title2 = "subsub z",
--                 abstract2 = "A silly text not needing an abstract.",
--                 author2 = "AUF", date2 = "2022-01-04 00:00:00 UTC",
--                 publish2 = "publish", isIndex = False}]}
--         :: MenuEntry
