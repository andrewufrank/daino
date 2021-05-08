{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
-- {-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE StandaloneDeriving     #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

module Lib.CheckInputs_test where -- (openMain, htf_thisModuelsTests)

-- import Data.HashMap.Lazy (fromList)
import Data.Aeson.Types
import Data.List ((\\))
import Data.Map (fromList)
-- import           Uniform.Pandoc
-- import Lib.Pandoc

import Lib.CheckInput
-- import Lib.Indexing(getMetaRec)
-- import           Lib.Foundation_test (testLayout)
import Lib.Foundation (SiteLayout (..), layoutDefaults)
import Test.Framework
import Text.Pandoc.Definition hiding (Null)
import Uniform.DocRep
import Uniform.Test.TestHarness
import UniformBase

-- test_allLabels = do
--     res <- runErr $ do
--             let fn1 = (makeAbsFile "/home/frank/Workspace11/ssg/docs/site/baked/Blog/blog1.docrep")
--             dr1 <- read8  fn1 docRepFileType
--             putIOwords ["test_allLabels", showT . yam $ dr1]
--             let bakedP = bakedDir layoutDefaults
--             dr2 <- checkDocRep1 doughP bakedP fn1 (yam dr1)
--             putIOwords ["\t result yam", showT  dr2]
--             return dr2
--     assertEqual (Right exp1)  res

exp1 :: DocYaml
exp1 = zero

-- exp1 = DocYaml {dyFn = "/home/frank/Workspace11/ssg/docs/site/baked/Blog/blog1.docrep", dyLink = "", dyLang = DLenglish, dyTitle = "Mein erster Blog", dyAbstract = "Ein Blog ohne Sinn auf Deutsch", dyAuthor = "", dyDate = Just "2020-06-18", dyKeywords = "Blog", dyBibliography = Nothing, dyStyle = Nothing, dyPublish = Nothing, dyIsIndexPage = False, dyDirEntries = [], dyFileEntries = []}

-- test_resultLabels = do
--     res <- runErr $ do
--             let fn1 = (makeAbsFile "/home/frank/Workspace11/ssg/docs/site/baked/Blog/blog1.docrep")
--             dr1 <- read8  fn1 docRepFileType
--             putIOwords ["test_allLabels", showT . yam $ dr1]
--             let bakedP = bakedDir layoutDefaults
--             dr2 <- checkDocRep bakedP fn1 dr1
--             putIOwords ["\t result dr2", showT  dr2]
--             return dr2
--     assertEqual (Right exp2)  res

exp2 :: DocRep
exp2 = zero

-- exp2 = DocRep {yam = Object (fromList [("fileEntries",Array []),("style",Null),("link",String ""),("bibliography",Null),("lang",String "DLenglish"),("date",String "2020-06-18"),("isIndexPage",Bool False),("keywords",String "Blog"),("author",String ""),("dirEntries",Array []),("abstract",String "Ein Blog ohne Sinn auf Deutsch"),("title",String "Mein erster Blog"),("fn",String "/home/frank/Workspace11/ssg/docs/site/baked/Blog/blog1.docrep"),("publish",Null)]), pan = Pandoc (Meta {unMeta = fromList []}) [Header 1 ("ein-erster-abschnitt",[],[]) [Str "Ein",Space,Str "erster",Space,Str "Abschnitt"],Para [Str "Ein",Space,Str "Blog",Space,Str "ohne",Space,Str "Sinn",Space,Str "und",Space,Str "dem",Space,Str "einzigen",Space,Str "Zweck,",Space,Str "zu",Space,Str "testen,",Space,Str "wie",Space,Str "ein",Space,Str "Blog",Space,Str "in",SoftBreak,Str "ein",Space,Str "PDF",Space,Str "umgewandelt",Space,Str "wird."],Header 1 ("dies-ist-der-zweite-abschnitt",[],[]) [Str "Dies",Space,Str "ist",Space,Str "der",Space,Str "zweite",Space,Str "Abschnitt"],Para [Str "und",Space,Str "auch",Space,Str "ein",Space,Str "bischen",Space,Str "text."],Header 2 ("mit-einer-unterabschnitt",[],[]) [Str "mit",Space,Str "einer",Space,Str "unterabschnitt"],Para [Str "hier."],Para [Str "das",Space,Str "waers.",Space,Str "es",Space,Str "fehlt",Space,Str "Referenzen,",Space,Str "listen",Space,Str "und",Space,Str "aehnliches"]]}

-- psIn = ["true", "publish", "draft", "old", "", "xx", "Publish", "Draft", "OLD"]
-- psRes =  [ PSpublish,  PSpublish,  PSdraft,  PSold,
--      PSzero, PSzero,
--       PSpublish,  PSdraft,  PSold]

-- test_PS = assertEqual psRes (map (text2publish . Just) psIn)

-- sain = ["title", "titel", "date", "reversedate", "reverseDate"
--     , "xx", "", "TITEL"] :: [Text]
-- saRes = [SAtitle, SAtitle, SAdate, SAreverseDate, SAreverseDate
--     , SAzero, SAzero, SAtitle]

-- test_SA = assertEqual saRes (map (text2sortargs . Just) sain)

-- -- test_PSnothing = assertEqual Nothing (text2publish PSzero)

-- getTripleDocX = getTripleDoc layoutDefaults
-- getTripleDocX :: Path Abs File -> ErrIO TripleDoc
-- ----
-- -- test_getTripleDoc_1 :: IO ()
-- -- test_getTripleDoc_1 = test1FileIO progName  "pageFn1" "TripleDoc1" getTripleDocX
-- -- test_getTripleDoc_2 = test1FileIO progName  "pageFn2" "TripleDoc2" getTripleDocX
-- -- test_getTripleDoc_3 = test1FileIO progName  "pageFn3" "TripleDoc3" getTripleDocX
-- -- test_getTripleDoc_4 = test1FileIO progName  "pageFn4" "TripleDoc4" getTripleDocX
-- -- test_getTripleDoc_5 = test1FileIO progName  "pageFn5" "TripleDoc5" getTripleDocX
-- -- test_getTripleDoc_6 = test1FileIO progName  "pageFn6" "TripleDoc6" getTripleDocX

-- instance  ShowTestHarness TripleDoc

-- instance ShowTestHarness (Path Abs File)
--      --

-- linkIndex1 = doughDir testLayout </> makeRelFile "Blog/index.md"
--                         :: Path Abs File

-- -- test_MetaRec_index1 = do
-- --     res <- runErr $   getMetaRec testLayout linkIndex1
-- --     assertEqual (Right metaRecIndex1) res

-- metaRecIndex1 = MetaRec
--   {fn = "/home/frank/Workspace11/ssg/docs/site/dough/Blog/index.md"
--   , relURL = "/Blog/index.md"
--   , title = "primary index for Blog"
--   , abstract = "The directory for experiments.", author = "AUF",
--    date = "2019-01-04 00:00:00 UTC",
--    -- remove date in md file - will be replaced with today
--    publicationState = PSpublish,
--    bibliography = Nothing,
--    bibliographyGroup = Nothing,
--     keywords = Just "test",
--     pageTemplate =  "/home/frank/Workspace11/ssg/theme/templates/page3.yaml",
--     indexPage = True, indexSort = SAzero}

-- subsubDir = makeAbsFile "/home/frank/Workspace11/ssg/docs/site/dough/Blog/SubBlog/SubSub/index.md"

-- -- test_MetaRec_indexSubSub = do
-- --     res <- runErr $   getMetaRec testLayout subsubDir
-- --     assertEqual (Right metaRecIndexSubSub) res

-- metaRecIndexSubSub = (MetaRec{fn =
--              "/home/frank/Workspace11/ssg/docs/site/dough/Blog/SubBlog/SubSub/index.md",
--            relURL = "/Blog/SubBlog/SubSub/index.md",
--            title = "index for subsubdir",
--            abstract = "The subdirectory experiment", author = "AUF",
--            date = "2019-01-04 00:00:00 UTC", publicationState = PSpublish,
--            bibliography = Nothing, bibliographyGroup = Nothing,
--            keywords = Just "test",
--            pageTemplate =  "/home/frank/Workspace11/ssg/theme/templates/page3.yaml",
--            indexPage = True, indexSort = SAzero})

-- keys2  = ["abstract", "title", "author", "date", "publish", "bibliography", "bibliographyGroup"
--                 , "keywords", "pageTemplate", "indexSort"]:: [Text]
-- requiredLabels = ["abstract", "title", "keywords", "pageTemplate", "indexSort"]:: [Text]

-- test_labelsDiff = assertEqual labDiff (keys2 \\ requiredLabels)

-- labDiff = ["author", "date", "publish", "bibliography", "bibliographyGroup"]
-- -- the non-essential labels

-- test_null1 :: IO ()
-- test_null1 = assertBool (null' (""::Text))

-- test_Maybe23, test_Maybe22, test_Maybe21 :: IO ()
-- test_Maybe21 = assertEqual (Just ("a"::Text)) $ reduce (Just (Just ("a"::Text)))
-- test_Maybe22 = assertEqual ((Nothing::Maybe Text)) $ reduce (Just (Nothing::Maybe Text))
-- test_Maybe23 = assertEqual ((Nothing::Maybe Text)) $ reduce (Just (Nothing::Maybe Text))

-- reduce :: Maybe (Maybe a) -> Maybe a
-- reduce (Just a) =   a
-- reduce (Nothing) = Nothing