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
{-# LANGUAGE StandaloneDeriving     #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

module Lib.CheckInputs_test  -- (openMain, htf_thisModuelsTests)
     where


import Test.Framework
import Data.List ( (\\) )
import Uniform.Strings
import Uniform.Test.TestHarness
import Uniform.DocRep 
-- import           Uniform.Pandoc
-- import Lib.Pandoc 

import Lib.CheckInput
-- import Lib.Indexing(getMetaRec)
-- import           Lib.Foundation_test (testLayout)
import Lib.Foundation ( SiteLayout (..), layoutDefaults)


test_allLabels = do 
    res <- runErr $ do 
            dr1 <- read8 (makeAbsFile "/home/frank/Workspace8/ssg/docs/site/baked/Blog/blog1.docrep") docRepFileType
            putIOwords ["test_allLabels", showT . yam $ dr1]
            miss1x <- checkDocRep dr1 
            putIOwords ["\t missing", showT miss1x]
            return miss1x 
    assertEqual (Right "()" ) res 

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
--   {fn = "/home/frank/Workspace8/ssg/docs/site/dough/Blog/index.md"
--   , relURL = "/Blog/index.md"
--   , title = "primary index for Blog"
--   , abstract = "The directory for experiments.", author = "AUF",
--    date = "2019-01-04 00:00:00 UTC", 
--    -- remove date in md file - will be replaced with today
--    publicationState = PSpublish,
--    bibliography = Nothing, 
--    bibliographyGroup = Nothing,
--     keywords = Just "test",
--     pageTemplate =  "/home/frank/Workspace8/ssg/theme/templates/page3.yaml",
--     indexPage = True, indexSort = SAzero}

-- subsubDir = makeAbsFile "/home/frank/Workspace8/ssg/docs/site/dough/Blog/SubBlog/SubSub/index.md"

-- -- test_MetaRec_indexSubSub = do 
-- --     res <- runErr $   getMetaRec testLayout subsubDir  
-- --     assertEqual (Right metaRecIndexSubSub) res 


-- metaRecIndexSubSub = (MetaRec{fn =
--              "/home/frank/Workspace8/ssg/docs/site/dough/Blog/SubBlog/SubSub/index.md",
--            relURL = "/Blog/SubBlog/SubSub/index.md",
--            title = "index for subsubdir",
--            abstract = "The subdirectory experiment", author = "AUF",
--            date = "2019-01-04 00:00:00 UTC", publicationState = PSpublish,
--            bibliography = Nothing, bibliographyGroup = Nothing,
--            keywords = Just "test",
--            pageTemplate =  "/home/frank/Workspace8/ssg/theme/templates/page3.yaml",
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