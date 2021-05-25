{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- {-# OPTIO-unused-imports #-}

module Lib.Doc2html_test where

import Foundational.Filetypes4sites
import Foundational.Foundation
import Foundational.MetaPage
import Lib.Indexing
import Lib.Md2doc_test
import Test.Framework
import Uniform.Json
import Uniform.Pandoc
import Uniform.Test.TestHarness
import Uniform2.HTMLout
import UniformBase
import Wave.Doc2html

-- -- | test to produce pan
testing_md2pan f1 f2 = test1FileIO "ssg" f1 f2 (docrep2panrep NoticeLevel0 layoutDefaults )

test_blog1_md2pan = testing_md2pan "blog1T.docrep" "panrep_blog1"
test_index_md2pan = testing_md2pan "indexT.docrep" "panrep_index"
test_postwk_md2pan = testing_md2pan "postwkT.docrep" "panrep_postwk"
test_withRef_md2pan = testing_md2pan "withRefT.docrep" "panrep_withRef"

testing_pan2indexEntry f = test1File "ssg" ("panrep_" <> f) ("ixEntry1_" <> f) op -- dr1 <- read8 inputFn docrepFileType
op :: Panrep -> IndexEntry
op = dyIndexEntry . panyam

test_pan2indexEntry_blog1 = testing_pan2indexEntry "blog1"
test_pan2indexEntry_index = testing_pan2indexEntry "index"
test_pan2indexEntry_postwk = testing_pan2indexEntry "postwk"
test_pan2indexEntry_withRef = testing_pan2indexEntry "withRef"

testing_pan2HTMLout f = test1FileIO "ssg" ("panrep_" <> f) ("htmlout_" <> f) op1 -- dr1 <- read8 inputFn docrepFileType
op1 :: Panrep -> ErrIO HTMLout
op1 = panrep2html NoticeLevel0 layoutDefaults 

test_pan2HTMLout_blog1 = testing_pan2HTMLout "blog1"
test_pan2HTMLout_index = testing_pan2HTMLout "index"
test_pan2HTMLout_postwk = testing_pan2HTMLout "postwk"
test_pan2HTMLout_withRef = testing_pan2HTMLout "withRef"

------------ old -----

-- -- | conversion of markdown file f1 (with extension) to intermediate d11
-- testing_md2dr1 f1 = test1FileIO "ssg"  (f1<> ".md") (f1 <> "_dr1" )  (pandoc2docrep NoticeLevel0 doughP bakedP fn2process .  MarkdownText)
--   where
--       fn2process:: Path Abs File
--       fn2process = blogRoot </> (makeRelFile f1)
--       blogRoot = makeAbsDir "/home/frank/Workspace11/ssg/docs/site/dough/Blog"
--       -- TODO needs somewhere fix to build to website root
-- test_blog1_md2dr1 = testing_md2dr1 "blog1"
-- test_index_md2dr1 = testing_md2dr1 "index"
-- test_postwk_md2dr1 = testing_md2dr1 "postwk"
-- test_withRef_md2dr1 = testing_md2dr1 "withRef"

-- -- | testing conversion from dr1 to dr3
-- testing_dr12dr3 f1  = test1FileIO "ssg" (f1<> "_dr1") (f1 <> "_dr3" ) (addRefs NoticeLevel0   )

-- test_blog1_dr1_dr3 = testing_dr12dr3 "blog1"
-- test_index_dr1_dr3 = testing_dr12dr3  "index"
-- test_postwk_dr1_dr3 = testing_dr12dr3  "postwk"
-- test_withRef_dr1_dr3 = testing_dr12dr3  "withRef"

-- -- | testing dr3 to docrep (check stepwise same result)
-- testing_md2docrep f1= test1FileIO "ssg" (f1<> ".md") (f1 <> "T.docrep" ) (md2docrep NoticeLevel0 settings403 fn2process .  MarkdownText)
--   where
--       fn2process:: Path Abs File
--       fn2process = blogRoot </> (makeRelFile f1)
--       blogRoot = makeAbsDir "/home/frank/Workspace11/ssg/docs/site/dough/Blog"
--       -- TODO needs somewhere fix to build to website root
-- test_blog1_dm2docrep = testing_md2docrep "blog1"
-- test_index_dm2docrep = testing_md2docrep "index"
-- test_postwk_dm2docrep = testing_md2docrep "postwk"
-- test_withRef_dm2docrep = testing_md2docrep "withRef"

-- instance ShowTestHarness MarkdownText
-- instance ShowTestHarness Pandoc
-- instance ShowTestHarness Docrep
-- instance ShowTestHarness MetaPage
-- instance ShowTestHarness IndexEntry
instance ShowTestHarness Panrep
instance ShowTestHarness HTMLout

-- doughP = doughDir settings403
-- bakedP = bakedDir settings403

-- settings403 =
--     SiteLayout {themeDir = makeAbsDir "/home/frank/Workspace11/ssg/theme/", doughDir = makeAbsDir "/home/frank/Workspace11/ssg/docs/site/dough/", bakedDir = makeAbsDir "/home/frank/Workspace11/ssg/docs/site/baked/", reportFile = makeAbsFile "Path Abs File /home/frank/SSGreport.txt", testDir = makeAbsDir "/home/frank/.SSG/", bannerImage = makeRelFile "Path Rel File cropped-DSC05127-1024x330.jpg", uploadServer = "test.gerastree.at"} :: SiteLayout