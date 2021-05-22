-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- {-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports #-}

module Lib.Md2doc_test 
     where

import Test.Framework
import Uniform.Test.TestHarness
import Foundational.MetaPage 
import Foundational.Foundation
import Foundational.Filetypes4sites
import Wave.Md2doc
import Uniform.Pandoc
import Uniform.Json
import UniformBase
import Lib.Indexing

-- | test to produce pandoc 
testing_readMarkdown2pandoc f1  = test1FileIO "ssg" (f1<> ".md") (f1 <> "_dr1" )(readMarkdown2 . MarkdownText) 

test_blog1_readMarkdown2pandoc = testing_readMarkdown2pandoc   "blog1"
test_index_readMarkdown2pandoc = testing_readMarkdown2pandoc "index"   
test_postwk_readMarkdown2pandoc = testing_readMarkdown2pandoc "postwk"   
test_withRef_readMarkdown2pandoc = testing_readMarkdown2pandoc "withRef"   

-- | conversion of markdown file f1 (with extension) to intermediate d11  
testing_md2dr1 f1 = test1FileIO "ssg"  (f1<> ".md") (f1 <> "_dr1" )  (readMarkdown2docrep NoticeLevel0 doughP bakedP fn2process .  MarkdownText) 
  where 
      fn2process:: Path Abs File 
      fn2process = blogRoot </> (makeRelFile f1)
      blogRoot = makeAbsDir "/home/frank/Workspace11/ssg/docs/site/dough/Blog"
      -- TODO needs somewhere fix to build to website root
test_blog1_md2dr1 = testing_md2dr1 "blog1" 
test_index_md2dr1 = testing_md2dr1 "index" 
test_postwk_md2dr1 = testing_md2dr1 "postwk" 
test_withRef_md2dr1 = testing_md2dr1 "withRef" 

-- | testing conversion from dr1 to dr3 
testing_dr12dr3 f1  = test1FileIO "ssg" (f1<> "_dr1") (f1 <> "_dr3" ) (addRefs NoticeLevel0   )


test_blog1_dr1_dr3 = testing_dr12dr3 "blog1"   
test_index_dr1_dr3 = testing_dr12dr3  "index"  
test_postwk_dr1_dr3 = testing_dr12dr3  "postwk"   
test_withRef_dr1_dr3 = testing_dr12dr3  "withRef"    
    

-- | testing dr3 to docrep (check stepwise same result)
testing_md2docrep f1= test1FileIO "ssg" (f1<> ".md") (f1 <> "T.docrep" ) (md2docrep NoticeLevel0 settings403 fn2process .  MarkdownText) 
  where 
      fn2process:: Path Abs File 
      fn2process = blogRoot </> (makeRelFile f1)
      blogRoot = makeAbsDir "/home/frank/Workspace11/ssg/docs/site/dough/Blog"
      -- TODO needs somewhere fix to build to website root
test_blog1_dm2docrep = testing_md2docrep "blog1"
test_index_dm2docrep = testing_md2docrep "index"
test_postwk_dm2docrep = testing_md2docrep "postwk"
test_withRef_dm2docrep = testing_md2docrep "withRef"
 

instance ShowTestHarness MarkdownText 
instance ShowTestHarness Pandoc 
instance ShowTestHarness Docrep  
instance ShowTestHarness MetaPage  
instance ShowTestHarness IndexEntry   

doughP = doughDir settings403 
bakedP = bakedDir settings403

settings403 = 
    SiteLayout {themeDir = makeAbsDir "/home/frank/Workspace11/ssg/theme/", doughDir = makeAbsDir "/home/frank/Workspace11/ssg/docs/site/dough/", bakedDir = makeAbsDir "/home/frank/Workspace11/ssg/docs/site/baked/", reportFile = makeAbsFile "Path Abs File /home/frank/SSGreport.txt", testDir = makeAbsDir "/home/frank/.SSG/", bannerImage = makeRelFile "Path Rel File cropped-DSC05127-1024x330.jpg", uploadServer = "test.gerastree.at"} :: SiteLayout 