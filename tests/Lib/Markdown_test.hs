-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- {-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports #-}

module Lib.Markdown_test 
     where

import Test.Framework
import Uniform.Test.TestHarness
import Foundational.MetaPage 
import Foundational.Foundation
import Foundational.Filetypes4sites
import Wave.Markdown
import Uniform.Pandoc
import Uniform.Json
import UniformBase
import Lib.Indexing
-- test_docrepjsong2docrep = test1File "ssg" "docrepjson_blog1"
--     "docrep_blog1"
--     (docrepJSON2docrep)


test_dm2dr1 = test1FileIO "ssg" "blog1.md" "dr1_blog1" (readMarkdown2docrep NoticeLevel0 doughP bakedP (makeAbsFile "/home/frank/Workspace11/ssg/docs/site/dough/Blog/blog1.md") .  MarkdownText) 
-- why is here MarkdownText needed?

test_readMarkdown2pandoc = test1FileIO "ssg" "blog1.md" "pandoc_blog1" (readMarkdown2 . MarkdownText) 

test_dr1_d2 = test1FileIO "ssg" "dr1_blog1" "dr2_blog1" (completeDocRep NoticeLevel0 doughP bakedP (makeAbsFile "/home/frank/Workspace11/ssg/docs/site/dough/Blog/blog1.md")  ) 

test_dr2_dr2 = test1FileIO "ssg" "dr2_blog1" "dr3_blog1" 
    (addRefs NoticeLevel0   ) 

test_dm2docrep = test1FileIO "ssg" "blog1.md" "blog1T.docrep" (md2docrep NoticeLevel0 settings403 (makeAbsFile "/home/frank/Workspace11/ssg/docs/site/dough/Blog/blog1.md") .  MarkdownText) 


-- test_readMarkdown2pandoc = test1FileIO "ssg"  "blog1.md" "pandoc_blog1" (readMarkdown2 . MarkdownText)

-- test_pandoc2docrepJSON = test1File "ssg" "pandoc_blog1" "docrepjson_blog1" f
--         -- dr1 <- read8 inputFn docrepFileType
-- f ::  Pandoc -> Docrep
-- f md = DocrepJSON (flattenMeta . getMeta $ md) md 

-- test_completeDocRep = test1FileIO "ssg" "docrepjson_blog1" "docrepjsonCompleted_blog1"
--     (completeDocRep NoticeLevel0 
--         (doughDir settings403)
--         (bakedDir settings403)
--         (makeAbsFile "/home/frank/Workspace11/ssg/docs/site/dough/Blog/blog1.md")  -- muss dough sein, nicht baked
--     )

-- -- needs file with biblio to test 
-- test_addRefs2docrepJSON = test1FileIO "ssg" "docrepjsonCompleted_blog1"
--     "docrepjsonWithRefs_blog1"
--    (addRefs NoticeLevel0 
--             )


-- unwrapMD :: MarkdownText -> Text
-- unwrapMD (MarkdownText a) = a

instance ShowTestHarness MarkdownText 
instance ShowTestHarness Pandoc 
instance ShowTestHarness Docrep  
instance ShowTestHarness MetaPage  
instance ShowTestHarness IndexEntry   

doughP = doughDir settings403 
bakedP = bakedDir settings403

settings403 = 
    SiteLayout {themeDir = makeAbsDir "/home/frank/Workspace11/ssg/theme/", doughDir = makeAbsDir "/home/frank/Workspace11/ssg/docs/site/dough/", bakedDir = makeAbsDir "/home/frank/Workspace11/ssg/docs/site/baked/", reportFile = makeAbsFile "Path Abs File /home/frank/SSGreport.txt", testDir = makeAbsDir "/home/frank/.SSG/", bannerImage = makeRelFile "Path Rel File cropped-DSC05127-1024x330.jpg", uploadServer = "test.gerastree.at"} :: SiteLayout 