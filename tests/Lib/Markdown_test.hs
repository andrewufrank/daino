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


import Wave.Markdown
import Uniform.Pandoc
import Uniform.Json
import UniformBase

test_exampleTrue = assertEqual 0 0
test_readMarkdown2pandoc = test1FileIO "ssg"  "blog1.md" "pandoc_blog1" (readMarkdown2 . MarkdownText)

test_pandoc2docrepJSON = test1File "ssg" "pandoc_blog1" "docrepjson_blog1" f
        -- dr1 <- read8 inputFn docrepFileType
f ::  Pandoc -> DocrepJSON 
f md = DocrepJSON (flattenMeta . getMeta $ md) md 

test_completeDocRep = test1FileIO "ssg" "docrepjson_blog1" "docrepjsonCompleted_blog1"
    (completeDocRep NoticeLevel0 
        (doughDir settings403)
        (bakedDir settings403)
        (makeAbsFile "/home/frank/Workspace11/ssg/docs/site/baked/Blog/blog1.md")
    )

-- unwrapMD :: MarkdownText -> Text
-- unwrapMD (MarkdownText a) = a

instance ShowTestHarness MarkdownText 
instance ShowTestHarness Pandoc 
instance ShowTestHarness DocrepJSON  

settings403 = 
    SiteLayout {themeDir = makeAbsDir "Path Abs Dir /home/frank/Workspace11/ssg/theme/", doughDir = makeAbsDir "Path Abs Dir /home/frank/Workspace11/ssg/docs/site/dough/", bakedDir = makeAbsDir "Path Abs Dir /home/frank/Workspace11/ssg/docs/site/baked/", reportFile = makeAbsFile "Path Abs File /home/frank/SSGreport.txt", testDir = makeAbsDir "Path Abs Dir /home/frank/.SSG/", bannerImage = makeRelFile "Path Rel File cropped-DSC05127-1024x330.jpg", uploadServer = "test.gerastree.at"} :: SiteLayout 