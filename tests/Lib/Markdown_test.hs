-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- {-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports #-}

module Lib.Markdown_test where

import Test.Framework
import Uniform.Test.TestHarness
import Foundational.MetaPage 

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

-- unwrapMD :: MarkdownText -> Text
-- unwrapMD (MarkdownText a) = a

instance ShowTestHarness MarkdownText 
instance ShowTestHarness Pandoc 
instance ShowTestHarness DocrepJSON  
