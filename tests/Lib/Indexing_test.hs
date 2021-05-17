{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports #-}

module Lib.Indexing_test where

import Test.Framework
import Uniform.Test.TestHarness
import Foundational.Filetypes4sites
import Foundational.MetaPage
import Lib.Templating
import Lib.Indexing
import UniformBase
import Uniform.Pandoc

test_exampleTrue = assertEqual 0 0

-- read a docrec 
--"/home/frank/Workspace11/ssg/docs/site/baked/Blog/blog1.docrep" 
-- copy file manulally into .ssg

test_readDocrep = test1File "ssg"  "blog1.docrep" "metaBlog1" f
        -- dr1 <- read8 inputFn docrepFileType
f :: Docrep -> MetaPage 
f = fromJustNote "docRepJSON2docrep not a value" . fromJSONValue .yam 

test_initializeIx = test1File "ssg" "metaBlog1" "ixBlog1" (initializeIndex NoticeLevel0)
instance ShowTestHarness Docrep
instance ShowTestHarness MetaPage
instance ShowTestHarness IndexEntry