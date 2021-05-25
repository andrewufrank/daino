-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- {-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports #-}

module Lib.Panrep2pdf_test 
     where

import Test.Framework
import Uniform.Test.TestHarness
import Foundational.MetaPage 
import Foundational.Foundation
import Foundational.Filetypes4sites
import Wave.Md2doc
import Uniform.Pandoc
-- import Uniform.Json
import UniformBase
import Lib.Md2doc_test

-- import Lib.Indexing

testing_panrep2textsnip :: FilePath -> IO ()
-- | test to produce pandoc - step0 in   Md2.doc 
testing_panrep2textsnip f1  = test1FileIO "ssg" ("panrep_" <> f1) (f1 <> "T_texsnip" )(readMarkdown2 . MarkdownText) 

test_blog1_panrep2texsnip = testing_panrep2textsnip   "blog1"
test_index_panrep2texsnip = testing_panrep2textsnip "index"   
test_postwk_panrep2texsnip = testing_panrep2textsnip "postwk"   
test_withRef_panrep2texsnip = testing_panrep2textsnip "withRef"   


