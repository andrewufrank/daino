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
import Lib.Md2doc_test  -- to have settings (i.e. layout)
 
import Foundational.Filetypes4sites
import Foundational.MetaPage
import Lib.Templating
import Lib.Indexing
import UniformBase
import Uniform.Pandoc
-- import Data.Aeson
import Uniform.Json
test_exampleTrue = assertEqual 0 0

-- read a docrec 
--"/home/frank/Workspace11/ssg/docs/site/baked/Blog/blog1.docrep" 
-- copy file manulally into .ssg

testing_getMeta f =   test1File "ssg"  
    (f <> "T.docrep") (f <> "_meta") op   
         -- dr1 <- read8 inputFn docrepFileType
op :: Docrep -> MetaPage 
op = meta1 

test_getMeta_blog1 = testing_getMeta "blog1"
test_getMeta_index = testing_getMeta "index"


testing_initializeIx f1 = test1File "ssg"  
    (f1 <> "_meta" ) (f1 <> "_ix" ) (initializeIndex )
test_initializeIx_index = testing_initializeIx "index"

testing_complete f1 =  test1FileIO "ssg"   
    (f1 <> "_ix" )(f1 <> "_ixComp")
        (completeIndex NoticeLevel0 doughPL bakedPL)

test_completeIndex = testing_complete "index"
 