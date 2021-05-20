{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports #-}

module Lib.IndexMake_test where

import Test.Framework
import Uniform.Test.TestHarness
import Lib.Md2doc_test  -- to have settings (i.e. layout)
 
import Foundational.Filetypes4sites
import Foundational.MetaPage
import Lib.Templating
import Lib.IndexMake
import UniformBase
import Uniform.Pandoc
-- import Data.Aeson
import Uniform.Json

testing_indexEntry2MenuEntry f = test1FileIO "ssg"   ("ixEntry1_" <> f) (  "ixEntry4__" <> f) convertIndexEntries      
 
test_indexEntry2MenuEntry_blog1 = testing_indexEntry2MenuEntry "blog1"
test_indexEntry2MenuEntry_index = testing_indexEntry2MenuEntry "index"
test_indexEntry2MenuEntry_postwk = testing_indexEntry2MenuEntry "postwk"
test_indexEntry2MenuEntry_withRef = testing_indexEntry2MenuEntry "withRef"
-- instance ShowTestHarness Docrep
-- instance ShowTestHarness MetaPage
instance ShowTestHarness MenuEntry 

