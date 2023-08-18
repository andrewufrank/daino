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
-- import Uniform.Test.TestHarness
import Lib.Md2doc_test  -- to have siteHeader (i.e. layout)
 
import Foundational.Filetypes4sites
import Lib.IndexMake
import UniformBase
import Uniform.Pandoc
-- import Wave.Doc2html
-- import Data.Aeson
import Uniform.Json



test_BlankAuthor_AUF = assertEqual ""  (blankAuthorName  ["AUF", "Andrew U. Frank"] "AUF")
test_BlankAuthor_notAUF = assertEqual a2 (blankAuthorName  ["AUF", "Andrew U. Frank"] a2 )
    where a2 = "XX"

-- 
