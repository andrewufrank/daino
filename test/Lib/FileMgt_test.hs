-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans #-}

module Lib.FileMgt_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import Uniform.Strings hiding ((</>))
import Lib.Bake
import Uniform.Filenames
import Lib.Foundation (layoutDefaults, SiteLayout (..))
import Lib.FileMgt

doughD = doughDir layoutDefaults
testD = makeAbsDir "/home/frank/.SSG" :: Path Abs Dir

post1 = makeRelFile "Blog/postwk.md" :: Path Rel File

-- show produces the "xx"
test_1 = do
            res <- runErr $
                do
                    mdf :: MarkdownText <- read8 (doughD </> post1) markdownFileType
                    write8 (testD </> post1) markdownFileType mdf
                    mdf2 <- read8 (testD </> post1) markdownFileType
                    return (mdf == mdf2)
            assertEqual  (Right True) res


