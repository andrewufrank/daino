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

post1 = makeAbsFile "/home/frank/Workspace8/SSG/site/dough/Blog/postwk.md" :: Path Abs File
post2 = makeAbsFile "/home/frank/Workspace8/SSG/site/dough/Blog/postwk2.md" :: Path Abs File

test_md2 = testFileReadWrite post1 post2 markdownFileType
-- show produces the "xx"
--test_md = do
--            res <- runErr $
--                do
--                    mdf :: MarkdownText <- read8 (doughD </> post1) markdownFileType
--                    write8 (testD </> post1) markdownFileType mdf
--                    mdf2 <- read8 (testD </> post1) markdownFileType
--                    return (mdf == mdf2)
--            assertEqual  (Right True) res

testDocVal = makeAbsFile "/home/frank/.SSG/Blog/postwk.content.docval"
testDocVal2 = makeAbsFile "/home/frank/.SSG/Blog/postwkTest.content.docval"

test_docvalue2 = testFileReadWrite testDocVal testDocVal2 docValueFileType

--test_docvalue = do
--            res <- runErr $
--                do
--                    inf :: DocValue <- read8 testDocVal docValueFileType
--                    write8 testDocVal2 docValueFileType inf
--                    f2 <- read8 testDocVal2 docValueFileType
--                    return (inf == f2)
--            assertEqual  (Right True) res

testYAML = makeAbsFile "/home/frank/Workspace8/SSG/site/dough/master.yaml"
testYAML2 = makeAbsFile "/home/frank/Workspace8/SSG/site/dough/masterTest.yaml"


test_YAMLue = testFileReadWrite testYAML testYAML2 yamlFileType

testHTMLout = makeAbsFile "/home/frank/.SSG/Blog/postwk.a.html"
testHTMLout2 = makeAbsFile "/home/frank/.SSG/Blog/postwk.a2.html"

test_htmlout = testFileReadWrite testHTMLout testHTMLout2 htmloutFileType

testFileReadWrite file1 file2 filetype = do
            res <- runErr $
                do
                    inf <- read8 file1 filetype
                    write8 file2 filetype inf
                    f2 <- read8 file2 filetype
                    return (inf == f2)
            assertEqual  (Right True) res

