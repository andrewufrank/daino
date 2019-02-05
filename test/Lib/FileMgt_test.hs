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
--import Uniform.Strings hiding ((</>))
--import Lib.Bake
import Uniform.Filenames
import Uniform.TypedFile
import Lib.Foundation   (layoutDefaults, SiteLayout (..), sourceDir)
import Lib.FileMgt

doughD = doughDir layoutDefaults
testD = testDir layoutDefaults

post1 = sourceDir </> makeRelFile "site/dough/Blog/postwk.md" :: Path Abs File
post2 = testD </> makeRelFile "site/dough/Blog/postwk2.md" :: Path Abs File

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

testDocVal = testD </> makeRelFile "Blog/postwk.content.docval"
testDocVal2 = testD </> makeRelFile "Blog/postwkTest.content.docval"

test_docvalue2 = testFileReadWrite testDocVal testDocVal2 docValueFileType

--test_docvalue = do
--            res <- runErr $
--                do
--                    inf :: DocValue <- read8 testDocVal docValueFileType
--                    write8 testDocVal2 docValueFileType inf
--                    f2 <- read8 testDocVal2 docValueFileType
--                    return (inf == f2)
--            assertEqual  (Right True) res

testYAML = sourceDir </> makeRelFile "site/dough/master.yaml"
testYAML2 = testD </> makeRelFile "site/dough/masterTest.yaml"


test_YAMLue = testFileReadWrite testYAML testYAML2 yamlFileType

testHTMLout = testD </> makeRelFile "Blog/postwk.a.html"
testHTMLout2 = testD </> makeRelFile "Blog/postwk.a2.html"

test_htmlout = testFileReadWrite testHTMLout testHTMLout2 htmloutFileType

--testGtemp = sourceDir </> makeRelFile "theme/templates/Master3.gtpl"
--testGtemp2 = testD </> makeRelFile "theme/templates/Master32.gtpl"
--
--test_gtemp = testFileReadWrite testGtemp testGtemp2 gtmplFileType

testDtemp = sourceDir </> makeRelFile "theme/templates/Page3.dtpl"
testDtemp2 =testD </> makeRelFile "theme/templates/Page32.dtpl"

test_dtemp = testFileReadWrite testDtemp testDtemp2 dtmplFileType

testFileReadWrite file1 file2 filetype = do
            res <- runErr $
                do
                    inf <- read8 file1 filetype
                    write8 file2 filetype inf
                    f2 <- read8 file2 filetype
                    return (inf == f2)
            assertEqual  (Right True) res

