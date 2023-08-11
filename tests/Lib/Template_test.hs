{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports #-}

module Lib.Template_test where

import Test.Framework
-- import Uniform.Test.TestHarness

import Lib.Templating

test_exampleTrue = assertEqual 0 0

settFn = makeAbsFile "/home/frank/Workspace11/daino/settings3.yaml"
fnin = makeAbsFile "/home/frank/Workspace11/dainoSite/ReadMe/index.md"
htmlTest = makeAbsFile "/home/frank/Workspace11/u4blog/uniform-pandoc/resources/metaplusHtml.dtpl"



testing_templatehtml = do 
    res1 <- runErr $ do 
        let debug = NoticeLevel0
        sett3 <- readSettings debug settFn 
        docrefp1 <- readmarkdonwFile2docrep debug sett3 fnin

        -- docrep2panrep does currenty nothing
        -- from panrep2htm
        htmlTempl  <- compileTemplateFile2 htmlTest

        htm1 <- meta2xx writeHtml5String2 (metap metaplus4)
        let metaplus5 = metaplus4{metaHtml = htm1}
    
        let hpl1 = renderTemplate htmlTempl (toJSON metaplus5)  -- :: Doc Text
        -- putIOwords ["tpl1 \n", showT tpl1]
        let ht1 = render (Just 50) hpl1  -- line length, can be Nothing
        putIOwords ["testing_templatehtml ht1 \n", showT ht1]

        return ht1

    assertEqual (Right zero) res1