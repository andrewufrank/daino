-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- {-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports #-}

module Lib.Panrep2html_test 
     where

import Test.Framework
-- import Uniform.Test.TestHarness

import Foundational.SettingsPage
import Foundational.Filetypes4sites
import Wave.Md2doc
-- import Wave.Docrep2panrep
import Wave.Panrep2pdf
import Uniform.Pandoc
-- import Uniform.Json
import UniformBase
import Lib.Md2doc_test
import Lib.Docrep2panrep_test
import Uniform.Http ( HTMLout (HTMLout), unHTMLout, htmloutFileType ) 
import Data.Hash
import Wave.Panrep2html
import Foundational.CmdLineFlags
-- import Lib.IndexCollect

settFn = makeAbsFile 
    "/home/frank/Workspace11/daino/settingsTest.yaml"
fnmd2a = makeAbsFile "/home/frank/Workspace11/dainoSite/ReadMe/index.md"

 
reshtmlout = makeAbsFile "/home/frank/tests/htmlout"
-- test regular processing
-- test_toHtmlout = do 
--     res1 <- runErr $ do 
--         metaplus5 <- setup_md2metaplus settingsDainoSite fnmd2a 
--             -- let debug = NoticeLevel0
--             -- sett3 <- readSettings debug settFn 
--                     -- this is a particular settingsTest.yaml
--             -- metaplus1 <- readMarkdownFile2docrep debug sett3 fn
--         (metap1,_) <- docrep2panrep NoticeLevel0 
--                         (def::PubFlags)  metaplus5
--         (html1, _, tt1) <- panrep2html NoticeLevel0 def metap1
--         -- putIOwords ["test_toHtmlTest pr \n", unHTMLout html1]
--         write8 reshtmlout htmloutFileType html1
--         write8 reshtmlout tthFileType tt1
--         let hash1 = show . hash . show $  html1 :: String
--         return hash1

--     assertEqual (Right "Hash {asWord64 = 14632867472327410945}") 
--         res1


testDir = makeAbsDir $
                ("/home/frank" :: FilePath)
                    </> ("tests" :: FilePath)
                    -- must correspond to what testharness is using
                    -- does not

fnmd1 = makeAbsFile "/home/frank/Workspace11/daino/tests/data/ReadMe/index.md"
reshtml = makeAbsFile"/home/frank/tests/htmlTest"
testTemplate = makeAbsFile "/home/frank/Workspace11/daino/tests/data/metaplusHtml.dtpl"
