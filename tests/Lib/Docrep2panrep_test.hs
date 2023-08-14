{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- {-# OPTIONS??-unused-imports #-}

module Lib.Docrep2panrep_test where

import Foundational.Filetypes4sites
import Foundational.SettingsPage
import Foundational.MetaPage
import Lib.IndexCollect
import Lib.Md2doc_test
import Test.Framework
import Uniform.Json
import Uniform.Markdown
import Uniform.Pandoc
-- import Uniform.Test.TestHarness
import Uniform.Http
import UniformBase
import Wave.Docrep2panrep
import Foundational.CmdLineFlags ( PubFlags )
import Data.Hash

fnmd1 = makeAbsFile "/home/frank/Workspace11/dainoSite/ReadMe/index.md"
resdocrep = makeAbsFile"/home/frank/tests/panrep1"


test_toPanrep = do 
    res1 <- runErr $ do 
        metaplus5 <- setup_md2metaplus fnmd1 

        pr <- docrep2panrep NoticeLevel2 def zero metaplus5
        putIOwords ["test_toPanrep pr \n", showPretty pr]
        let hash1 = show . hash . show $  pr :: String
        return hash1

    assertEqual (Right "Hash {asWord64 = 1964609469859751127}") 
        res1


extra7 = DainoValues ------------------------docrep2panrep end if DainoValues
  { mdFile = "/home/frank/Workspace11/dainoSite/ReadMe/index.md"
  , indexEntry =
      IndexEntry2
        { ixfn = "/home/frank/Workspace11/dainoSite/ReadMe/index.md"
        , link = "ReadMe/index.md"
        , title = ""
        , abstract = ""
        , author = ""
        , date = ""
        , content = ""
        , dirEntries = []
        , fileEntries =
            [ IndexEntry2
                { ixfn = ""
                , link = ""
                , title = ""
                , abstract = ""
                , author = ""
                , date = ""
                , content = ""
                , dirEntries = []
                , fileEntries = []
                , headerShift = 0
                }
            , IndexEntry2
                { ixfn = ""
                , link = ""
                , title = ""
                , abstract = ""
                , author = ""
                , date = ""
                , content = ""
                , dirEntries = []
                , fileEntries = []
                , headerShift = 0
                }
            ]
        , headerShift = 0
        }
  , dainoVersion =
      "Version {versionBranch = [0,1,5,6,3], versionTags = []}"
  , latLanguage = "english"
  , authorReduced = ""
  }  


-- not working ok, because the file names are not corresponding
-- to the path given

-- -- | test to produce pan
-- testing_md2pan f = test1FileIO "daino" 
--     (f <> "T.docrep" ) (f <>"_panrep") 
--     (docrep2panrep NoticeLevel0 (def::PubFlags) (def::SiteLayout) )

-- test_blog1_md2pan = testing_md2pan "01blog1"
-- test_index_md2pan = testing_md2pan "index"
-- test_postwk_md2pan = testing_md2pan "03postwk"
-- test_withRef_md2pan = testing_md2pan "02withRef"


-- testing_pan2indexEntry f = test1File "daino" 
--     (f <> "_panrep" ) (f <>"_ixEntry1") op -- dr1 <- read8 inputFn docrepFileType
-- op :: Panrep -> IndexEntry
-- op = dyIndexEntry . panyam

-- test_pan2indexEntry_index = testing_pan2indexEntry "index"
-- test_pan2indexEntry_blog1 = testing_pan2indexEntry "01blog1"
-- test_pan2indexEntry_postwk = testing_pan2indexEntry "03postwk"
-- test_pan2indexEntry_withRef = testing_pan2indexEntry "02withRef"

-- testing_pan2HTMLout f = test1FileIO "daino" 
--     (f <> "_panrep" ) (f <> "_htmlout") op1 -- dr1 <- read8 inputFn docrepFileType
-- op1 :: Panrep -> ErrIO HTMLout
-- op1 = panrep2html NoticeLevel0 masterfn def 
--     where 
--         mf :: Path Rel File 
--         mf = masterTemplateFile layoutDefaults
--         masterfn :: Path Abs File 
--         masterfn = templatesDir layoutDefaults </> mf

-- test_pan2HTMLout_blog1 = testing_pan2HTMLout "01blog1"
-- test_pan2HTMLout_index = testing_pan2HTMLout "index"
-- test_pan2HTMLout_postwk = testing_pan2HTMLout "03postwk"
-- test_pan2HTMLout_withRef = testing_pan2HTMLout "02withRef"

-- testing_pan2vals f = test1FileIO "daino" 
--     (f <> "_panrep" ) (f <> "_vals1") op2 -- dr1 <- read8 inputFn docrepFileType
-- op2 :: Panrep -> ErrIO [Value]
-- op2 =  panrep2vals NoticeLevel0 ( def) -- static menu

-- test_pan2vals_blog1 = testing_pan2vals "01blog1"
-- test_pan2vals_index = testing_pan2vals "index"
-- test_pan2vals_postwk = testing_pan2vals "03postwk"
-- test_pan2vals_withRef = testing_pan2vals "02withRef"
------------ old -----

-- -- | conversion of markdown file f1 (with extension) to intermediate d11
-- testing_md2dr1 f1 = test1FileIO "daino"  (f1<> ".md") (f1 <> "_dr1" )  (pandoc2docrep NoticeLevel0 doughP bakedP fn2process .  MarkdownText)
--   where
--       fn2process:: Path Abs File
--       fn2process = blogRoot </> (makeRelFile f1)
--       blogRoot = makeAbsDir "/home/frank/Workspace11/daino/docs/site/dough/Blog"
--       -- TODO needs somewhere fix to build to website root
-- test_blog1_md2dr1 = testing_md2dr1 "blog1"
-- test_index_md2dr1 = testing_md2dr1 "index"
-- test_postwk_md2dr1 = testing_md2dr1 "postwk"
-- test_withRef_md2dr1 = testing_md2dr1 "withRef"

-- -- | testing conversion from dr1 to dr3
-- testing_dr12dr3 f1  = test1FileIO "daino" (f1<> "_dr1") (f1 <> "_dr3" ) (addRefs NoticeLevel0   )

-- test_blog1_dr1_dr3 = testing_dr12dr3 "blog1"
-- test_index_dr1_dr3 = testing_dr12dr3  "index"
-- test_postwk_dr1_dr3 = testing_dr12dr3  "postwk"
-- test_withRef_dr1_dr3 = testing_dr12dr3  "withRef"

-- -- | testing dr3 to docrep (check stepwise same result)
-- testing_md2docrep f1= test1FileIO "daino" (f1<> ".md") (f1 <> "T.docrep" ) (md2docrep NoticeLevel0 settings403 fn2process .  MarkdownText)
--   where
--       fn2process:: Path Abs File
--       fn2process = blogRoot </> (makeRelFile f1)
--       blogRoot = makeAbsDir "/home/frank/Workspace11/daino/docs/site/dough/Blog"
--       -- TODO needs somewhere fix to build to website root
-- test_blog1_dm2docrep = testing_md2docrep "blog1"
-- test_index_dm2docrep = testing_md2docrep "index"
-- test_postwk_dm2docrep = testing_md2docrep "postwk"
-- test_withRef_dm2docrep = testing_md2docrep "withRef"


-- instance ShowTestHarness Panrep
-- instance ShowTestHarness HTMLout
-- instance ShowTestHarness [Value]
-- instance ShowTestHarness (Panrep, [FilePath])
