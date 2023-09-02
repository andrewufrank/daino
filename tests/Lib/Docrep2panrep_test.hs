{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- {-# OPTIONS??-unused-imports #-}

module Lib.Docrep2panrep_test where

import Foundational.Filetypes4sites
import Foundational.SettingsPage
import Lib.IndexCollect
<<<<<<< HEAD
-- import Lib.Md2doc_test
=======
import Lib.Md2doc_test
>>>>>>> 73f6a93f6bf536704377ab4ef59a887eead704b3
import Test.Framework
import Uniform.Json
import Uniform.Markdown
import Uniform.Pandoc
-- import Uniform.Test.TestHarness
import Uniform.Http
import UniformBase
-- import Wave.Docrep2panrep
import Foundational.CmdLineFlags ( PubFlags (PubFlags) )
import Data.Hash

-- fnmd1 = makeAbsFile "/home/frank/Workspace11/daino/tests/data/ReadMe/index.md"
fnmd2 = makeAbsFile "/home/frank/Workspace11/dainoSite/ReadMe/index.md"
resdocrep = makeAbsFile "/home/frank/tests/docrep1"
respanrep = makeAbsFile "/home/frank/tests/panrep1"

-- settingsDainoSite = makeAbsFile "/home/frank/Workspace11/dainoSite/settings3.yaml"
-- test regular docrep2panrep with files from dainoSite
-- test_toPanrep = do 
--     res1 <- runErr $ do 
--         metaplus5 <- setup_md2metaplus settingsDainoSite fnmd2 

--         pr@(m1,n1) <- docrep2panrep NoticeLevel0 (def::PubFlags) metaplus5
--         putIOwords ["test_toPanrep pr \n", showPretty  $ m1]
--         putIOwords ["test_toPanrep extra \n", showPretty . extra $ m1]
--         write8 respanrep panrepFileType  m1
--         let hash1 = show . hash . show $  pr :: String
--         return hash1

--     assertEqual (Right "Hash {asWord64 = 4759121937026287653}") 
--         res1


-- test_toPanrep extra 
--  DainoValues
--   { mdFile = "/home/frank/Workspace11/dainoSite/ReadMe/index.md"
--   , mdRelPath = "ReadMe/index.md"
--   , dirEntries = []
--   , fileEntries =
--       [ IndexEntry2
--           { ixfn = "/home/frank/Workspace11/dainoSite/ReadMe/03tree.md"
--           , link = "ReadMe/03tree.md"
--           , title = ""
--           , abstract = ""
--           , author = ""
--           , date = ""
--           , content = ""
--           , visibility = ""
--           , version = ""
--           , sortOrder = ""
--           , headerShift = 0
--           }
--       , IndexEntry2
--           { ixfn = "/home/frank/Workspace11/dainoSite/ReadMe/02alltxt.md"
--           , link = "ReadMe/02alltxt.md"
--           , title = ""
--           , abstract = ""
--           , author = ""
--           , date = ""
--           , content = ""
--           , visibility = ""
--           , version = ""
--           , sortOrder = ""
--           , headerShift = 0
--           }
--       ]
--   , dainoVersion =
--       "Version {versionBranch = [0,1,5,6,3], versionTags = []}"
--   , latLanguage = "english"
--   , authorReduced = ""
--   }