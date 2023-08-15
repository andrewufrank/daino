{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- {-# OPTIONS??-unused-imports #-}

module Lib.Docrep2panrep_test where

import Foundational.Filetypes4sites
import Foundational.SettingsPage
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

        pr@(m1,n1) <- docrep2panrep NoticeLevel0 def zero metaplus5
        putIOwords ["test_toPanrep pr \n", showPretty  $ m1]
        putIOwords ["test_toPanrep extra \n", showPretty . extra $ m1]
        let hash1 = show . hash . show $  pr :: String
        return hash1

    assertEqual (Right "Hash {asWord64 = 13387489375694972454}") 
        res1


extra7 =  DainoValues
  { mdFile = "/home/frank/Workspace11/dainoSite/ReadMe/index.md"
  , mdRelPath = "ReadMe/index.md"
  , dirEntries = []
  , fileEntries =
      [ IndexEntry2
          { ixfn = "/home/frank/Workspace11/dainoSite/ReadMe/03tree.md"
          , link = ""
          , title = ""
          , abstract = ""
          , author = ""
          , date = ""
          , content = ""
          , publish = Nothing
          , headerShift = 0
          }
      , IndexEntry2
          { ixfn = "/home/frank/Workspace11/dainoSite/ReadMe/02alltxt.md"
          , link = ""
          , title = ""
          , abstract = ""
          , author = ""
          , date = ""
          , content = ""
          , publish = Nothing
          , headerShift = 0
          }
      ]
  , dainoVersion =
      "Version {versionBranch = [0,1,5,6,3], versionTags = []}"
  , latLanguage = "english"
  , authorReduced = ""
  }
