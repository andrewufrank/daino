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

        pr <- docrep2panrep NoticeLevel0 def zero metaplus5
        -- putIOwords ["test_toPanrep pr \n", showPretty pr]
        let hash1 = show . hash . show $  pr :: String
        return hash1

    assertEqual (Right "Hash {asWord64 = 14153968531146367287}") 
        res1


extra7 = DainoValues ------------------------docrep2panrep end if DainoValues
  { mdFile = "/home/frank/Workspace11/dainoSite/ReadMe/index.md"
  , mdRelPath = "ReadMe/index.md"
  , indexEntry =
      IndexEntry2
        { dirEntries = []
        , fileEntries =
            [ "/home/frank/Workspace11/dainoSite/ReadMe/03tree.md"
            , "/home/frank/Workspace11/dainoSite/ReadMe/02alltxt.md"
            ]
        }
  , dainoVersion =
      "Version {versionBranch = [0,1,5,6,3], versionTags = []}"
  , latLanguage = "english"
  , authorReduced = ""
  } 


