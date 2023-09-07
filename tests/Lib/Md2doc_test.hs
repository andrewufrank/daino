-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- {-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports #-}

module Lib.Md2doc_test 
     where

import Test.Framework
-- import Uniform.Test.TestHarness

import Foundational.SettingsPage
import Foundational.Filetypes4sites
import Wave.Md2doc
import Uniform.Markdown
import Uniform.Pandoc
-- import Uniform.MetaPlus
import UniformBase
-- import Lib.IndexCollect
import ShakeBake.ReadSettingFile
import Data.Hash

setup_md2metaplus settingsFn fn = do 
        let debug = NoticeLevel0   -- avoid_output_fromHere_down
        sett4 <- readSettings debug settingsFn 
        pandoc1 <- readMd2pandoc fn 
        mp3 <- pandoc2metaplus sett4 fn pandoc1

        -- metaplus1 <- readMarkdownFile2docrep debug sett3 fn
        return mp3


settingsLocal = makeAbsFile "/home/frank/Workspace11/daino/tests/data/settingsTest.yaml"

--     "/home/frank/Workspace11/daino/settingsTest.yaml"
-- settingsDainoSite = makeAbsFile "/home/frank/Workspace11/dainoSite/settings3.yaml"


fnmd = makeAbsFile "/home/frank/Workspace11/daino/tests/data/ReadMe/index.md"
resdocrep = makeAbsFile"/home/frank/tests/docrep1"

-- test md2docrep with local settings and local file 
test_md2docrep = do 
    res1 <- runErr $ do 
        dr <- setup_md2metaplus settingsLocal fnmd 
        write8 resdocrep docrepFileType dr
        let hash1 = show . hash . show $  dr :: String
        return hash1

    assertEqual (Right "Hash {asWord64 = 15933632981836880692}") 
        res1

-- test with reference to check citeproc

fn3 = makeAbsFile "/home/frank/Workspace11/daino/tests/data/ReadMe/03tree.md"
res3 = makeAbsFile"/home/frank/tests/docrep3"

test_md3 = do 
    res1 <- runErr $ do 
        dr <- setup_md2metaplus settingsLocal fn3 
        write8 res3 docrepFileType dr
        let hash1 = show . hash . show $  dr :: String
        return hash1

    assertEqual (Right "Hash {asWord64 = 15933632981836880692}") 
        res1
