{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports #-}

module Lib.Template_test where

import Test.Framework

import Foundational.Filetypes4sites
import Foundational.SettingsPage
    -- ( SiteLayout(doughDir, bakedDir, themeDir),
    --   Settings(siteLayout) )
import Foundational.CmdLineFlags
import Foundational.MetaPage
import Wave.Md2doc
import ShakeBake.ReadSettingFile
import Lib.Templating
import Lib.IndexCollect
import Uniform.Pandoc
-- import Data.Aeson
import Uniform.Json
-- import Uniform.Test.TestHarness
import Uniform.MetaPlus hiding (MetaPlus(..), Settings(..), ExtraValues(..))
import Lib.Templating
import UniformBase

-- test_exampleTrue = assertEqual 0 0

settFn = makeAbsFile "/home/frank/Workspace11/daino/settings3.yaml"
fnin = makeAbsFile "/home/frank/Workspace11/dainoSite/ReadMe/index.md"
htmlTest = makeAbsFile "/home/frank/Workspace11/daino/tests/data/metaplusHtml.dtpl"
-- "/home/frank/Workspace11/u4blog/uniform-pandoc/resources/metaplusHtml.dtpl"



test_templatehtml = do 
    res1 <- runErr $ do 
        let debug = NoticeLevel2
        sett3 <- readSettings debug settFn 
        metaplus4 <- readMarkdownFile2docrep debug sett3 fnin
        -- putIOwords ["ttesting_templatehtml metaplus4 \n", showT metaplus4]

        -- docrep2panrep does currenty nothing
        -- from panrep2htm
        htmlTempl  <- compileTemplateFile2 htmlTest

        htm1 <- meta2xx writeHtml5String2 (metap metaplus4)
        let metaplus5 = metaplus4{metaHtml = htm1}

        putIOwords ["testing_templatehtml metaplus5 \n"
                    , showPretty . toJSON $ metaplus5]

        let hpl1 = renderTemplate htmlTempl (toJSON metaplus5)  -- :: Doc Text
        -- putIOwords ["testing_templatehtmltpl1 \n", showT tpl1]
        let ht1 = render (Just 50) hpl1  -- line length, can be Nothing
        putIOwords ["testing_templatehtml ht1 \n", ht1]

        return ht1

    assertEqual (Right zero) res1