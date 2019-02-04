
------------------------------------------------------------------------------
--
-- Module      :   read the setting file
--

-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lib.ReadSettingFile   -- (openMain, htf_thisModuelsTests)
     where

import Uniform.Strings hiding ((</>))
import Uniform.Filenames
--import Uniform.FileStrings
import Uniform.TypedFile
import Lib.Foundation
import Lib.FileMgt
import Lib.Pandoc -- (getMeta)

readSettings :: ErrIO SiteLayout
readSettings = do
    putIOwords ["readSettings start"]
    wd <- return $  makeAbsDir "/home/frank/Workspace8/ssg/site/dough/"
    settingsTxt <- read8 (wd </> makeRelFile "settings") markdownFileType
    putIOwords ["readSettings text", showT settingsTxt]
    layout3 <- readSettings2 True settingsTxt
    putIOwords ["readSettings d", showT layout3]



    layout2 <- return layoutDefaults
    putIOwords ["readSettings end", showT layout2]
    return layout2


readSettings2 :: Bool ->  MarkdownText ->  ErrIO SiteLayout
-- ^ process the settings file
readSettings2 debug (MarkdownText t)  = do
    pandoc :: Pandoc  <- readMarkdown2   t
    let meta2 = flattenMeta (getMeta pandoc)

    let themeDir2 = meta2 ^? key "storage". key "themeDir" . _String :: Maybe Text
    let doughDir2 = meta2 ^? key "storage". key "doughDir" . _String :: Maybe Text
    let bakedDir2 = meta2 ^? key "storage". key "bakedDir" . _String :: Maybe  Text
    let reportFile2 = meta2 ^? key "storage". key "reportFile" . _String :: Maybe  Text
    let testDir2 =  meta2 ^? key "storage". key "testDir" . _String :: Maybe Text

    let layout3 = SiteLayout { themeDir = makeAbsDir . t2s $ fromJustNote "themedir xxdwe" themeDir2
                    , doughDir = makeAbsDir . t2s $  fromJustNote "doughdir xxdwe"  doughDir2
                    , bakedDir = makeAbsDir . t2s $  fromJustNote "bakedir xxdwe"bakedDir2
                    , reportFile = makeAbsFile . t2s $  fromJustNote "testfile xxdwe"  reportFile2
                    ,testDir = makeAbsDir . t2s $  fromJustNote "testdir xxdwe" testDir2
                    }

    putIOwords ["readSettings2", showT layout3]

    return  layout3









