
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
{-# LANGUAGE DeriveGeneric #-}

module Lib.ReadSettingFile   -- (openMain, htf_thisModuelsTests)
     where

import Uniform.Strings hiding ((</>))
import Uniform.Filenames
--import Uniform.FileStrings
import Uniform.TypedFile
import  Lib.Foundation
import Lib.FileMgt
import Lib.Pandoc -- (getMeta)
--import Text.Pandoc.Definition
import Data.Yaml

readSettings :: ErrIO SiteLayout
readSettings = do
    let debug = False
    when debug $ putIOwords ["readSettings start"]
    wd <- return $  makeAbsDir "/home/frank/Workspace8/ssg/site/dough/"
    settingsTxt <- read8 (wd </> makeRelFile "settings2") yamlFileType
    when debug $ putIOwords ["readSettings text", showT settingsTxt]
    layout3 <- readSettings2 debug settingsTxt
--    putIOwords ["readSettings layout3", showT layout3]
    putIOwords ["readSettings end", showT layout3]
    return layout3


readSettings2 :: Bool ->  YamlText ->  ErrIO SiteLayout
-- ^ process the settings file
readSettings2 debug (YamlText t)  = do
    meta2 :: Value <- decodeThrow   . t2b $ t

    when debug $ putIOwords ["readSettings2 settings", showT meta2]
--    let meta2 = flattenMeta (getMeta pandoc)

--    let themeDir2 = meta2 ^?   key "themeDir" . _String :: Maybe Text
--    when debug $ putIOwords ["readSettings2 themedir", showT themeDir2]

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
--    let layout3 = F.layoutDefaults
    when debug $ putIOwords ["readSettings2", showT layout3 ]

    return  layout3

--instance FromJSON Settings
--
--data Settings = Settings
--    { themeDir :: Text -- ^ the place of the  theme files (includes templates)
--    , doughDir                  -- ^ where the content is originally (includes resources)
--    , bakedDir :: Text -- ^ where all the files serving are
--    , reportFile :: Text  -- ^ the report from processing baked with pipe
--    , testDir :: Text -- ^ the directory the test results go
--                        -- not important
--    } deriving (Show, Generic)









