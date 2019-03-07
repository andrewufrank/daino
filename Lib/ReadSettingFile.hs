
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
import Data.Yaml (decodeThrow)
import Data.Aeson.Lens
import Data.Aeson
import Control.Lens ((^?))

readSettings ::Path Rel File ->  ErrIO (SiteLayout, Int)
-- must be the settings2.yaml file, relative to the current working dir
-- which contain the rest of the settings
-- returns layout and port

readSettings settingsfilename = do
    let debug = False

--    when debug $
    putIOwords ["readSettings", "file", showT settingsfilename]
    wd <- currentDir
    settingsTxt <- read8 (wd </> settingsfilename) yamlFileType
    when debug $ putIOwords ["readSettings text", showT settingsTxt]
    -- TODO where is settings
    layout3 <- readSettings2 debug settingsTxt
--    putIOwords ["readSettings layout3", showT layout3]
    when debug $ putIOwords ["readSettings end", showT layout3]
    return layout3


readSettings2 :: Bool ->  YamlText ->  ErrIO (SiteLayout, Int)
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
    let port =  meta2 ^?   key "localhostPort" . _Integral :: Maybe Integer


    let layout3 = SiteLayout { themeDir = makeAbsDir . t2s $ fromJustNote "themedir xxdwe" themeDir2
                    , doughDir = makeAbsDir . t2s $  fromJustNote "doughdir xxdwe"  doughDir2
                    , bakedDir = makeAbsDir . t2s $  fromJustNote "bakedir xxdwe"bakedDir2
                    , reportFile = makeAbsFile . t2s $  fromJustNote "testfile xxdwe"  reportFile2
                    ,testDir = makeAbsDir . t2s $  fromJustNote "testdir xxdwe" testDir2
                    }
--    let layout3 = F.layoutDefaults
    let port2 = fromInteger . fromJustNote "port wrwer" $ port

    when debug $ putIOwords ["readSettings2", showT layout3 ]
--    when debug $
    when debug $ putIOwords ["readSettings2 port", showT port2 ]

    return  (layout3, port2 )

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









