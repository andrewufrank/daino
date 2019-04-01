------------------------------------------------------------------------------
--
-- Module      :   read the setting file
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.ReadSettingFile -- (openMain, htf_thisModuelsTests)
                           where

import           Lib.Foundation -- (getMeta)
import           Uniform.Json                   ( Value
                                                , getAt2Key
                                                , getAtKey
                                                )
import           Uniform.Yaml                   ( YamlText(..)
                                                , decodeThrowT
                                                , yamlFileType
                                                )

import           Uniform.Filenames
-- import           Uniform.Strings         hiding ( (</>) )
import           Uniform.TypedFile (read8)

readSettings :: Path Abs File -> ErrIO (SiteLayout, Int)
-- must be the settings2.yaml file, (absolute, fixed before to current dir)
-- which contain the rest of the settings
-- returns layout and port
readSettings settingsfilename = do
    let debug = False
    putIOwords ["readSettings", "file", showT settingsfilename]
    -- wd          <- currentDir
    settingsTxt <- read8 (settingsfilename) yamlFileType
    when debug $ putIOwords ["readSettings text", showT settingsTxt]
      -- TODO where is settings
    layout3 <- readSettings2 debug settingsTxt
    when debug $ putIOwords ["readSettings end", showT layout3]
    return layout3

--    when debug $
--    putIOwords ["readSettings layout3", showT layout3]
readSettings2 :: Bool -> YamlText -> ErrIO (SiteLayout, Int)
-- ^ read the settings file to produce the layout and set the port
readSettings2 debug (YamlText t) = do
    meta2 :: Value <- decodeThrowT t -- decodeThrow . t2b $ t
    when debug $ putIOwords ["readSettings2 settings", showT meta2]
  --    let meta2 = flattenMeta (getMeta pandoc)
  --    let themeDir2 = meta2 ^?   key "themeDir" . _String :: Maybe Text
  --    when debug $ putIOwords ["readSettings2 themedir", showT themeDir2]
    let themeDir2   = getAt2Key meta2 "storage" "themeDir" :: Maybe Text
          --     meta2 ^? key "storage" . key "themeDir" . _String :: Maybe Text
    let doughDir2   = getAt2Key meta2 "storage" "doughDir" :: Maybe Text
    let bakedDir2   = getAt2Key meta2 "storage" "bakedDir" :: Maybe Text
    let reportFile2 = getAt2Key meta2 "storage" "reportFile" :: Maybe Text
    let testDir2    = getAt2Key meta2 "storage" "testDir" :: Maybe Text
    let port        = getAtKey meta2 "localhostPort" :: Maybe Integer
    let
        layout3 = SiteLayout
            { themeDir   = makeAbsDir . t2s $ fromJustNote "themedir xxdwe"
                                                           themeDir2
            , doughDir   = makeAbsDir . t2s $ fromJustNote "doughdir xxdwe"
                                                           doughDir2
            , bakedDir   = makeAbsDir . t2s $ fromJustNote "bakedir xxdwe"
                                                           bakedDir2
            , reportFile = makeAbsFile . t2s $ fromJustNote "testfile xxdwe"
                                                            reportFile2
            , testDir = makeAbsDir . t2s $ fromJustNote "testdir xxdwe" testDir2
            , bannerImage = makeRelFile "cropped-DSC05127-1024x330.jpg"
            , landingPage = makeRelFile "landingPage.html"                  
            }
  --    let layout3 = F.layoutDefaults
    let port2 = fromInteger . fromJustNote "port wrwer" $ port
    when debug $ putIOwords ["readSettings2", showT layout3]
  --    when debug $
    when debug $ putIOwords ["readSettings2 port", showT port2]
    return (layout3, port2)--instance FromJSON Settings
--
--data Settings = Settings
--    { themeDir :: Text -- ^ the place of the  theme files (includes templates)
--    , doughDir                  -- ^ where the content is originally (includes resources)
--    , bakedDir :: Text -- ^ where all the files serving are
--    , reportFile :: Text  -- ^ the report from processing baked with pipe
--    , testDir :: Text -- ^ the directory the test results go
--                        -- not important
--    } deriving (Show, Generic)
