---------------------------------------------------------------------
--
-- Module      :   read the setting file

----------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module ShakeBake.ReadSettingFile where  

import Foundational.Foundation
import Uniform.Json
import Uniform.Yaml
import UniformBase

readSettings :: NoticeLevel -> Path Abs File -> ErrIO (SiteLayout, Int)

{- | must be the settingsNN.yaml file, (absolute, fixed before to current dir)
 which contain the rest of the settings
 returns layout and port
-}
readSettings debug settingsfilename =
    do
        when (inform debug) $
            putIOwords
                [ "readSettings"
                , "file"
                , showPretty settingsfilename
                ]
        -- wd          <- currentDir
        settingsTxt <- read8 settingsfilename yamlFileType
        when (inform debug) $ putIOwords ["readSettings text", showPretty settingsTxt]
        -- TODO where is settings
        layout3 <- readSettings2 debug settingsTxt
        when (inform debug) $ putIOwords ["readSettings end", showPretty layout3]
        when (informall debug) $
            putIOwords ["readSettings layout3", showPretty layout3]
        return layout3

readSettings2 :: NoticeLevel -> YamlText -> ErrIO (SiteLayout, Int)
-- ^ read the settings file to produce the layout and set the port
readSettings2 debug (YamlText t) = do
    meta2 :: Value <- decodeThrowT t -- decodeThrow . t2b $ t
    when (inform debug) $ putIOwords ["readSettings2 settings", showPretty meta2]
 
    let themeDir2 = getAt2Key meta2 "storage" "themeDir" :: Maybe Text
    --     meta2 ^? key "storage" . key "themeDir" . _String :: Maybe Text
    let doughDir2 = getAt2Key meta2 "storage" "doughDir" :: Maybe Text
    let bakedDir2 = getAt2Key meta2 "storage" "bakedDir" :: Maybe Text
    let reportFile2 = getAt2Key meta2 "storage" "reportFile" :: Maybe Text
    let masterFile2 = getAt2Key meta2 "storage" "masterTemplateFile" :: Maybe Text
    let testDir2 = getAt2Key meta2 "storage" "testDir" :: Maybe Text
    let port = getAtKey meta2 "localhostPort" :: Maybe Integer
    let layout3 =
            SiteLayout
                { themeDir =
                    makeAbsDir . t2s $
                        fromJustNote
                            "themedir xxdwe"
                            themeDir2
                , doughDir =
                    makeAbsDir . t2s $
                        fromJustNote
                            "doughdir xxdwe"
                            doughDir2
                , bakedDir =
                    makeAbsDir . t2s $
                        fromJustNote
                            "bakedir xxdwe"
                            bakedDir2
                , reportFile =
                    makeAbsFile . t2s $
                        fromJustNote
                            "testfile xxdwe"
                            reportFile2
                , masterTemplateFile = 
                    makeRelFile . t2s $ 
                        fromJustNote "masterTemplateFile xxdwe" masterFile2
                , testDir = makeAbsDir . t2s $ fromJustNote "testdir xxdwe" testDir2
                 -- , bannerImage = makeRelFile "cropped-DSC05127-1024x330.jpg"
                  -- , landingPage = makeRelFile "index.html"
                --   uploadServer = uploadServerTest
                }
    --    let layout3 = F.layoutDefaults
    let port2 = fromInteger . fromJustNote "port wrwer" $ port
    when (inform debug) $ putIOwords ["readSettings2", showPretty layout3]
    --    when (inform debug) $
    when (inform debug) $ putIOwords ["readSettings2 port", showPretty port2]
    return (layout3, port2) --instance FromJSON Settings
