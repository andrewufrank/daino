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

import Foundational.SettingsPage
-- import Uniform.Json
import Uniform.Yaml
import UniformBase

readSettings :: NoticeLevel -> Path Abs File -> ErrIO  Settings

{- | must be the settingsNN.yaml file, (absolute, fixed before to current dir)
 which contain the rest of the siteHeader
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
        sett3 :: Settings <- readYaml2rec settingsfilename 
        when (inform debug) $ putIOwords ["settings read", showT sett3]
        return sett3 

