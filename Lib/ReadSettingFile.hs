
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

import Uniform.Strings
import Uniform.Filenames
--import Uniform.FileStrings
import Uniform.TypedFile
import Lib.Foundation
import Lib.FileMgt

readSettings :: ErrIO SiteLayout
readSettings = do
    putIOwords ["readSettings start"]
    layout2 <- return layoutDefaults
    putIOwords ["readSettings end", showT layout2]
    return layout2










