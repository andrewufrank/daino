
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
    d <- readSettings2 True settingsTxt
    putIOwords ["readSettings d", d]



    layout2 <- return layoutDefaults
    putIOwords ["readSettings end", showT layout2]
    return layout2


readSettings2 :: Bool ->  MarkdownText ->  ErrIO Text
-- ^ process the settings file
readSettings2 debug (MarkdownText t)  = do
    pandoc :: Pandoc  <- readMarkdown2   t
    let meta2 = flattenMeta (getMeta pandoc)

    -- test if biblio is present and apply
    let dough2 = fmap t2s $  ( meta2) ^? key "doughDir" . _String :: Maybe FilePath

    return . showT $ dough2









