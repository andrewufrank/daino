----------------------------------------------------------------------
--
-- Module      :   applying a template (using pandoc)
--
----------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Lib.Templating where -- (openMain, htf_thisModuelsTests)

import Foundational.Foundation  
import Uniform2.HTMLout


--(Abs, AtKey (getAtKey), Dir, ErrIO, Filenames3 ((</>)), HTMLout, Panrep (..), Path, Text, applyTemplate3, makeRelFile, putIOwords, showT, t2s, when)

import Foundational.Filetypes4sites
import Uniform.Json
import UniformBase

putValinMaster :: NoticeLevel -> Panrep -> Path Abs Dir -> ErrIO HTMLout
{- ^ get the master html template and put the val into it
 takes the master filename from val
 not clear what intended
 for now: use the master TODO
-}
putValinMaster debug (Panrep val p) templatesP = do
    when (inform debug) $ putIOwords ["putValinMaster", "templatesP", showT templatesP]
    -- let mmt = getAtKey val "masterTemplate" :: Maybe Text
    -- let mf = maybe masterTemplateFileName (makeRelFile . t2s) mmt
    let mf = masterTemplateFileName
    let masterfn = templatesP </> mf
    --   template <- read8 masterfn dtmplFileType
    --   when (inform debug) $ putIOwords ["putValinMaster", "template", take' 300 $ showT template]
    --   when (inform debug) $ putIOwords ["putValinMaster", "val", take' 300 $ showT val]
    html2 <- applyTemplate3 masterfn val -- inTemplate.html
    when True $ putIOwords ["putValinMaster", showT html2]
    return html2
