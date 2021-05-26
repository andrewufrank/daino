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

import Foundational.LayoutFlags
import Uniform2.HTMLout

--(Abs, AtKey (getAtKey), Dir, ErrIO, Filenames3 ((</>)), HTMLout, Panrep (..), Path, Text, applyTemplate3, makeRelFile, putIOwords, showT, t2s, when)

import Foundational.Filetypes4sites
import Uniform.Json
import Uniform.PandocHTMLwriter
import UniformBase

putValinMaster :: NoticeLevel -> [Value] -> Path Abs Dir -> ErrIO HTMLout
{- ^ get the master html template and put the val into it
 takes the master filename from val
 not clear what intended
 for now: use the master TODO
-}
putValinMaster debug vals templatesP = do
    when (inform debug) $ putIOwords ["putValinMaster", "templatesP", showT templatesP]
    let mf = masterTemplateFileName
    let masterfn = templatesP </> mf
    template2 :: Text <- readFile2 (toFilePath masterfn)
    -- templatapplyTemplate3 debug masterfn vals -- inTemplate.html
    html2 <- applyTemplate4 (inform debug) template2 vals -- inTemplate.html
    -- when (inform debug) $ putIOwords ["putValinMaster", showT html2]
    return . HTMLout $ html2
