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

import Lib.Foundation (masterTemplateFileName)
import Uniform.HTMLout
  ( HTMLout (..),
  )
import Uniform.Pandoc (Abs, AtKey (getAtKey), Dir, ErrIO, Filenames3 ((</>)), HTMLout, Panrep (..), Path, Text, applyTemplate3, makeRelFile, putIOwords, showT, t2s, when)
import UniformBase
  ( Abs,
    AtKey (getAtKey),
    Dir,
    ErrIO,
    Filenames3 ((</>)),
    Path,
    Text,
    makeRelFile,
    putIOwords,
    showT,
    t2s,
    when,
  )

putValinMaster :: Bool -> Panrep -> Path Abs Dir -> ErrIO HTMLout
-- ^ get the master html template and put the val into it
-- takes the master filename from val
putValinMaster debug (Panrep val p) templatesP = do
  when debug $ putIOwords ["putValinMaster", "templatesP", showT templatesP]
  let mmt = getAtKey val "masterTemplate" :: Maybe Text
  let mf = maybe masterTemplateFileName (makeRelFile . t2s) mmt

  let masterfn = templatesP </> mf
  --   template <- read8 masterfn dtmplFileType
  --   when debug $ putIOwords ["putValinMaster", "template", take' 300 $ showT template]
  --   when debug $ putIOwords ["putValinMaster", "val", take' 300 $ showT val]
  html2 <- applyTemplate3 masterfn val -- inTemplate.html
  when True $ putIOwords ["putValinMaster", showT html2]
  return html2
