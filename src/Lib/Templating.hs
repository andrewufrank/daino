
------------------------------------------------------------------------------
--
-- Module      :   applying a template (using pandoc)

--

-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lib.Templating  -- (openMain, htf_thisModuelsTests)
                      where
import UniformBase
    ( Path,
      Abs,
      Text,
      Dir,
      ErrIO,
      showT,
      makeRelFile,
      when,
      t2s,
      putIOwords,
      Filenames3((</>)),
      AtKey(getAtKey) )
import Uniform.HTMLout (HTMLout(..)
                                                )
import Uniform.Pandoc
    ( Path,
      Abs,
      Text,
      Dir,
      ErrIO,
      showT,
      makeRelFile,
      when,
      t2s,
      putIOwords,
      Filenames3((</>)),
      AtKey(getAtKey),
      Panrep(Panrep),
      HTMLout,
      applyTemplate3 )
import Uniform.Pandoc (Panrep(..))
import Lib.Foundation ( masterTemplateFileName )


putValinMaster :: Bool -> Panrep -> Path Abs Dir -> ErrIO HTMLout
-- ^ get the master html template and put the val into it
-- takes the master filename from val
putValinMaster debug (Panrep val p) templatesP = do
  when debug $ putIOwords ["putValinMaster", "templatesP", showT templatesP]
  let mmt      = getAtKey val "masterTemplate" :: Maybe Text
  let mf = maybe masterTemplateFileName (makeRelFile . t2s) mmt

  let masterfn = templatesP </> mf
--   template <- read8 masterfn dtmplFileType
--   when debug $ putIOwords ["putValinMaster", "template", take' 300 $ showT template]
--   when debug $ putIOwords ["putValinMaster", "val", take' 300 $ showT val]
  html2    <- applyTemplate3 masterfn val  -- inTemplate.html
  when True $ putIOwords ["putValinMaster", showT html2]
  return html2
