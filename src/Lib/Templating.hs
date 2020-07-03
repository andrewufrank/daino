
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

import           Lib.Foundation                 ( masterTemplateFileName )
-- import           Lib.Indexing                   ( getAtKey )
import           Uniform.Filenames
-- import Uniform.DocValue (dtmplFileType,  DocValue(..))
import Uniform.HTMLout (HTMLout(..)
            -- applyTemplate3
                                                )
import           Uniform.Pandoc  
-- import Uniform.HTMLout                
import           Uniform.TypedFile
import Uniform.Pandoc (Panrep(..))
import Lib.Foundation


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
