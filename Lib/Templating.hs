
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

import Lib.FileMgt
import Lib.Foundation                 ( masterTemplateFileName )
import Lib.Indexing (getAtKey)


import Uniform.Filenames
import Uniform.Json (getAtKey)
import Uniform.Pandoc (HTMLout(..), readMd2meta)
import Uniform.Strings         hiding ( (</>) )
import Uniform.TypedFile

putValinMaster :: Bool -> DocValue -> Path Abs Dir -> ErrIO HTMLout
-- ^ get the master html template and put the val into it
-- takes the master filename from val
putValinMaster debug val templatesP = do
     let mmt = getAtKey val "masterTemplate" :: Maybe Text
     let mf = maybe masterTemplateFileName (makeRelFile . t2s) mmt

     let masterfn = templatesP </> mf
     template <- read8 masterfn dtmplFileType
     html2    <- applyTemplate3 template val  -- inTemplate.html
     when debug $ putIOwords ["putValinMaster", showT html2]
     return html2









