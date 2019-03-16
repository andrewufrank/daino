
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
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lib.Templating  -- (openMain, htf_thisModuelsTests)
     where

import Uniform.Strings hiding ((</>))
import Uniform.Filenames
--import Uniform.FileStrings
import Uniform.TypedFile

--import Text.Glabrous (Template, insert) -- , insertMany)
import Text.DocTemplates (applyTemplate)
--import Data.Aeson
import Lib.Foundation (masterTemplateFileName)
import Lib.YamlBlocks 
import Lib.FileMgt 
-- import Lib.YamlBlocks (flattenMeta, getMeta, getMaybeStringAtKey
--                 , putStringAtKey, readMarkdown2, unPandocM)

putValinMaster :: Bool -> DocValue -> Path Abs Dir -> ErrIO HTMLout
-- ^ get the master html template and put the val into it
-- takes the master filename from val
putValinMaster debug val templatesP =  do
        let mmt = getMaybeStringAtKey val "masterTemplate" :: Maybe Text
        let mf = maybe (masterTemplateFileName) (makeRelFile . t2s) mmt

        let masterfn = (templatesP </> mf)
        template <- read8 masterfn dtmplFileType
        html2 <-  applyTemplate3 template val  -- inTemplate.html
        when debug $
            putIOwords ["putValinMaster", showT html2]
        return html2

-- the final application
applyTemplate3 :: Dtemplate -> DocValue -> ErrIO HTMLout
-- apply the template in the file to the text
--
applyTemplate3  templText val = do
     case applyTemplate (unwrap7 templText)  (unDocValue val) of
                    Left msg -> throwError  . s2t $ msg
                    Right val2 -> return  . HTMLout $  (val2 :: Text)








