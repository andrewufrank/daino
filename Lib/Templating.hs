
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

import Uniform.Strings
import Uniform.Filenames
--import Uniform.FileStrings
import Uniform.TypedFile

--import Text.Glabrous (Template, insert) -- , insertMany)
import Text.DocTemplates (applyTemplate)
--import Data.Aeson

import Lib.FileMgt
--import qualified Text.Glabrous as G

-- the final application
applyTemplate3 :: Dtemplate -> DocValue -> ErrIO HTMLout
-- apply the template in the file to the text
--
applyTemplate3  templText val = do
     case applyTemplate (unwrap7 templText)  (unDocValue val) of
                    Left msg -> throwError  . s2t $ msg
                    Right val2 -> return  . HTMLout $  (val2 :: Text)








