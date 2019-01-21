
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
import Uniform.FileStrings
import Text.Pandoc.Templates (applyTemplate)
--import Data.Aeson

import Lib.FileMgt




applyTemplate2 :: Path Abs File -> DocValue -> ErrIO HTMLout
-- apply the template in the file to the text
applyTemplate2 templateFN val = do
     templText <- readFile2 templateFN
     case applyTemplate templText  (unDocValue val) of
                    Left msg -> throwError  . s2t $ msg
                    Right val2 -> return  . HTMLout $  (val2 :: Text)


