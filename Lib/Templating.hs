
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
import Uniform.TypedFile

import Text.Glabrous (Template, insert) -- , insertMany)
import Text.DocTemplates (applyTemplate)
--import Data.Aeson

import Lib.FileMgt
import qualified Text.Glabrous as G

-- handling the glabrous templates gtpl
extGtemplate = Extension "gtpl"

newtype Gtemplate = Gtemplate Text deriving (Show, Read, Eq, Ord)

-- a wrapper around html ready to publish
--unGtemplate (Gtemplate a) = a

gtmplFileType = makeTyped extGtemplate :: TypedFile5 Text Gtemplate

instance Zeros Gtemplate where zero = Gtemplate zero

instance TypedFiles5 Text Gtemplate  where
instance TypedFiles7 Text Gtemplate  where

    wrap7 = Gtemplate
    unwrap7 (Gtemplate a) = a





-- the final application
applyTemplate2 :: Path Abs File -> DocValue -> ErrIO HTMLout
-- apply the template in the file to the text
applyTemplate2 templateFN val = do
     templText <- readFile2 templateFN
     case applyTemplate templText  (unDocValue val) of
                    Left msg -> throwError  . s2t $ msg
                    Right val2 -> return  . HTMLout $  (val2 :: Text)


