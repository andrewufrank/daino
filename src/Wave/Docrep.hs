---------------------------------------------------------------------
--
-- Module      :  Wave.Docrep
-- the abstract representation of the documents
-- see Filetypes4sites DocrepJSON
------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports 
            -fno-warn-unused-matches #-}

module Wave.Docrep (
    module Wave.Docrep,
    HTMLout,
    htmloutFileType,
) where

-- import Control.Lens (
--     -- needed for the query expressions
--     (^?),
--     -- , (?~)
--     -- , (&)
--     -- , at
--  )
-- import Data.Aeson.Lens (AsValue, key)
-- import Data.Aeson.Types (
--     FromJSON (parseJSON),
--     ToJSON,
--     Value,
--     parseMaybe,
--  )
import UniformBase

-- import Foundational.Foundation 
import Uniform.Json
import Foundational.MetaPage 

import Uniform.Pandoc
-- import Wave.Markdown 
 
import Foundational.Filetypes4sites  
import Uniform2.HTMLout  





-- Docrep
--   { yam = fromJustNote "docRepJSON2docrep not a value" . fromJSONValue $ j,
--     pan = p
--   }





-- mergeAll :: DocrepJSON -> [Value] -> DocrepJSON
-- -- ^ merge the values with the values in DocRec -- last winns
-- -- issue how to collect all css?
-- mergeAll (DocrepJSON y p) vs = DocrepJSON (mergeRightPref $ y : vs) p

-- instance AtKey DocrepJSON Text where
--   getAtKey dr k2 = getAtKey (yam dr) k2

--   putAtKey k2 txt (DocrepJSON y p) = DocrepJSON (putAtKey k2 txt y) p

-- instance AtKey Docrep [Reference] where
--   getAtKey dr k2 =  (yam dr) ^? k2

--   putAtKey k2 b dr = Docrep $ putAtKey k2 b (unDocrep meta2)
