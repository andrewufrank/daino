
------------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
module Lib.OpenClass  -- (openMain, htf_thisModuelsTests)
     where

import Uniform.Strings

data Data1 = Data1 {s :: String}
data Data2 = Data2 {f :: Text}
data Data3 = Data3 {i :: Int}

--type family IsA t where
----    op1 :: A t -> String      -- show
----    op2 :: A t -> A t -> A t  -- add
--    IsA (Data1) = String
--    IsA (Data2) = Text

class X p where
    type IsA p
    op1 :: p -> IsA p      -- show
--    op2 :: A t -> A t -> A t  -- add

instance  X (Data1 ) where
    type IsA Data1 = String
    op1 (Data1 s) = show s

instance  X (Data2 ) where
    type IsA Data2 = Text
    op1 (Data2 s) = s2t $ show s

instance  X (Data3) where    -- does compile
    type IsA Data3 = String
    op1 (Data3 s) =   show s

d1 = Data1 "eines"
d2 = Data2 "zwei"
d3 = Data3 3

-- x
