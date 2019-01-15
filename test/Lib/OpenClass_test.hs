-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lib.OpenClass_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import Uniform.Strings
import Lib.OpenClass


-- show produces the "xx"
test_1 = assertEqual 7 (lengthChar $ op1 d1)
test_2 = assertEqual 6 (lengthChar $ op1 d2)




