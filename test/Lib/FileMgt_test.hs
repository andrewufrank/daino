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
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans #-}

module Lib.Bake_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import Uniform.Strings
import Lib.Bake
import Uniform.Filenames


post1 = makeRelFile "postwk.md" :: Path Rel File

-- show produces the "xx"
test_1 = assertEqual  () ()
--                (do
--                    r <- runErr $ bakeOneFile post1
--                    return (r:: ErrOrVal ())
--                    )



