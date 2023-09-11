{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports #-}

module Lib.DainoTest_test where

import Test.Framework
import UniformBase
import Foundational.CmdLineFlags
import ShakeBake.StartDainoProcess ( dainoProcess )
-- import Uniform.Test.TestHarness


test_DainoTest = do 
    res1 <- runErr $ do 
        dainoProcess NoticeLevel0 testFlags
            {testFlag = True 
            , testNewFlag = True -- T 
            -- , quickFlag = True   -- q 
            }
    assertEqual (Right zero) res1

