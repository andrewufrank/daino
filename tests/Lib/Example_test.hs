{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports #-}

module Lib.Example_test where

import Test.Framework
-- import UniformBase
-- import Uniform.Test.TestHarness


-- import Data.Text

-- some = "some"
-- text1 = "text1":: Text 

-- two = Data.Text.concat [some, text1]



test_exampleTrue = assertEqual 0 1
