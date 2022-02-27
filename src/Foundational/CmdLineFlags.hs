----------------------------------------------------------------------
--
-- Module      : l flags  values from command line
----------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- | the defintion for a layout and a flags type
  which carry info from the command line and the siteHeader file
 the defaults for flags are set up for testing  are overridden
 the defaults for layout must correspond to what is set in the test siteHeader file.
 layout defaults are used in testing

 content dirs are those, which have *.md files
-}
module Foundational.CmdLineFlags
    (module Foundational.CmdLineFlags
    , def ) where

import UniformBase
import Data.Default.Class -- to define a default class for pub flags 
import Foundational.SettingsPage
import Uniform.Json

progName :: Text
progName = "SSG"  



-- | the switches for material to include
-- they are (for now) just bools which allow the 
-- baking of all md (higher than the switch)
data PubFlags = PubFlags
    { privateFlag
      , draftFlag
    --   , oldFlag
      , testFlag
      , testNewFlag 
      , quickFlag
      , watchFlag
      , serverFlag :: Bool
    -- , uploadFlag :: Bool
    -- , settingsFile :: Path Abs File
    }
    deriving (Show, Eq) -- no read for path

instance Zeros PubFlags where
    zero = PubFlags zero zero zero zero zero zero zero
instance Default PubFlags where 
        def = testFlags 

testFlags :: PubFlags
testFlags =
    zero
        { privateFlag = False -- not including draft
        , draftFlag = False
        -- , settingsFile = sourceDirTestSite </> settingsFileName
        }


