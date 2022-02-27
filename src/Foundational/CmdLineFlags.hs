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

settingsFileName :: Path Rel File
-- ^ the yaml file in which the siteHeader are fixec
settingsFileName = makeRelFile "settings3" -- the yaml file
-- -- the value for cannot go into layout as this is its name!
-- is then set in flags
-- testNew bakes all test data, test alone continue the previous test


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
    , settingsFile :: Path Abs File
    }
    deriving (Show, Eq) -- no read for path

instance Zeros PubFlags where
    zero = PubFlags zero zero zero zero zero zero zero zero 
instance Default PubFlags where 
        def = testFlags 

testFlags :: PubFlags
testFlags =
    zero
        { privateFlag = False -- not including draft
        , draftFlag = False
        , settingsFile = sourceDirTestSite </> settingsFileName
        }

blankAuthorName :: [Text] -> Text -> Text 
-- suppress/oppress author name, if the author name is the same as one in the first arg (AUF, Andrew U..) then set it to empty else copy 
-- idea is to avoid to have each page say the obvious "author XX"
blankAuthorName names current = 
    if current `elem` names 
        then zero 
        else current 

