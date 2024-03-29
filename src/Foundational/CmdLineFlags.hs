----------------------------------------------------------------------
--
-- Module      : l flags  values from command line
----------------------------------------------------------------------
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}
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
import Uniform.Shake ( Text, File, Path, Rel, Zeros(zero) )
import Data.Default.Class ( Default(..) )
import UniformBase ( Text, Zeros(zero) ) -- to define a default class for pub flags 

progName, progTitle :: Text
progName = "daino"  
progTitle = "constructing a static site generator" :: Text



-- | the flags represent the switches from CmdLineArgs
-- see there for meaning
data PubFlags = PubFlags
    { privateFlag
      , draftFlag
    --   , oldFlag
      , testFlag
      , testNewFlag 
      , restartFlag
      , quickFlag
      , watchFlag
      , serverFlag
      , verboseFlag :: Bool
      , locationDir :: FilePath -- can be absolute or relative
                    -- the location given on the command line
      , mdFiles :: [Path Rel File]
        -- experiment to calculate the list of all md files to include
    }
    deriving (Show, Eq) -- no read for path

instance Zeros PubFlags where
    zero = PubFlags zero zero zero zero zero zero zero zero zero zero zero
instance Default PubFlags where 
        def = testFlags 

testFlags :: PubFlags
testFlags =
    zero
        { privateFlag = False -- not including draft
        , draftFlag = False
        , verboseFlag = False  -- sets debug
        }


