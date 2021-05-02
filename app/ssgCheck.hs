-----------------------------------------------------------------------------
--
-- Module      :   ssgCheck
-- the main for checking the input files
-- for the pages
-- separated to figure it out
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where      -- must have Main (main) or Main where

import UniformBase
import           Uniform.Convenience.StartApp (startProg)
-- import           Uniform.Error  
import           Lib.Foundation (SiteLayout(..))
-- import Lib.CheckProcess

programName, progTitle :: Text
programName = "ssgCheck" :: Text

progTitle = "checking the input files for a static site generator x6 0.0.2.1" :: Text

{- simple approach 
    1. to get all the md files 
    2. check each and produce error message
-}


main :: IO ()
main = startProg
  programName
  progTitle
  (do 
      let 
        flags = True -- the debug flag
        sitefn :: FilePath 
        sitefn = "/home/frank/Workspace8/ssg/docs/site/dough/settings2" 
      checkProcess flags sitefn  
    )



