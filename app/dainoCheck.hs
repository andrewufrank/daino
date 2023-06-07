-----------------------------------------------------------------------------
--
-- Module      :   dainoCheck
-- the main for checking the input files
-- works always on current dir
-- for the pages
-- separated to identify problems of each page individually
{- simple approach
    1. to get all the md files
    2. check each and produce error message
-}
---------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Lib.CheckProcess (checkProcess)
-- import Uniform.Convenience.StartApp (startProg)
import UniformBase 

programName, progTitle :: Text
programName = "dainoCheck" :: Text
progTitle = "checking the input files for a static site generator 0.0.4.10" :: Text

main :: IO ()
main =
  startProg
    (unwords' [programName, progTitle])
    ( do
        -- let sitefn = makeAbsFile "/home/frank/Workspace11/daino/docs/site/settings3"
        workingdir1 :: Path Abs Dir <- currentDir 
        let sitefn = workingdir1 </> (makeRelFile "settings3" )  
        checkProcess NoticeLevel0 sitefn
    )
