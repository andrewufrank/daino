-----------------------------------------------------------------------------
--
-- Module      :   ssgCheck
-- the main for checking the input files
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
programName = "ssgCheck" :: Text
progTitle = "checking the input files for a static site generator x6 0.0.4.2" :: Text

main :: IO ()
main =
  startProg
    (unwords' [programName, progTitle])
    ( do
        let sitefn = makeAbsDir "/home/frank/Workspace11/ssg/docs/site/dough/settings3"
        checkProcess NoticeLevel0 sitefn
    )
