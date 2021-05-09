--------------------------------------------------------------------
--
-- Module      :  the process to check the input files
-- TODO - check for filenames with blanks (specially at end)
----------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports 
            -fno-warn-unused-imports #-}

module Lib.CheckProcess where

import Lib.CheckInput (completeDocRep) -- (getTripleDoc, MetaRec(..))
import Lib.Foundation (SiteLayout (..))
import Lib.ReadSettingFile (readSettings)
import Pipes ((>->))
import qualified Pipes as Pipe
import qualified Pipes.Prelude as PipePrelude
import Uniform.Docrep
-- import Uniform.Markdown (markdownFileType, readMarkdown2docrepJSON)
import UniformBase
import Uniform.Pandoc

checkProcess :: Bool -> FilePath -> ErrIO ()
-- ^ checking all md files
checkProcess debug filepath = do
  let settingsFileName = makeAbsFile filepath
  (layout2, _) <- readSettings settingsFileName
  let doughP = doughDir layout2 -- the regular dough
  putIOwords
    [ "\nstart with \n",
      "settingsFileName",
      showT settingsFileName,
      "\ndoughDir",
      showT doughP,
      "\nfilepath",
      showT filepath
    ]

  fns <- allFilenames3 doughP
  putIOwords ["the filenames\n", showList' . lines' $ fns]

  report <- allMetaRecReport layout2 doughP
  putIOwords ["the report on reading md files\n", report]

  when debug $
    putIOwords
      [ "\n\n*******************************************",
        "all md files checked\n",
        s2t filepath
      ]

tmpResultFile :: Path Abs File
tmpResultFile = makeAbsFile "/home/frank/Workspace11/ssg/docs/site/resfile4checkProcess.txt" :: Path Abs File

allFilenames3 :: Path Abs Dir -> ErrIO Text
allFilenames3 dirname = do
  pipedDoIO tmpResultFile dirname showT
  readFile2 tmpResultFile

--  produce file for reports form getMetaRec
report_metaRec :: SiteLayout -> Path Abs File -> ErrIO String
report_metaRec layout2 f = do
  -- old code:
  -- (_, metaRec, report1) <- getTripleDoc layout f
  -- let report2 = if isNothing report1 then ""
  --                     else concatT ["filex ", s2t $ link metaRec
  --                                 , fromJustNote "report metarec xxweer" report1]
  -- report3 = unlines' .filter (/= "\n") . lines' $ report2
  --replace with
  md1 <- read8 f markdownFileType
  dr1 <- readMarkdown2docrepJSON md1
  let doughP = doughDir layout2 -- the regular dough
      bakedP = bakedDir layout2
  dr2 <- completeDocRep doughP bakedP f dr1
  dr3 <- addRefs False dr2
  let report2 = dr3

  return . show $ report2

allMetaRecReport :: SiteLayout -> Path Abs Dir -> ErrIO Text
allMetaRecReport layout dirname = do
  pipedDoIOwithFilter tmpResultFile dirname (Extension "md") (report_metaRec layout)
  res1 :: Text <- readFile2 tmpResultFile
  -- let res2 =  filter (not . isPrefixOf' "none") . lines' $  res1 :: [Text]
  let res2 = filter (not . null') . lines' $ res1 :: [Text]
  return . unlines' $ res2

-- moved for 1.1 to fileio piped
-- a convenient function to go through a directory and
-- -- recursively apply a function to each file or directory
-- -- filters for extension md
-- pipedDoIOwithFilter :: Path Abs File -> Path Abs Dir -> Extension -> (Path Abs File -> ErrIO String) -> ErrIO ()
-- pipedDoIOwithFilter file path ext opex = do
--   hand <- openFile2handle file WriteMode
--   Pipe.runEffect $
--     getRecursiveContents path
--       >-> PipePrelude.filter (hasExtension ext)
--       >-> PipePrelude.mapM opex
--       >-> PipePrelude.toHandle hand
--   closeFile2 hand
--   return ()
