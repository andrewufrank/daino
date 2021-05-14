--------------------------------------------------------------------
--
-- Module      :  the process to check the input files
-- TODO - check for filenames with blanks (specially at end)
-- questionable: needed - used ? TODO
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

import Foundational.Foundation (SiteLayout (..))
import ShakeBake.ReadSettingFile (readSettings)

import Uniform.Pandoc (markdownFileType)
import Wave.Docrep
import Wave.Markdown

import Foundational.Filetypes4sites (Docrep)
import UniformBase

checkProcess :: Bool -> FilePath -> ErrIO ()
-- ^ checking all md files
checkProcess debug filepath = do
    let settingsFileName = makeAbsFile filepath
    (layout2, _) <- readSettings debug settingsFileName
    let doughP = doughDir layout2 -- the regular dough
    when debug $
        putIOwords
            [ "\nstart with \n"
            , "settingsFileName"
            , showT settingsFileName
            , "\ndoughDir"
            , showT doughP
            , "\nfilepath"
            , showT filepath
            ]

    fns <- allFilenames3 doughP
    putIOwords ["the filenames\n", showList' . lines' $ fns]

    report <- allMetaRecReport layout2 doughP
    when debug $ putIOwords ["the report on reading md files\n", report]

    when debug $
        putIOwords
            [ "\n\n*******************************************"
            , "all md files checked\n"
            , s2t filepath
            ]

tmpResultFile :: Path Abs File
tmpResultFile = makeAbsFile "/home/frank/Workspace11/ssg/docs/site/resfile4checkProcess.txt" :: Path Abs File

allFilenames3 :: Path Abs Dir -> ErrIO Text
allFilenames3 dirname = do
    pipedDoIO tmpResultFile dirname showT
    readFile2 tmpResultFile

--  | produce file for reports form getMetaRec
report_metaRec :: SiteLayout -> Path Abs File -> ErrIO String
report_metaRec layout2 inputFn = do
    md1 <- read8 inputFn markdownFileType
    dr3 :: Docrep <- md2docrep False layout2 inputFn md1
    return . show $ dr3

allMetaRecReport :: SiteLayout -> Path Abs Dir -> ErrIO Text
allMetaRecReport layout dirname = do
    pipedDoIOwithFilter tmpResultFile dirname (Extension "md") (report_metaRec layout)
    res1 :: Text <- readFile2 tmpResultFile
    -- let res2 =  filter (not . isPrefixOf' "none") . lines' $  res1 :: [Text]
    let res2 = filter (not . null') . lines' $ res1 :: [Text]
    return . unlines' $ res2
