------------------------------------------------------------------------------
--
-- Module      :  the process to check the input files 
-- TODO - check for filenames with blanks (specially at end)
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes, Trustworthy #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports 
            -fno-warn-unused-imports 
            #-}
            
module Lib.CheckProcess where

import           Uniform.Error (ErrIO) -- , callIO, liftIO)
import           Uniform.Strings (putIOwords, showT)
import           Lib.Foundation (SiteLayout(..))
import Lib.ReadSettingFile (readSettings)
import Uniform.Filenames hiding (handle)
import Uniform.Piped (pipedDoIO)
import Uniform.FileStrings (readFile2)
import Lib.CheckInput (getTripleDoc, MetaRec(..))
-- import Lib.Indexing(getMetaRec)
-- import Lib.Foundation (progName, SiteLayout (..), layoutDefaults)
import Uniform.FileStrings (openFile2handle, closeFile2, IOMode(..))
import qualified Pipes as Pipe
import  Pipes ((>->))
import Uniform.Piped (getRecursiveContents)
import qualified Pipes.Prelude as PipePrelude
-- import qualified System.IO as IO

checkProcess :: Bool -> FilePath  -> ErrIO ()
-- ^ checking all md files 

checkProcess debug filepath = do 
    let settingsFileName =  makeAbsFile filepath  
    (layout2, _) <- readSettings settingsFileName
    let doughP = doughDir layout2 -- the regular dough
    putIOwords ["\nstart with \n", "settingsFileName", showT settingsFileName 
                , "\ndoughDir", showT doughP
                , "\nfilepath", showT filepath]

    fns <- allFilenames3 doughP
    putIOwords ["the filenames\n", showList' . lines' $ fns]

    report <- allMetaRecReport layout2 doughP 
    putIOwords ["the report on reading md files\n", report ]

    when debug $ putIOwords
        [ "\n\n*******************************************"
        , "all md files checked\n"
        , s2t filepath]

tmpResultFile = makeAbsFile "/home/frank/Workspace8/ssg/docs/site/resfile4checkProcess.txt" :: Path Abs File

allFilenames3 :: Path Abs Dir -> ErrIO (Text) 
allFilenames3 dirname = do 
        pipedDoIO tmpResultFile dirname showT
        readFile2 tmpResultFile 

--  produce file for reports form getMetaRec
report_metaRec :: SiteLayout -> Path Abs File -> ErrIO String
report_metaRec layout f = do 
    (_, metaRec, report1) <- getTripleDoc layout f
    let report2 = if isNothing report1 then ""  
                        else concatT ["filex ", s2t $ relURL metaRec
                                    , fromJustNote "report metarec xxweer" report1]
        -- report3 = unlines' .filter (/= "\n") . lines' $ report2
    return . t2s  $ report2

allMetaRecReport :: SiteLayout -> Path Abs Dir -> ErrIO Text
allMetaRecReport layout dirname = do 
    pipedDoIOwithFilter tmpResultFile dirname (Extension "md") (report_metaRec layout)
    res1 :: Text <- readFile2 tmpResultFile  
    -- let res2 =  filter (not . isPrefixOf' "none") . lines' $  res1 :: [Text]
    let res2 =  filter (not . null') . lines' $  res1 :: [Text]
    return . unlines' $ res2        

-- a convenient function to go through a directory and 
-- recursively apply a function to each file or directory
-- filters for extension md
pipedDoIOwithFilter :: Path Abs File -> Path Abs Dir -> Extension -> (Path Abs File -> ErrIO String) -> ErrIO ()
pipedDoIOwithFilter file path ext opex =  do
    hand <-   openFile2handle file WriteMode
    Pipe.runEffect $
                getRecursiveContents path
                >-> PipePrelude.filter (hasExtension ext)
                >-> PipePrelude.mapM opex 
                >-> PipePrelude.toHandle hand    
    closeFile2 hand
    return ()

