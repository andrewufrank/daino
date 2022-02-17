----------------------------------------------------------------------
--
-- Module      :   check the md files 
----------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Lib.CheckProcess where

import UniformBase
import ShakeBake.ReadSettingFile  
import Foundational.LayoutFlags
import System.Directory.Recursive
import Uniform.Pandoc
-- import Foundational.Filetypes4sites  
-- import Wave.Md2doc 
import Foundational.MetaPage 
import Control.Exception
import Control.DeepSeq
import Data.Char

checkProcess :: NoticeLevel -> Path Abs File-> ErrIO ()
{- ^ the top call to check the md files. first collect all the filenames
-}
checkProcess debug sitefn = do
    when (inform debug) $ putIOwords ["checkProcess", "start"]
    sett3 <- readSettings debug (sitefn)
    let doughP = doughDir (storage sett3)
    when (inform debug) $ putIOwords ["checkProcess 1", "doughP", showPretty doughP]

    -- get all md files in doughP 
    fns :: [FilePath] <- callIO $ getDirRecursive (toFilePath doughP) 

    when (inform debug) $ putIOwords ["checkProcess 1", "fns", showT . take 10 $ fns]

    let mds = filter (hasExtension "md") fns 
    -- let mds = filter (hasExtension extMD) fns -- TODO 
    
    when (inform debug) $ putIOwords ["checkProcess 2", "mds", showT . take 10 $ mds]

    let mds1 = filter (notDNB (storage $ sett3)) mds
    when (inform debug) $ putIOwords ["checkProcess 2", "mds1", showT . take 10 $ mds1]
    let mds2 = map makeAbsFile mds1
    -- let hpname = blogAuthorToSuppress.storag sett3

    _ <- mapM (checkOneMD debug doughP ) mds2 

    when (inform debug) $ putIOwords ["checkProcess", "end"]
    return ()



checkOneMD:: NoticeLevel -> Path Abs Dir -> Path Abs File -> ErrIO ()
-- check one md file (only the yaml head) for necessary values 
checkOneMD debug doughP  fnin  =
    
    ( do
        when (informAll debug) $ putIOwords ["checkOneMD fnin", showPretty fnin]

        -- (Docrep y1 _) <- readMarkdownFile2docrep debug doughP fnin
        -- copied from md2doc.hs
        mdfile <- read8 fnin markdownFileType 
        pd <- readMarkdown2 mdfile
        when (inform debug) $ putIOwords ["checkOneMD 1"]
        y1 <- check_readMeta debug doughP fnin  pd 

        -- when (inform debug) $ putIOwords ["checkOneMD 2", "metapage", showPretty  y1]

        when (inform debug) $ putIOwords ["checkOneMD", "done"]
        return ()
    )
    `catchError` (\e -> do
        putIOwords ["checkOneMD", "discovered error in file", showT fnin]
        -- putIOwords ["the yaml head is read as:", showPretty y1]
        putIOwords ["the error msg is:", showT e]
        return () 
        )

check_readMeta:: NoticeLevel -> Path Abs Dir ->  Path Abs File -> Pandoc -> ErrIO  MetaPage
check_readMeta debug doughP fnin  pd = do 
    when (inform debug) $ putIOwords ["check_readMeta 1"]

    let y1 = pandoc2MetaPage doughP fnin  pd
    
    y2 <- liftIO $ do 
        let ll =sum .  map ord .  show $ y1
        -- when (informAll debug) $ putIOwords ["check_readMeta 2", "metapage", showPretty  y1]    
        putStr .   show $ ll  
        let y3 =   deepseq ll y1
    -- output is necessary to force evaluation - deepseq seems not to do it
        return y3

        `catch`(\e -> do 
            putIOwords ["\n\ncheck_readMeta", "discovered error in file", showT fnin]
            -- putIOwords ["the yaml head is read as:", showPretty y1]
            putIOwords ["the error msg is:", showT (e :: SomeException), "\n"]
            return zero 
            
            )
    return y2

