----------------------------------------------------------------------
--
-- Module Shake2 :
----------------------------------------------------------------------
{- the conversion starts with the root files to produce, 
    i.e. only index.md 
    This triggers the rule html -> panrep 
    and panrep2html produces the needs for *.pdf, templates, jpg and bib

    for now the css, dtpl, jpg etc. are still included
    -}
----------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans
            -fno-warn-missing-signatures
            -fno-warn-missing-methods
            -fno-warn-duplicate-exports  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use ++" #-}


module ShakeBake.Shake2 where

import UniformBase 
import Foundational.CmdLineFlags
import Uniform.Shake
 
import Foundational.SettingsPage
import Foundational.Filetypes4sites

import ShakeBake.Bake
 
import ShakeBake.Shake2aux
-- import Development.Shake (getDirectoryFilesIO)
import Wave.Panrep2html  

import ShakeBake.Shake2html
import ShakeBake.Shake2panrep
import ShakeBake.Shake2docrep

type RelFiles = [Path Rel File]

shakeMD ::
    NoticeLevel ->
    Settings ->
    PubFlags ->
    IO ()

shakeMD debug sett4 flags = do 

  let   layout = siteLayout sett4 
        doughP = doughDir layout -- the regular dough
        bakedP = bakedDir layout
    -- themeP = themeDir layout

  shakeArgs2 bakedP $ do

    -- putInform debug [ "\nshakeMD dirs\n"
    --     , "\tbakedP", showT bakedP
    --     , "\n\tdoughP", showT doughP
    --     , "\ndebug", showT debug]
   

    want ["allMarkdownConversion"]

    phony "allMarkdownConversion" $ do
        -- let debug = NoticeLevel0
        -- these are functions to construct the desired results
    
        -- put a link to the themplates folder into dough/resources
        -- otherwise confusion with copying the files from two places

        needPwithoutput "initial" "md" ( mdFiles flags)

    shake2html debug flags sett4 bakedP       
    shake2panrep debug flags sett4 bakedP       
    shake2docrep debug flags sett4 bakedP       


    (toFilePath bakedP <> "/*.css")
        %> \out -> -- insert css -- no subdir
            copyFileToBaked debug doughP bakedP out
    -- (toFilePath bakedP <> "/*.csl")  -- not used with biber TODO 
    --     %> \out -> -- insert css -- no subdir
    --         copyFileToBaked debug doughP bakedP out

    [toFilePath bakedP <> "/*.JPG", toFilePath bakedP <> "/*.jpg"]
    -- seems not to differentiate the JPG and jpg; copies whatever the original 
    -- the html and/or the pdf includegraphics seem to be case sensitive, even for the extension
        |%> \out -> -- insert img files
        -- no subdir (for now)
            copyFileToBaked debug doughP bakedP out

    -- (toFilePath bakedP <> "**/*.bib")
    --     %> \out -> copyFileToBaked debug doughP bakedP out
    -- -- the fonts in a compressed format 
    -- (toFilePath bakedP <> "**/*.woff")
    --     %> \out -> copyFileToBaked debug doughP bakedP out

