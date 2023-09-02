----------------------------------------------------------------------
--
-- Module Shake2 :
----------------------------------------------------------------------
{- the conversion starts with the root files to produce, 
    i.e. only index.md 
    This triggers the rule html -> panrep 
<<<<<<< HEAD
    and Panrep2 produces the needs for *.pdf, templates, jpg and bib
=======
    and panrep2html produces the needs for *.pdf, templates, jpg and bib
>>>>>>> 73f6a93f6bf536704377ab4ef59a887eead704b3

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
 
import Wave.Panrep2pdf
import ShakeBake.Shake2aux

import ShakeBake.Shake2html
import ShakeBake.Shake2latex
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
    shake2latex debug flags sett4 bakedP       
    shake2panrep debug flags sett4 bakedP       
    shake2docrep debug flags sett4 bakedP       

    (toFilePath bakedP <> "**/*.pdf") %> \out -> -- insert pdfFIles1
        do
            when (inform debug) $ putIOwords ["rule **/*.pdf", showT out]
            -- imgs <- getNeeds debug sett4 doughP bakedP "jpg" "jpg"
            -- imgs2 <- getNeeds debug sett4 doughP bakedP "JPG" "JPG"
            -- needP imgs
            -- needP imgs2
            -- why is this here necessary: failed on testSort.pdf?
            -- was ein jpg will ?
            -- TODO improve error from lualatex
            -- when (inform debug) $ putIOwords ["rule **/*.pdf need", showT imgs, showT imgs2]

            let outP = makeAbsFile out :: Path Abs File
            let fromfile = doughP </> makeRelativeP bakedP outP
            putInform debug ["rule **/*.pdf 1 fromFile", showT fromfile]
            fileExists <- io2bool $ doesFileExist' fromfile
            when (inform debug) $ putIOwords ["fileExist:", showT fileExists]
            
            if fileExists 
                then copyFileToBaked debug doughP bakedP out
                else do
                    let targetP = bakedP 
                        sourceP = bakedP 
                        fromfilePath = sourceP </> makeRelativeP targetP outP
                        fromfilePathExt = replaceExtension' 
                            (s2t . unExtension $ extTex) fromfilePath 
                    putInform debug ["rule **/*.pdf 2 fromfilePathExt"
                            , showT fromfilePathExt]
                    

                  
                    -- convertAny debug bakedP bakedP flags sett4 out  "convTex2pdf"
                    -- anyop debug flags fromfilePathExt layout outP
                    runErr2action $ tex2pdf debug fromfilePathExt outP doughP
                    putInform debug ["rule **/*.pdf 3 produce outP (fake)"
                        , showT outP]
            putInform debug ["rule **/*.pdf 4 end"]

-- tex2pdf :: NoticeLevel -> Path Abs File ->  Path Abs File ->  Path Abs Dir ->  ErrIO ()
-- tex2pdf debug fn fnres doughP  =  do


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

    (toFilePath bakedP <> "**/*.bib")
        %> \out -> copyFileToBaked debug doughP bakedP out
    -- the fonts in a compressed format 
    (toFilePath bakedP <> "**/*.woff")
        %> \out -> copyFileToBaked debug doughP bakedP out

