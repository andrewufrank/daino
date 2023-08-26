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


module ShakeBake.Shake2html where

import UniformBase 
import Foundational.CmdLineFlags
import Uniform.Shake
 
import Foundational.SettingsPage
import Foundational.Filetypes4sites

import ShakeBake.Bake
 
import ShakeBake.Shake2aux
import Development.Shake  
import Wave.Panrep2html  

 
shake2html :: NoticeLevel -> PubFlags -> Settings -> Path Abs Dir -> Rules ()
shake2html debug flags sett4 bakedP  =    
    (toFilePath bakedP <> "**/*.html") %> \out -> -- from Panrep
    -- calls the copy html if a html exist in dough 
            -- pushes a need for *.pandoc 

        do
            let layout = siteLayout sett4
                doughP = doughDir layout -- the regular dough

            putInform debug ["rule **/*.html", showT out]
 
            let outP = makeAbsFile out :: Path Abs File
            let fromFile = doughP </> makeRelativeP bakedP outP
                    -- try to see if the file exists in dough 
            fileExists <- io2bool $ doesFileExist' fromFile
            putInform debug ["rule **/*.html - fileExist:", showT fileExists]
            
            if fileExists 
              then copyFileToBaked debug doughP bakedP out
              else do
                let bakedFrom = replaceExtension'  "panrep" outP
                putInform debug ["rule **/*.html - bakedFrom", showT bakedFrom]
                need [toFilePath bakedFrom]

                putInform debug ["\nrule **/*.html continued 1" , showT out]

                needsFound :: [Path Abs File]<- runErr2action $ do
                    dr1 <- read8 bakedFrom panrepFileType
                    let needsFound1 = map (addFileName bakedP . replaceExtension' "html") . getIndexFiles4meta $ dr1 
                    putInform debug ["\nrule **/*.html needsFound1" 
                            , showT needsFound1]
                    return needsFound1
                needP needsFound

                putInform debug ["\nrule **/*.html continued 3", showT out]

                needs2 <- runErr2action $ bakeOnePanrep2html debug flags bakedFrom sett4 outP 
                putInform debug ["rule **/*.html - needs2", showT needs2]
                putInform debug ["\nrule **/*.html end continued 4", showT out]

                return ()            

