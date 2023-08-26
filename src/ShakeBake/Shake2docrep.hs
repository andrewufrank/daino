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


module ShakeBake.Shake2docrep where

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

shake2docrep debug flags sett4 bakedP  =     

    (toFilePath bakedP <> "**/*.docrep") %> \out -> -- insert pdfFIles1  -- here start with doughP
        do 
            putInform debug ["rule **/*.docrep", showT out]

            let outP = makeAbsFile out :: Path Abs File
            let bakedFrom = replaceDirectoryP  bakedP doughP $  
                                replaceExtension'  "md" outP
            -- putInform debug ["rule **/*.docrep - bakedFrom", showT bakedFrom]
            -- needP [bakedFrom]
            
            putInform debug ["rule **/*.docrep continued", showT out]

            needs2 <- runErr2action $ bakeOneMD2docrep debug flags bakedFrom sett4 outP 
            putInform debug ["rule **/*.docrep end - needs2", showT needs2]
            return ()
    

  

