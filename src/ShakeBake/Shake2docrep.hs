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
import Uniform.Shake
import Uniform.Pandoc 
import Foundational.SettingsPage
import Foundational.Filetypes4sites
import Wave.Md2doc 

shake2docrep debug flags sett4 bakedP  =     
    (toFilePath bakedP <> "**/*.docrep") %> \out -> do 
            -- insert pdfFIles1  -- here start with doughP
    putInform debug ["rule **/*.docrep 1", showT out]

    let layout = siteLayout sett4 
        doughP = doughDir layout -- the regular dough
        
    let outP = makeAbsFile out :: Path Abs File
    let bakedFrom = replaceDirectoryP  bakedP doughP $  
                        replaceExtension'  "md" outP
    putInform debug ["rule **/*.docrep 2 - bakedFrom", showT bakedFrom
            , "\n resfn2", showT outP ]
    needP [bakedFrom]  
  
    
    putInform debug ["rule **/*.docrep 3 continued", showT out]

    _ <- runErr2action $ do -- bakeOneMD2docrep debug flags bakedFrom sett4 outP 
    -- no needs follow 
-- bakeOneMD2docrep debug flags inputFn sett3 resfn2 = do
  
--     dr4 <- readMarkdownFile2docrep NoticeLevel0 flags sett4  bakedFrom 
--         -- dr4 <- addRefs debug dr3
--            readMarkdownFile2docrep debug flags sett3 fnin = do

        p1 <- readMd2pandoc bakedFrom -- need posted

       -- check for german and process umlaut, 
        -- repeat readMd2pandoc if changed 

        -- default values only what is used for citeproc and ??
        -- rest can go into settings 
        -- these are copied from previous values (OLD below)

    
        let p2 = addListOfDefaults (metaDefaults sett4) p1
        m1 <- md2Meta_Process p2
        -- process citeproc
        let mp1 = setMetaPlusInitialize sett4 bakedFrom m1
            incl = includeBakeTest3docrep flags (metap mp1) 

        putInform NoticeLevel1 ["rule **/*.docrep 4 "]
        --  showPretty incl]
        let dr4 =  if incl then mp1 else zero     
        write8 outP docrepFileType dr4

        putInform debug  [ "\n-- rule **/*.docrep 5 done resfn2", showT outP
                ]
        return []

    putInform debug ["rule **/*.docrep 5 end"] -- no  - needs2", showT needs2]
    return ()
    


