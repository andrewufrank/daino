----------------------------------------------------------------------
--
-- Module Shake2 docrep
----------------------------------------------------------------------
{- the conversion starts with the root files to produce, 
    i.e. only index.md 
    This triggers the rule html -> panrep 
    and panrep2html produces the needs for *.pdf, templates, jpg and bib

    for now the css, dtpl, jpg etc. are still included

    docrep includes all pandoc processing
        citeproc for the references
        conversion from markdown to pandoc internal rep
                    to latex and html 
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
import Foundational.CmdLineFlags
import Wave.Md2doc 
-- import Uniform.MetaPlus
-- import ShakeBake.Shake2indexes (fillTextual4MP)

-- shake2docrep :: NoticeLevel ->  PubFlags -> Settings -> Path Abs Dir -> Rules ()
shake2docrep :: NoticeLevel -> PubFlags -> Settings -> Path Abs Dir -> Rules ()
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
            , "\n\t\t resfn2", showT outP ]
    needP [bakedFrom]  
  
    
    putInform debug ["rule **/*.docrep 3 continued - md need set"]

    _ <- runErr2action $ do -- bakeOneMD2docrep debug flags bakedFrom sett4 outP 
    -- no needs ? 

        pandoc1 <- readMd2pandoc bakedFrom -- need posted
        putInform NoticeLevel1 ["rule **/*.docrep 4 read", showT bakedFrom]

       -- check for german and process umlaut, 
        -- repeat readMd2pandoc if changed 

        
        mp3 <- pandoc2metaplus sett4 bakedFrom pandoc1 
    

        let incl = includeBakeTest3docrep flags (metap mp3) 
    
        let dr4 =  if incl then mp3 else zero     
        putInform NoticeLevel1 ["rule **/*.docrep 5 ready to write outP"
                            , showT outP, "incl", showT incl]
        write8 outP docrepFileType dr4

        putInform debug  [ "rule **/*.docrep 6 written outP", showT outP
                ]
        return []

    putInform debug ["rule **/*.docrep 7 end ----------------"] -- no  - needs2", showT needs2]
    return ()
    


