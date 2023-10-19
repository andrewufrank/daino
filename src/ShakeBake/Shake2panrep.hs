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


module ShakeBake.Shake2panrep where

import UniformBase 
import Foundational.CmdLineFlags
import Uniform.Shake
import Foundational.SettingsPage
import Foundational.Filetypes4sites
import Wave.Md2doc
import ShakeBake.Shake2indexes 

-- import Development.Shake (getDirectoryFilesIO)
-- import qualified Data.Map as M

shake2panrep :: NoticeLevel -> PubFlags -> Settings -> Path Abs Dir -> Rules ()
shake2panrep debug flags sett4 bakedP = 
    (toFilePath bakedP <> "**/*.panrep") %> \out -> do  -- insert pdfFIles1
    -- let debug = NoticeLevel2r
         
    let layout = siteLayout sett4 
        doughP = doughDir layout -- the regular dough
        -- bakedP = bakedDir layout

    putInformOne debug ["rule **/*.panrep 1 start out", showT out]

    let outP = makeAbsFile out :: Path Abs File
    
    let bakedFrom = replaceExtension'  "docrep" outP

    putInformOne debug ["rule **/*.panrep 2 - bakedFrom", showT bakedFrom]
    needP [bakedFrom]

    putInformOne debug ["\nrule **/*.panrep 3 continued", showT out]

    let thisDirP =  makeAbsDir $ getParentDir outP :: Path Abs Dir

    
    putInformOne debug ["rule **/*.panrep 4 - thisDirP", showT thisDirP]

    (fileEnts, dirEnts) <- 
        if not (isIndexPage outP) 
        -- if not (thisDirP /= bakedP && isIndexPage outP) 
        then return (zero, zero)
        else   do -- construct index entries 
    --  here the unless insert 
            (files2, ind3)  <- indexNeeds debug sett4 doughP bakedP outP
                    
            putInformOne debug 
                ["\nrule **/*.panrep 4a \n\t files", showT files2
                    , "\n\t\t index.md for directories", showT ind3]
            needP (files2 ++ ind3)

            fileEnt1 <- mapM (constructFileEnry debug sett4) files2 
            dirEnt1 <- mapM (constructFileEnry debug sett4) ind3 
            -- produces the data for the index.md file
            -- one level
            putInformOne debug 
                ["\nrule **/*.panrep 4b \n fileEntries", showT fileEnt1
                    , "\n dirEntries", showT dirEnt1]

            return (catMaybes fileEnt1, catMaybes dirEnt1)
    
    putInformOne debug ["\nrule **/*.panrep 4x continued after unless" ]

    -- (dirEntries, fileEntries) <- constructIndexEntries
    putInformOne debug ["\nrule **/*.panrep 5 continued 2", showT out]

    needs2empty <- runErr2action $ do
        putInformOne debug [ "\nrule **/*.panrep 6 bakedFrom"
            , showT bakedFrom 
            , "outP", showT outP
            ]
        dr1 :: DainoMetaPlus <- read8 bakedFrom docrepFileType
 
        putInformOne debug ["rule **/*.panrep 7"
                    ]
        let 
            extra6 = metaSetBook sett4 dr1 
            extra7 = extra6{  dirEntries = dirEnts
                            , fileEntries = fileEnts}
        let dr2 = dr1   {
                         extra = extra7 }
        write8 outP panrepFileType dr2 -- set only dirEntries

        putInformOne debug 
                ["rule **/*.panrep 8 done produced resf2n", showT outP
                    , "/n dirEntries", showT dirEnts 
                    -- , "\n needsFound", showT needsFound
                ]
        return [] --  needsFound

    putInformOne debug ["rule **/*.panrep 9 end", showT out]

