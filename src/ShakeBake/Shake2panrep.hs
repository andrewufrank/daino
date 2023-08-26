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

import ShakeBake.Bake
 
import ShakeBake.Shake2aux
-- import Development.Shake (getDirectoryFilesIO)
import Wave.Panrep2html  

import ShakeBake.Shake2html

shake2panrep debug flags sett4 bakedP =      

    (toFilePath bakedP <> "**/*.panrep") %> \out -> -- insert pdfFIles1
        do 
            let layout = siteLayout sett4 
                doughP = doughDir layout -- the regular dough
                bakedP = bakedDir layout
                
            putInform debug ["rule **/*.panrep", showT out]

            let outP = makeAbsFile out :: Path Abs File
            
            let bakedFrom = replaceExtension'  "docrep" outP

            putInform debug ["rule **/*.panrep - bakedFrom", showT bakedFrom]
            needP [bakedFrom]

            putInform debug ["rule **/*.panrep continued 1", showT out]
        
            let thisDir1 =  getParentDir outP ::FilePath
            
            putInform debug ["rule **/*.panrep - thisDir1", showT thisDir1]

            unless ("/home/frank/bakedTestSite" == thisDir1) $ do 
                -- the case of the webroot is dealt with already  
                -- during initialization
                let thisDir2 = makeAbsDir $ getParentDir outP :: Path Abs Dir
                    thisDir = replaceDirectoryP bakedP doughP  thisDir2
                putInform debug ["rule **/*.panrep - thisDir2", showT thisDir2]
                putInform debug ["rule **/*.panrep - thisDir", showT thisDir]

                fs2 :: [Path Rel File] <- getDirectoryFilesP thisDir ["*.md"]
                dr2 :: [Path Rel File]  <- getDirectoryFilesP thisDir ["index.md"]
                let dr3 = dr2 
                -- filter (not . (isInfixOf' 
                        -- (t2s $ doNotBake (siteLayout sett4))) . getNakedDir) dr2
                        -- problem with get nacked dir 
                        -- not relevant, the webroot is not serched
                -- lint is triggered. perhaps should use the
                -- non traced getDirectoryFilesIO?
                putIOwords ["rule **/*.panrep fs2", showT fs2]
                putIOwords ["rule **/*.panrep dr2", showT dr2]
                putIOwords ["rule **/*.panrep dr3", showT dr3]

                -- needs for the docrep but 
                let needsmd = -- map (replaceDirectoryP bakedP doughP) .
                            map (addFileName thisDir)
                            . map (replaceExtension' "docrep") 
                            $  (fs2 ++ dr3)
                putIOwords ["rule **/*.panrep needsmd", showT needsmd]
                needP needsmd
                      
            putInform debug ["rule **/*.panrep continued 2", showT out]

            needs2 <- runErr2action $ bakeOneDocrep2panrep debug flags bakedFrom sett4 outP 
            putInform debug ["rule **/*.panrep end - needs2", showT needs2]
            need needs2 
            putInform debug ["rule **/*.panrep continued 3 end", showT out]

            -- missed the write panrep?
            return ()            
            
