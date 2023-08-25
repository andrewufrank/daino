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
import ShakeBake.Bake
 
import ShakeBake.Shake2aux


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

    (toFilePath bakedP <> "**/*.html") %> \out -> -- from Panrep
    -- calls the copy html if a html exist in dough 
            -- pushes a need for *.pandoc 

        do
            putInform debug ["rule **/*.html", showT out]

            let outP = makeAbsFile out :: Path Abs File
            let fromFile = doughP </> makeRelativeP bakedP outP
                    -- try to see if the file exists in dough 
            fileExists <- io2bool $ doesFileExist' fromFile
            putInform debug ["rule **/*.html - fileExist:", showT fileExists]
            
            if fileExists 
                then copyFileToBaked debug doughP bakedP out
                else 
                    do 
                        let bakedFrom = replaceExtension'  "panrep" outP
                        putInform debug ["rule **/*.html - bakedFrom", showT bakedFrom]
                        need [toFilePath bakedFrom]

                        needsFound :: [FilePath]<- runErr2action $ 
                             getNeeds4html debug flags bakedFrom sett4 outP
                        need needsFound

                        putInform debug ["\nrule **/*.html continued", showT out]

                        needs2 <- runErr2action $ bakeOnePanrep2html debug flags bakedFrom sett4 outP 
                        putInform debug ["rule **/*.html - needs2", showT needs2]
                        return ()            

    (toFilePath bakedP <> "**/*.panrep") %> \out -> -- insert pdfFIles1
        do 
            putInform debug ["rule **/*.panrep", showT out]

            let outP = makeAbsFile out :: Path Abs File
            
            let bakedFrom = replaceExtension'  "docrep" outP
            putInform debug ["rule **/*.panrep - bakedFrom", showT bakedFrom]
            need [toFilePath bakedFrom]
            fs2 <- getDirectoryFilesP bakedP ["*.docrep"]
            dr2 <- getDirectoryFilesP bakedP ["*/*.docrep"]
            putIOwords ["rule **/*.panrep fs2", showT fs2]
            putIOwords ["rule **/*.panrep dr2", showT dr2]

            -- needsFound :: [FilePath]<- runErr2action $ 
            --         getNeeds4pan debug flags bakedFrom sett4 outP
            -- need needsFound            
            putInform debug ["rule **/*.panrep continued", showT out]

            needs2 <- runErr2action $ bakeOneDocrep2panrep debug flags bakedFrom sett4 outP 
            putInform debug ["rule **/*.panrep - needs2", showT needs2]
            return ()            
            
    (toFilePath bakedP <> "**/*.docrep") %> \out -> -- insert pdfFIles1  -- here start with doughP
        do 
            putInform debug ["rule **/*.docrep", showT out]

            let outP = makeAbsFile out :: Path Abs File
            
            let bakedFrom = replaceDirectoryP  bakedP doughP $  
                                replaceExtension'  "md" outP
            putInform debug ["rule **/*.html - bakedFrom", showT bakedFrom]
            needsFound :: [FilePath]<- runErr2action $ do 
                    getNeeds4doc debug flags bakedFrom sett4 outP
            need needsFound  
            putInform debug ["rule **/*.docrep continued", showT out]

            needs2 <- runErr2action $ bakeOneMD2docrep debug flags bakedFrom sett4 outP 
            putInform debug ["rule **/*.html - needs2", showT needs2]
            return ()
    

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

