----------------------------------------------------------------------
--
-- Module Shake2 docrep
----------------------------------------------------------------------
{- the conversion starts with the root files to produce, 
    i.e. only index.md 
   
    it needs the *.md file 

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
-- import Uniform.Pandoc 
import Foundational.SettingsPage
import Foundational.Filetypes4sites
import Foundational.CmdLineFlags
import Wave.Md2doc 
 
shake2docrep :: NoticeLevel -> PubFlags -> Settings -> Path Abs Dir -> Rules ()
shake2docrep debug flags sett4 bakedP  =     
    (toFilePath bakedP <> "**/*.docrep") %> \out -> do 
            -- insert pdfFIles1  -- here start with doughP
    putInformOne def ["rule **/*.docrep 1", showT out]

    let layout = siteLayout sett4 
        doughP = doughDir layout -- the regular dough
        
    let outP = makeAbsFile out :: Path Abs File
    let bakedFrom = replaceDirectoryP  bakedP doughP $  
                        replaceExtension'  "md" outP
    putInformOne debug ["rule **/*.docrep 2 - bakedFrom", showT bakedFrom
            , "\n\t\t resfn2", showT outP ]
    needP [bakedFrom]  
  
    
    putInformOne debug ["rule **/*.docrep 3 continued - md need set"]

    _ <- runErr2action $ do -- no needs added here 
                    --  (because not certain that included) 

        putInformOne debug ["rule **/*.docrep 4 read", showT bakedFrom]

       -- check for german and process umlaut, 
       --       this is done, even if the file is not include later
        -- repeat readMd2pandoc if changed   
            -- then add list of defaults
            -- walk to replace /lf/ 
            -- apply citeproc
            -- fill body and convert to html and latex
        mp3 <- md2doc debug sett4 bakedFrom 
        putInformOne def  [ "rule **/*.docrep 4a read mp3", showT mp3]  

        let incl = includeBakeTest3docrep flags (metap mp3) 
    
        let dr4 =  if incl then mp3 else zero     
        putInformOne def ["rule **/*.docrep 5 ready to write outP"
                            , showT outP, "incl", showT incl]
        write8 outP docrepFileType dr4

        putInformOne def  [ "rule **/*.docrep 6 written outP", showT outP
                , "dr4\n", showT dr4]
        return []

    putInformOne def ["rule **/*.docrep 7 end ----------------"] -- no  - needs2", showT needs2]
    return ()
    


