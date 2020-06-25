------------------------------------------------------------------------------
--
-- Module      :  with Path  the  process to convert
--              files in any input format to html
--              orginals are found in dire doughDir and go to bakeDir
--------------------------------------------------------------------------
{-  die struktur geht von den files aus, die man braucht und 
    diese rekonstruieren die directories wieder, wenn sie kreiert werden.

    wird moeglicherweise ein filetyp (durch extension fixiert) aus zwei quellen produziert so muss dass in der regel fuer die generation
        beruecksichtigt werden 
        (probleme html - entweder durch uebersestzen oder als resource eingestellt
        (problem pfd - dito )
        )   

    ausgeschlossene directories werden durch DNB  markiert
    die files die in diesen gefunden werden, nicht zum umwandeln 
        anzumelden, indem deren namen nicht in "want" eingeschlossen 
        werden.

    pdf werden aus tex erzeugt, die aus texsnip erzeugt werden.
    jedes md ergibt ein texsnip
    jedes texsnip gibt ein tex 
        die indexseiten, die grosse themen zusammenfassen
        produzieren ein tex mit includes fuer die subseiten 
    jedes tex gibt ein pdf 
    das heisst: jedes md gibt ein pdf (auch eingestellte)
    -}
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports 
            -fno-warn-unused-imports 
            #-}

module Lib.Shake2 where

import           Uniform.Error                  ( ErrIO
                                                , callIO
                                                , liftIO
                                                )
import           Uniform.Shake
import           Development.Shake              ( Rules
                                                , (|%>)
                                                )
-- import          Development.Shake.FilePath (replaceExtensions)
import           Uniform.Strings                ( putIOwords
                                                , showT
                                                )
import           Lib.Foundation                 ( SiteLayout(..)
                                                , staticDirName
                                                )
import           Lib.CmdLineArgs                ( PubFlags(..) )
import           Lib.ConvertFiles

shakeDelete :: SiteLayout -> FilePath -> ErrIO ()
-- ^ experimental - twich found delete of md
-- not yet used 

shakeDelete _ filepath = putIOwords
    [ "\n\n*******************************************"
    , "experimental -- twich found  DELETED MD file "
    , s2t filepath
    ]

shakeArgs2 :: Path b t -> Rules () -> IO ()
-- | set the options for shake 
-- called in shakeMD 
shakeArgs2 bakedP = do
  -- putIOwords ["shakeArgs2", "bakedP", s2t . toFilePath $ bakedP]
    res <- shake  -- not shakeArgs, which would include the cmd line args
                 shakeOptions { shakeFiles     = toFilePath bakedP
                              , shakeVerbosity = Chatty -- Loud
                              , shakeLint      = Just LintBasic
                              }
  -- putIOwords ["shakeArgs2", "done"]
    return res

shakeAll :: Bool -> SiteLayout -> PubFlags -> FilePath -> ErrIO ()
-- ^ calls shake in the IO monade. this is in the ErrIO  

shakeAll debug layout flags filepath = do
    -- let debug = False
    --  where the layout is used, rest in shakeWrapped
    putIOwords
        [ "\n\n=====================================shakeAll start"
        , "\n flags"
        , showT flags
        , "caused by"
        , s2t filepath
        , "."
        ]
    let doughP = doughDir layout -- the regular dough
        bakedP = bakedDir layout
    setCurrentDir doughP  -- needed for citeproc in pandoc 
    callIO $ shakeMD debug layout flags doughP bakedP
    -- return ()

shakeMD
    :: Bool
    -> SiteLayout
    -> PubFlags
    -> Path Abs Dir -- dough (source for files)
    -> Path Abs Dir -- baked (target dir for site)
    -> IO ()
-- ^ bake all md files and copy the resources
-- sets the current dir to doughDir
-- copies banner image 
-- in IO
-- TOP shake call 
shakeMD debug layout flags doughP bakedP = shakeArgs2 bakedP $ do
    -- the special filenames which are necessary
    -- because the file types are not automatically 
    -- copied 

    putIOwords
        [ "shakeMD dirs\n"
        , "\tstaticDirName"
        , showT staticDirName
        , "\tbakedP\n"
        , showT bakedP
        ]
    want ["allMarkdownConversion"]

    phony "allMarkdownConversion" $ do
        -- these are functions to construct the desired results
        -- which then produce them
        -- the original start needs in baked (from the files in dough)  
        pdfs  <- getNeeds debug doughP bakedP "md" "pdf"
        htmls      <- getNeeds debug  doughP bakedP "md" "html"

        -- the next should not be put here 
        bibs         <- getNeeds debug doughP bakedP "bib" "bib"
        imgs          <- getNeeds debug doughP bakedP "jpg" "jpg"
        imgs2         <- getNeeds debug doughP bakedP "JPG" "JPG"

        csss          <- getNeeds debug doughP bakedP "css" "css"
                -- templatesP 
                -- (bakedP </> staticDirName) -- exception
        -- mds :: [Path Abs File]  <- getNeeds debug doughP bakedP "md" "md"
        -- pdf2 :: [Path Abs File] <- getNeeds debug doughP bakedP "md" "pdf"
        -- docvals :: [Path Abs File] <-  getNeeds debug doughP bakedP "md" "docval"
        -- given md produce pdf and md files,
        -- but produce the common precursor docval
        -- TODO 

        csls                    <- getNeeds debug doughP bakedP "csl" "csl"
    -- convert to needs (perhaps wants better)
    -- no restriction on order    

        needP pdfs
        needP htmls
        needP bibs
        needP imgs
        needP imgs2
        needP csss
        needP csls
        -- needP mds   -- fuer html
        -- needP pdf2  -- fuer pdf  
        -- needP [bannerImageTarget]
        --  
        -- needP [masterSettings_yaml] -- checks only that file exists
        -- needP  [masterTemplate]
        -- need (map toFilePath yamlPageFiles2)
    return ()

    let debug2 = True

    (toFilePath bakedP <> "**/*.html")
        %> \out
        -- calls the copy html and the conversion from md
                -> convertAny debug bakedP bakedP flags layout out convDocrep2html

    (toFilePath bakedP <> "**/*.pdf")
        %> \out -- insert pdfFIles1  
                -> convertAny debug2 bakedP bakedP flags layout out convTex2pdf

    (toFilePath bakedP <> "**/*.tex")
        %> \out -- insert pdfFIles1  
                -> convertAny debug2 bakedP bakedP flags layout out convTexsnip2tex

    (toFilePath bakedP <> "**/*.texsnip")
        %> \out -- insert pdfFIles1   
                -> convertAny debug2 bakedP bakedP flags layout out convDocrep2texsnip

    (toFilePath bakedP <> "**/*.docrep")
        %> \out -- insert pdfFIles1  -- here start with doughP
                -> convertAny debug2 doughP bakedP flags layout out convMD2docrep

    -- rest are copies 

    (toFilePath (bakedP) <> "/*.md")
        %> \out  -- insert css -- no subdir
                -> copyFileToBaked debug2 doughP bakedP out
    (toFilePath (bakedP) <> "/*.css")
        %> \out  -- insert css -- no subdir
                -> copyFileToBaked debug2 doughP bakedP out
    (toFilePath (bakedP) <> "/*.csl")
        %> \out  -- insert css -- no subdir
                -> copyFileToBaked debug2 doughP bakedP out

    [toFilePath bakedP <> "/*.JPG", toFilePath bakedP <> "/*.jpg"]
        |%> \out -- insert img files 
                                            -- no subdir (for now)
                 -> copyFileToBaked debug2 doughP bakedP out

    (toFilePath bakedP <> "**/*.bib")
        %> \out -> copyFileToBaked debug2 doughP bakedP out

getNeeds
    :: Bool
    -> Path Abs Dir
    -> Path Abs Dir
    -> Text
    -> Text
    -> Action [Path Abs File]
-- ^ find the files which are needed (generic)
--  from source with extension ext
getNeeds debug sourceP targetP extSource extTarget = do
    let sameExt = extSource == extTarget
    when debug $ liftIO $ putIOwords
        [ "===================\ngetNeeds extSource"
        , extSource
        , "extTarget"
        , extSource
        , "sameExt"
        , showT sameExt
        ]

    filesWithSource :: [Path Rel File] <- getDirectoryToBake
        "DNB"
        sourceP
        [("**/*." <> t2s extSource)]
                -- subdirs
    let
        filesWithTarget = if sameExt
            then [ targetP </> c | c <- filesWithSource ]
            else
                map (replaceExtension' extTarget . (targetP </>)) filesWithSource :: [ Path
                      Abs
                      File
                ]
    when debug $ liftIO $ do
        putIOwords
            [ "===================\nbakePDF -  source files 1"
            , "for ext"
            , extSource
            , "files\n"
            , showT filesWithSource
            ]
        putIOwords
            [ "\nbakePDF -  target files 2"
            , "for ext"
            , extTarget
            , "files\n"
            , showT filesWithTarget
            ]
    return filesWithTarget


