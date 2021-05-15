----------------------------------------------------------------------
--
-- Module Shake2 :     
----------------------------------------------------------------------
{-  die struktur geht von den files aus, die man braucht und
    diese rekonstruieren die directories wieder, wenn sie kreiert werden.

    the start is with /getNeeds/ in phony:
        - md produces pdf
            - with convTex2pdf, calling
            - write2pdf (runs lualatex)
        - md produces html
            - with convPanrep2html, calling
            - bakeOneFile2panrep
            - docrep2panrep

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
----------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports  #-}

{- | to convert
              files in any input format to html and pdf
              orginals are found in doughDir and go to bakeDir
-}
module ShakeBake.Shake2 where

import Uniform.Shake
    
import Lib.CmdLineArgs (PubFlags (..))
import ShakeBake.ConvertFiles
import Foundational.Foundation 

-- shakeDelete :: SiteLayout -> FilePath -> ErrIO ()
-- {- ^ experimental - twich found delete of md
--  not yet used
-- -}
-- shakeDelete _ filepath =
--     putIOwords
--         [ "\n\n*******************************************"
--         , "experimental -- twich found  DELETED MD file "
--         , s2t filepath
--         ]

shakeArgs2 :: Path b t -> Rules () -> IO ()

{- | set the options for shake
 called in shakeMD
-}
shakeArgs2 bakedP = do
    -- putIOwords ["shakeArgs2", "bakedP", s2t . toFilePath $ bakedP]
    res <-
        shake  
            shakeOptions
                { shakeFiles = toFilePath bakedP
                , shakeVerbosity = Verbose-- Loud
                , shakeLint = Just LintBasic
                }
    -- putIOwords ["shakeArgs2", "done"]
    return res

shakeAll :: NoticeLevel -> SiteLayout -> PubFlags -> FilePath -> ErrIO ()
-- ^ calls shake in the IO monade. this is in the ErrIO
shakeAll debug layout flags filepath = do
    putIOwords
        [ "\n\n===================================== shakeAll start"
        , "\n flags"
        , showPretty flags
        , "\ncaused by"
        , s2t filepath
        , "."
        , "\n======================================="
        ]
    let doughP = doughDir layout -- the regular dough
        bakedP = bakedDir layout
    callIO $ shakeMD debug layout flags doughP bakedP


shakeMD ::
    NoticeLevel ->
    SiteLayout ->
    PubFlags ->
    Path Abs Dir -> -- dough (source for files)
    Path Abs Dir -> -- baked (target dir for site)
    IO ()
{- ^ bake all md files and copy the resources
 from each md produce:
 -
 sets the current dir to doughDir
 copies banner image
 in IO
 TOP shake call
-}
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
        pdfs <- getNeeds debug doughP bakedP "md" "pdf"
        htmls <- getNeeds debug doughP bakedP "md" "html"



        needP pdfs
        needP htmls

    -- (toFilePath bakedP <> "**/*.html") %> \out -> -- from Panrep
    -- -- calls the copy html and the conversion from md
    --     do
    --         csss <- getNeeds debug doughP bakedP "css" "css"
    --         when (inform debug) $ putIOwords ["rule **/*.html need", showT csss]
    --         imgs <- getNeeds debug doughP bakedP "jpg" "jpg"
    --         imgs2 <- getNeeds debug doughP bakedP "JPG" "JPG"
    --         needP imgs
    --         needP imgs2
    --         when (inform debug) $ putIOwords ["rule **/*.html need", showPretty imgs, showPretty imgs2]
    --         convertAny debug bakedP bakedP flags layout out convPanrep2html "convPanrep2html"

    (toFilePath bakedP <> "**/*.html") %> \out -> -- from Panrep
    -- calls the copy html and the conversion from md
        do
            csss <- getNeeds debug doughP bakedP "css" "css"
            when (inform debug) $ putIOwords ["rule **/*.html need", showT csss]
            imgs <- getNeeds debug doughP bakedP "jpg" "jpg"
            imgs2 <- getNeeds debug doughP bakedP "JPG" "JPG"
            needP imgs
            needP imgs2
            when (inform debug) $ putIOwords ["rule **/*.html need", showPretty imgs, showPretty imgs2]
            convertAny debug bakedP bakedP flags layout out convPanrep12html "convPanrep12html"

    (toFilePath bakedP <> "**/*.panrep1") %> \out -> -- from Panrep
    -- calls the copy html and the conversion from md
        do
            csss <- getNeeds debug doughP bakedP "css" "css"
            when (inform debug) $ putIOwords ["rule **/*.html need", showT csss]
            imgs <- getNeeds debug doughP bakedP "jpg" "jpg"
            imgs2 <- getNeeds debug doughP bakedP "JPG" "JPG"
            needP imgs
            needP imgs2
            when (inform debug) $ putIOwords ["rule **/*.html need", showPretty imgs, showPretty imgs2]
            convertAny debug bakedP bakedP flags layout out convPanrep2panrep1 "convPanrep2panrep1"

    (toFilePath bakedP <> "**/*.pdf") %> \out -> -- insert pdfFIles1
        do
            when (inform debug) $ putIOwords ["rule **/*.pdf", showT out]
            imgs <- getNeeds debug doughP bakedP "jpg" "jpg"
            imgs2 <- getNeeds debug doughP bakedP "JPG" "JPG"
            needP imgs
            needP imgs2
            when (inform debug) $ putIOwords ["rule **/*.pdf need", showT imgs, showT imgs2]
            convertAny debug bakedP bakedP flags layout out convTex2pdf "convTex2pdf"

    (toFilePath bakedP <> "**/*.tex") %> \out -> -- insert pdfFIles1
        convertAny debug bakedP bakedP flags layout out convTexsnip2tex "convTexsnip2tex"

    (toFilePath bakedP <> "**/*.texsnip") %> \out -> -- insert pdfFIles1
        convertAny debug bakedP bakedP flags layout out convPanrep2texsnip "convPanrep2texsnip"

    (toFilePath bakedP <> "**/*.panrep") %> \out -> -- insert pdfFIles1
        convertAny debug bakedP bakedP flags layout out convDocrep2panrep "convDocrep2panrep"

    (toFilePath bakedP <> "**/*.docrep") %> \out -> -- insert pdfFIles1  -- here start with doughP
        do
            bibs <- getNeeds debug doughP bakedP "bib" "bib"
            needP bibs
            csls <- getNeeds debug doughP bakedP "csl" "csl"
            needP csls
            when (inform debug) $ putIOwords ["rule **/*.docrep need", showT bibs]
            when (inform debug) $ putIOwords ["rule **/*.docrep need", showT csls]

            convertAny debug doughP bakedP flags layout out convMD2docrep "convMD2docrep"

    -- rest are copies

    (toFilePath bakedP <> "/*.md") -- is this required??
        %> \out -> -- insert css -- no subdir
            copyFileToBaked debug doughP bakedP out
    (toFilePath bakedP <> "/*.css")
        %> \out -> -- insert css -- no subdir
            copyFileToBaked debug doughP bakedP out
    (toFilePath bakedP <> "/*.csl")
        %> \out -> -- insert css -- no subdir
            copyFileToBaked debug doughP bakedP out

    [toFilePath bakedP <> "/*.JPG", toFilePath bakedP <> "/*.jpg"]
        |%> \out -> -- insert img files
        -- no subdir (for now)
            copyFileToBaked debug doughP bakedP out

    (toFilePath bakedP <> "**/*.bib")
        %> \out -> copyFileToBaked debug doughP bakedP out

getNeeds ::
    NoticeLevel ->
    Path Abs Dir ->
    Path Abs Dir ->
    Text ->
    Text ->
    Action [Path Abs File]
{- ^ find the files which are needed (generic)
  from source with extension ext
-}
getNeeds debug sourceP targetP extSource extTarget = do
    let sameExt = extSource == extTarget
    when (inform debug) $
        putIOwords
            [ "===================\ngetNeeds extSource"
            , extSource
            , "extTarget"
            , extSource
            , "sameExt"
            , showT sameExt
            ]

    filesWithSource :: [Path Rel File] <-
        getDirectoryToBake
            "DNB"
            sourceP
            ["**/*." <> t2s extSource]
    -- subdirs
    let filesWithTarget =
            if sameExt
                then [targetP </> c | c <- filesWithSource]
                else
                    map
                        (replaceExtension' extTarget . (targetP </>))
                        filesWithSource ::
                        [Path Abs File]
    when (inform debug) $ do
        putIOwords
            [ "===================\ngetNeeds -  source files 1"
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
