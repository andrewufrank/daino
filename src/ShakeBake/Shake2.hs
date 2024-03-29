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

        anders geloest: 
        test ob file in dough existiert, dann kopiert

    ausgeschlossene directories werden durch DNB  markiert
    die files die in diesen gefunden werden, nicht zum umwandeln
        anzumelden, indem deren namen nicht in "want" eingeschlossen
        werden.

    pdf werden aus tex erzeugt, die aus texsnip erzeugt werden.
    jedes md ergibt ein texsnip
    jedes texsnip gibt ein tex
        die indexseiten, die grosse themen zusammenfassen
        produzieren ein tex mit includes fuer die subseiten
        und der preamble/post fuer e.g. biblio
    jedes tex gibt ein pdf
    das heisst: jedes md gibt ein pdf (auch eingestellte)
    -}
----------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans
            -fno-warn-missing-signatures
            -fno-warn-missing-methods
            -fno-warn-duplicate-exports  #-}


module ShakeBake.Shake2 where

import           Uniform.Shake
import Foundational.SettingsPage
    -- ( SiteLayout(doughDir, bakedDir, themeDir),
    --   Settings(siteLayout) )
import Foundational.CmdLineFlags
import ShakeBake.ConvertFiles
    ( io2bool, convertAny, copyFileToBaked )
import Wave.Md2doc 

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
                { shakeFiles = toFilePath bakedP  -- wgy should the shake files to into baked?
                , shakeVerbosity = Info -- Verbose --  Loud
                        -- verbose gives a single line for each file processed
                        --          plus info for copying
                        -- info gives nothing in normal process 
                , shakeLint = Just LintBasic
                }
    -- putIOwords ["shakeArgs2", "done"]
    return res

shakeAll :: NoticeLevel -> Settings -> PubFlags -> FilePath -> ErrIO ()
-- ^ calls shake in the IO monade. this is in the ErrIO
shakeAll debug sett3 flags causedby = do
    let layout = siteLayout sett3 
        doughP = doughDir layout -- the regular dough
        bakedP = bakedDir layout
        themeP = themeDir layout
    putIOwords
        [ "\n\n===================================== shakeAll start"
        , "\n flags"
        , showPretty flags
        , "\ncaused by"
        , s2t causedby
        , "."
        , "\ndebug:", showT debug
        , "\ndough", showT doughP 
        , "\nbaked", showT bakedP 
        , "\ntheme", showT themeP 
        , "\ndebug", showT debug 

        , "\n======================================="
        ]

    -- to cllect the list of all md files to publish
    -- and pass it on     
    flags2 <- collectMd2include sett3 flags 
    putIOwords ["flags", showPretty flags2]        
    callIO $ shakeMD debug sett3  flags2  doughP bakedP

-- todo remove shakeMD and pass only layout

type RelFiles = [Path Rel File]

shakeMD ::
    NoticeLevel ->
    Settings ->
    PubFlags ->
    Path Abs Dir -> -- dough (source for files)
    Path Abs Dir -> -- baked (target dir for site)
    IO ()
{- ^ bake all md files and copy the resources
 from each md produce:
    - html 
    - pdf 
 sets the current dir to doughDir
 copies banner image
 in IO
 TOP shake call
-}
shakeMD debug sett4 flags  doughP bakedP = shakeArgs2 bakedP $ do
    -- the special filenames which are necessary
    -- because the file types are not automatically
    -- copied
    -- todo remove doughP and bakedP


    putInform debug [ "shakeMD dirs\n"
        , "\tbakedP\n", showT bakedP
        , "\ndebug", showT debug]
    -- let siteDirs = siteLayout sett4 
        -- doughP = doughDir siteDirs -- the regular dough
        -- bakedP = bakedDir siteDirs
        -- themeP = themeDir siteDirs

    want ["allMarkdownConversion"]

    phony "allMarkdownConversion" $ do
        -- these are functions to construct the desired results
        -- which then produce them
        -- the original start needs in baked (from the files in dough)

        -- put a link to the themplates folder into dough/resources
        -- otherwise confusion with copying the files from two places

        -- from the theme folder copy woff, css and jpg/JPG 
        -- woffTheme <- getNeeds debug sett4 themeP bakedP "woff" "woff"
        -- needP woffTheme
        -- imgsTheme <- getNeeds debug sett4   themeP bakedP "jpg" "jpg"
        -- imgs2Theme <- getNeeds debug sett4   themeP bakedP "JPG" "JPG"
        -- needP imgsTheme
        -- needP imgs2Theme
        -- cssTheme <- getNeeds debug sett4 themeP bakedP "css" "css"
        -- needP cssTheme

            -- do the images first to be findable by latex processor
        imgs <- getNeeds debug sett4   doughP bakedP "jpg" "jpg"
        imgs2 <- getNeeds debug sett4   doughP bakedP "JPG" "JPG"
        needP imgs
        needP imgs2
        -- needPwithoutput imgs
        -- needPwithoutput imgs2

        -- htmls <- getNeedsMD debug flags sett4 doughP bakedP "md" "html"
        let needsSpec =  map (bakedP </> ) $ -- (replaceDirectoryP doughP bakedP) $  
                map (replaceExtension' "html" ) (mdFiles flags)
        needPwithoutput "md" "html" needsSpec
        putIOwords ["need mdFiles"]
        -- let mdFiles = map (makeRelativeP bakedP )  htmls
    
        -- check against the list (quicker than reading files)
        unless (quickFlag flags) $ do 
            pdfs <- getNeedsMD debug flags sett4 doughP bakedP "md" "pdf"
            needPwithoutput "md" "pdf" pdfs


        csss <- getNeeds debug sett4   doughP bakedP "css" "css"
        needP csss  -- needPwithoutput csss

        -- fonts, takes only the woff
        -- from the link to the template folder
        woffs <- getNeeds debug sett4   doughP bakedP "woff" "woff"
        needP woffs

        publist <- getNeeds debug sett4   doughP bakedP "html" "html"
        needP publist
        -- for the pdfs which are already given in dough
        pdfs2 <- getNeeds debug sett4   doughP bakedP "pdf" "pdf"
        needP pdfs2
        bibs <- getNeeds debug sett4   doughP bakedP "bib" "bib"
        needP bibs


    (toFilePath bakedP <> "**/*.html") %> \out -> -- from Panrep
    -- calls the copy html if a html exist in dough 
            -- else calls the conversion from md

        do
            putInform debug ["rule **/*.html", showT out]

            let outP = makeAbsFile out :: Path Abs File
            let fromfile = doughP </> makeRelativeP bakedP outP
            fileExists <- io2bool $ doesFileExist' fromfile
            putInform debug ["rule **/*.html - fileExist:", showT fileExists]
            
            if fileExists 
                then copyFileToBaked debug doughP bakedP out
                else 
            -- csss <- getNeeds debug sett4 doughP bakedP "css" "css"
            -- needP csss
            -- -- csss seems not necessary
            -- imgs <- getNeeds debug sett4 doughP bakedP "jpg" "jpg"
            -- imgs2 <- getNeeds debug sett4 doughP bakedP "JPG" "JPG"
            -- needP imgs
            -- needP imgs2
            -- putInform debug ["rule **/*.html", showT out]
  
                    convertAny debug bakedP bakedP flags sett4 out  "convPanrep2html"


    (toFilePath bakedP <> "**/*.pdf") %> \out -> -- insert pdfFIles1
        do
            putInform debug ["rule **/*.pdf", showT out]
            -- imgs <- getNeeds debug sett4 doughP bakedP "jpg" "jpg"
            -- imgs2 <- getNeeds debug sett4 doughP bakedP "JPG" "JPG"
            -- needP imgs
            -- needP imgs2
            -- why is this here necessary: failed on testSort.pdf?
            -- was ein jpg will ?
            -- TODO improve error from lualatex
            -- putInform debug ["rule **/*.pdf need", showT imgs, showT imgs2]

            let outP = makeAbsFile out :: Path Abs File
            let fromfile = doughP </> makeRelativeP bakedP outP
            fileExists <- io2bool $ doesFileExist' fromfile
            putInform debug ["fileExist:", showT fileExists]
            
            if fileExists 
                then copyFileToBaked debug doughP bakedP out
                else             
                    convertAny debug bakedP bakedP flags sett4 out  "convTex2pdf"

    (toFilePath bakedP <> "**/*.tex") %> \out -> -- insert pdfFIles1
        convertAny debug bakedP bakedP flags sett4 out  "convTexsnip2tex"

    (toFilePath bakedP <> "**/*.texsnip") %> \out -> -- insert pdfFIles1
        convertAny debug bakedP bakedP flags sett4 out  "convPanrep2texsnip"

    (toFilePath bakedP <> "**/*.panrep") %> \out -> -- insert pdfFIles1
        do convertAny debug bakedP bakedP flags sett4 out  "convDocrep2panrep"

    (toFilePath bakedP <> "**/*.docrep") %> \out -> -- insert pdfFIles1  -- here start with doughP
        do
            -- bibs <- getNeeds debug sett4 doughP bakedP "bib" "bib"
            -- needP bibs
            -- csls <- getNeeds debug sett4 doughP bakedP "csl" "csl"
            -- needP csls
            -- putInform debug ["rule **/*.docrep need", showT bibs]
            -- putInform debug ["rule **/*.docrep need", showT csls]

            convertAny debug doughP bakedP flags sett4 out  "convMD2docrep"
            return ()

    -- rest are copies

    -- (toFilePath bakedP <> "/*.md") -- is required because the convA2B - but this is fixed 
    --     %> \out -> -- insert css -- no subdir
            -- copyFileToBaked debug doughP bakedP out
    (toFilePath bakedP <> "/*.css")
        %> \out -> -- insert css -- no subdir
            copyFileToBaked debug doughP bakedP out
    (toFilePath bakedP <> "/*.csl")  -- not used with biber TODO 
        %> \out -> -- insert css -- no subdir
            copyFileToBaked debug doughP bakedP out

    [toFilePath bakedP <> "/*.JPG", toFilePath bakedP <> "/*.jpg"]
    -- seems not to differentiate the JPG and jpg; copies whatever the original 
    -- the html and/or the pdf includegraphics seem to be case sensitive, even for the extension
        |%> \out -> -- insert img files
        -- no subdir (for now)
            copyFileToBaked debug doughP bakedP out

    (toFilePath bakedP <> "**/*.bib")
        %> \out -> copyFileToBaked debug doughP bakedP out
    -- the fonts in a compressed format 
    (toFilePath bakedP <> "**/*.woff")
        %> \out -> copyFileToBaked debug doughP bakedP out


getNeeds ::
    NoticeLevel 
    -> Settings -- ^ the site layout etc
    -> Path Abs Dir  -- ^ source dir
    -> Path Abs Dir  -- ^ target dir
    -> Text  -- ^ extension source
    -> Text  -- ^ extension target
    -> Action [Path Abs File]
{- ^ find the files which are needed (generic)
  from source with extension ext
  does not include directory DNB (do not bake)
-}
getNeeds debug  sett4 sourceP targetP extSource extTarget = do
    let sameExt = extSource == extTarget
    putInform debug 
            [ "===================\ngetNeeds extSource"
            , extSource
            , "extTarget"
            , extSource
            , "sameExt"
            , showT sameExt
            , "\ndebug", showT debug
            ]

    filesWithSource :: [Path Rel File] <- -- getDirectoryFilesP
        getFilesToBake
            (doNotBake  (siteLayout sett4)) -- exclude files containing
            sourceP
            ["**/*." <> t2s extSource]
    -- subdirs
    let filesWithTarget =
            if sameExt
                then [targetP </> c | c <- filesWithSource]
                else
                    map
                        (replaceExtension' extTarget . (targetP </>))
                         filesWithSource  
                                :: [Path Abs File]
    putInform debug 
            [ "===================\ngetNeeds -  source files 1"
            , "for ext"
            , extSource
            , "files\n"
            , showT filesWithSource
            ]
    putInform debug 
            [ "\nbakePDF -  target files 2"
            , "for ext"
            , extTarget
            , "files\n"
            , showT filesWithTarget
            ]
    return filesWithTarget

getNeedsMD ::
    NoticeLevel 
    -> PubFlags 
    -> Settings   -- perhaps the next two can be
    -> Path Abs Dir  -- ^ source dir
    -> Path Abs Dir  -- ^ target dir
    -> Text  -- ^ extension source
    -> Text  -- ^ extension target
    -> Action [Path Abs File]
{- ^ find the files which are needed (generic)
  from source with extension ext
  does not include directory DNB (do not bake)
  does include a filter for version field in YAML header 
-}
getNeedsMD debug flags sett4 sourceP targetP extSource extTarget = do
    -- let debug = NoticeLevel0   -- avoid_output_fromHere_down
    let sameExt = extSource == extTarget
    when (inform debug) $
        putIOwords
            [ "===================\ngetNeedsMD extSource"
            , extSource
            , "extTarget"
            , extSource
            , "sameExt"
            , showT sameExt
            , "\ndebug", showT debug
            ]

    filesWithSource :: [Path Rel File] <- -- getDirectoryFilesP
        getFilesToBake
             (doNotBake  (siteLayout sett4))   -- exclude files containing
            sourceP
            ["**/*." <> t2s extSource]
    -- filter for version < publish in YAML header 
    files2 :: [Maybe (Path Abs File)] <- runErr2action 
                $ mapM (filterNeeds debug flags sett4 ) filesWithSource
    let files3 = map (makeRelativeP sourceP) $   catMaybes files2 :: [Path Rel File]
    -- this is the complete list of all included mds
    -- use for all following checks 
    
    let filesWithTarget =
            if sameExt
                then [targetP </> c | c <- files3] :: [Path Abs File]
                            -- was filesWithSource
                else
                    map (replaceDirectoryP sourceP targetP) $  
                    map
                        (replaceExtension' extTarget )
                            (catMaybes files2) :: [Path Abs File]
    -- when (inform debug) $ do
    --     putIOwords
    --         [ "===================\ngetNeeds -  source files 1 not filtered"
    --         , "for ext"
    --         , extSource
    --         , "files\n"
    --         , showT filesWithSource
    --         ]
    when ((inform debug) && (filesWithTarget /= [])) $ do
        putIOwords
            [ "===================\ngetNeeds bakePDF -  target files 2 filtered"
            , "for ext"
            , extTarget
            , "files\n"
            , showT filesWithTarget
            ]
    return filesWithTarget

needPwithoutput t1 t2 files = do 
        putInform NoticeLevel1 ["\nneeds set", t1, t2, showT files]
        needP files 