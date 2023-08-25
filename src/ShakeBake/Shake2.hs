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

import           Uniform.Shake
import Foundational.SettingsPage
import Foundational.CmdLineFlags
import ShakeBake.Bake

shakeArgs2 :: Path b t -> Rules () -> IO ()
{- | set the options for shake
 called in shakeMD
-}
shakeArgs2 bakedP = do
    -- putIOwords ["shakeArgs2", "bakedP", s2t . toFilePath $ bakedP]
    res <-
        shake
            shakeOptions
                { shakeFiles = toFilePath bakedP  -- where should the shake files to into baked?
                , shakeVerbosity = Verbose --  Loud Info -- 
                        -- verbose gives a single line for each file processed
                        --          plus info for copying
                        -- info gives nothing in normal process 
                , shakeLint = Just LintBasic
                }
    return res

shakeAll :: NoticeLevel -> Settings -> PubFlags -> FilePath -> ErrIO ()
-- ^ calls shake in the IO monade. this is in the ErrIO
shakeAll debug sett3 flags causedby = do
    let layout = siteLayout sett3 
        doughP = doughDir layout :: Path Abs Dir -- the regular dough
        bakedP = bakedDir layout :: Path Abs Dir
        themeP = themeDir layout :: Path Abs Dir
    putIOwords
        [ "\n\n===================================== shakeAll start"
        , "\n flags", showPretty flags
        , "\ncaused by", s2t causedby, "."
        , "\ndebug:", showT debug
        , "\ndough", showT doughP 
        , "\nbaked", showT bakedP 
        , "\ntheme", showT themeP 
        , "\ndebug", showT debug 
        , "\n======================================="
        ]

    let 
        fs4 = ["index.html"]
        fs4htmlTemplate =   ["resources/theme/templates/static/tufte.css"
                            , "resources/theme/templates/static/tufte-extra.css"
                            , "resources/theme/templates/static/pandoc.css"
                            , "resources/theme/templates/static/tufte-additions.css"
                            , "resources/img/squared16.jpg" -- add rest 
                            , "resources/theme/templates/img/DSC04809.JPG"
                            -- banner . siteHeader $ sett3 
                            ]
        flags2 = flags{mdFiles = 
            concat [map (addFileName bakedP) $ map makeRelFile fs4
                                    , map (addFileName bakedP ) $ map makeRelFile fs4htmlTemplate
                                    ]
        }
    putIOwords ["mdFiles flags", showT $ mdFiles flags2]        
    callIO $ shakeMD NoticeLevel2 sett3  flags2   

-- todo remove shakeMD and pass only layout

addDirFP :: FilePath -> FilePath -> FilePath 
addDirFP dir1 fn1 = dir1 </> fn1 

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
                        needsFound :: [FilePath]<- runErr2action $ 
                             getNeeds4html debug flags bakedFrom sett4 outP
                        need needsFound
                        putInform debug ["rule **/*.html continued", showT out]

                        needs2 <- runErr2action $ bakeOnePanrep2html debug flags bakedFrom sett4 outP 
                        putInform debug ["rule **/*.html - needs2", showT needs2]
                        return ()            

    (toFilePath bakedP <> "**/*.panrep") %> \out -> -- insert pdfFIles1
        do 
            putInform debug ["rule **/*.panrep", showT out]

            let outP = makeAbsFile out :: Path Abs File
            
            let bakedFrom = replaceExtension'  "docrep" outP
            putInform debug ["rule **/*.html - bakedFrom", showT bakedFrom]
            needsFound :: [FilePath]<- runErr2action $ 
                    getNeeds4pan debug flags bakedFrom sett4 outP
            need needsFound            
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



needPwithoutput t1 t2 files = do 
        putInform NoticeLevel1 ["\nneeds set", t1, t2, showT files]
        needP files 


io2bool :: MonadIO m => ErrIO b -> m b
io2bool op = do
    -- todo move
    x <- liftIO $ runErr op
    let res = case x of
            Left msg -> errorT [msg]
            Right b -> b
    return res

{- | the generic copy for all the files
 which can just be copied
 (exceptions md, which are a special case of needed)
-}
copyFileToBaked ::
    ( Filenames3 fp (Path Rel File)
    , FileResultT fp (Path Rel File) ~ Path Abs File
    ) =>
    NoticeLevel ->
    fp ->
    Path Abs Dir ->
    FilePath ->
    Action ()
copyFileToBaked debug doughP bakedP out = do
    let outP = makeAbsFile out :: Path Abs File
    when (inform debug) $ liftIO $ putIOwords ["\ncopyFileToBaked outP", showT outP]
    let fromfile = doughP </> makeRelativeP bakedP outP
    -- putInform debug
    --             ["\ncopyFileToBaked fromfile ", showT fromfile, "added NEED automatically"]
    copyFileChangedP fromfile outP