---------------------------------------------------------------------
--
-- Module      :  Uniform.Panrep2pdf
---------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans
            -fno-warn-missing-signatures
            -fno-warn-missing-methods
            -fno-warn-duplicate-exports
            -fno-warn-unused-imports
            -fno-warn-unused-matches #-}

{- |  ready for processing to HTML or to TexSnip -> Tex -> Pdf
-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Wave.Panrep2pdf (
    module Wave.Panrep2pdf,
) where

import UniformBase
import Foundational.Filetypes4sites
import Uniform.Pandoc ( writeTexSnip2 )
import Uniform.MetaStuff
import Uniform.WritePDF
import GHC.Generics (Generic)

import Uniform.Json ( ToJSON(toJSON), Value, ErrIO )
import Uniform.Pandoc
import Uniform.Http ( HTMLout (HTMLout) )
import Uniform.MetaPlus hiding (MetaPlus(..), Settings(..), ExtraValues(..))
import Foundational.CmdLineFlags

import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import System.FilePath (replaceExtension)
import Foundational.SettingsPage
import Wave.Panrep2html 
import Wave.Md2doc ( includeBakeTest3 ) 
import Uniform.Shake  

default (Integer, Double, Text)

-- ------------------------------------ panrep2texsnip

panrep2texsnip :: NoticeLevel -> Panrep -> ErrIO TexSnip
panrep2texsnip debug metaplus3 = do
    putIOwords ["panrep2texsnip \n"] --, showT res, "\n--"]
    lat1 <- meta2xx writeTexSnip2 (metap metaplus3)
    let metaplus5 = metaplus3{metaLatex = lat1}

    return metaplus5

 
    -- putInform debug ["\n panrep2texsnip start"]
    -- res1 <- writeTexSnip2 p
    -- putInform debug ["\n panrep2texsnip res1", showT res1]
    -- let res = (TexSnip y res1)
    -- putInform debug ["\n panrep2texsnip done"]


-- text2absFile :: Path Abs Dir -> Text -> Path Abs File 
-- text2absFile doughP t = doughP </> makeRelFile (t2s t)

-- texsnip2tex ::  NoticeLevel -> TexSnip -> ErrIO (Latex, [FilePath], Text)

-- NoticeLevel ->  Path Abs Dir -> Path Abs Dir -> DainoMetaPlus ->  Path Abs File -> ErrIO Latex
-- the (lead) snip which comes from the md which gives the name to the resulting tex and pdf 
-- and ist metadata are included (taken from the snip)
-- it may include other filenames, the snips of these
-- are then included in the pdf built. 

-- currently only one snip, 
-- currently the biblio and references seem not to work with the new citeproc stuff (which takes the info from the )
texsnip2tex :: NoticeLevel
    -> PubFlags
    -> DainoMetaPlus
    -> ExceptT Text IO (Latex, [FilePath], Text)
texsnip2tex  debug pubFlags metaplus4 = do 
-- debug doughP bakedP metaplus4 latexDtpl = do
    putInform debug ["\n texsnip2tex start"]

    let sett3 = sett metaplus4
        extra4 = extra metaplus4
        -- mf = masterTemplateFile $ siteLayout sett3
        mf = texTemplateFile $ siteLayout sett3 -- change to latex template
        masterfn = templatesDir (siteLayout sett3) </> mf

    putInform debug ["\ntexsnip2tex", "siteLayout sett3"
                , showPretty $ siteLayout sett3]
    putInform debug ["texsnip2tex", "mf", showPretty mf]
    putInform debug ["texsnip2tex", "masterfn", showPretty masterfn]

    targetTempl  <- compileTemplateFile2 masterfn
    testTempl  <- compileTemplateFile2 testTemplateFn

    -- htm1 <- meta2xx writeHtml5String2 (metap metaplus4)

    --if this is an index file it has files and dirs 
    putInform debug ["texsnip2tex", "extra4", showPretty extra4]

    let files = fileEntries  $ extra4 :: [IndexEntry2]
        dirs = dirEntries  $ extra4 :: [IndexEntry2]

    -- calculate needs 
    let
        bakedP =   bakedDir . siteLayout $ sett3
        bakedFP = toFilePath bakedP
        needs = map (`replaceExtension` "texsnip")  -- change extension
                            -- eventually will be panrep again
                            -- after removing the texsnip wave 
                . map (addDir bakedFP ) 
                .  map ixfn $ (dirs ++ files)
                     :: [FilePath]

    putInform debug ["texsnip2tex", "\n\tneeds ", showPretty needs ]

    valsDirs :: [Maybe IndexEntry2]<- 
            mapM (getVals2latex debug pubFlags bakedP) dirs
    valsFiles :: [Maybe IndexEntry2] <- 
            mapM (getVals2latex debug pubFlags bakedP) files

    putInform debug ["texsnip2tex 1"  ]

    putInform debug["texsnip2tex", "valsDirs", showT valsDirs]
            -- putIOwords ["texsnip2tex", "valsFiles", showT valsFiles]
    putInform debug ["texsnip2tex 2"  ]

    let extra5 = extra4{fileEntries = catMaybes valsFiles
                        , dirEntries = catMaybes valsDirs}
                        -- uses the same record selectors as html
                        -- but started with an empty slate?
    let metaplus5 = metaplus4{extra = extra5}
    putInform debug ["texsnip2tex", "extra5", showPretty extra5]
    when (inform debug) $ 
            putIOwords ["texsnip2tex", "metaplus5", showPretty metaplus5]

    let hpl1 = renderTemplate targetTempl (toJSON metaplus5)  -- :: Doc Text
    let ht1 = render (Just 50) hpl1  -- line length, can be Nothing

    let ttpl1 = renderTemplate testTempl (toJSON metaplus5)  -- :: Doc Text
    let tt1 = render (Just 50) ttpl1  -- line length, can be Nothing

    putInform debug ["texsnip2tex render html done", "ht1",  ht1 ]
    putInform debug ["texsnip2tex render testTemplate done", "tt1", tt1 ]
    
    -- bakeOnetexsnip2tex will write to disk
    return (Latex ht1, needs, tt1)

-- getVals2latex :: NoticeLevel -> Path Abs Dir -> IndexEntry2
                -- -> ErrIO (Maybe IndexEntry2)
-- get the panrep and fill the vals 
getVals2latex debug pubFlags bakedP ix2 = do
    putInform debug ["GetVals2 latex ix2", showPretty ix2]
    let 
        fnix4 = (ixfn ix2) :: FilePath
        fnix3 = addDir (toFilePath bakedP) fnix4 :: FilePath
        fnix2 =  fnix3 <.> "panrep"  :: FilePath
        fn = makeAbsFile fnix2
        pdf = replaceExtension2 ".pdf" fn 
        
    -- let fn = makeAbsFile $ addDir (toFilePath bakedP) (link ix2)  :: Path Abs File

    putInform debug ["getVals2latex fn", showT fn ]
    pan1 <- read8 fn panrepFileType
    putInform debug ["getVals2latex pan1", showT pan1 ]

    let m = metaLatex pan1  -- select latex code 
    
        ix3 = ix2   { abstract = lookup7withDef ""  "abstract" m
                    , title = lookup7withDef "TITLE MISSING" "title" m
                    , author = lookup7withDef "" "author" m -- todo suppressed?
                    , date = lookup7 "date" m
                    , sortOrder = lookup7withDef "filename" "sortOrder" m
                    , version = lookup7 "version" m
                    , visibility = lookup7 "visibility" m
                    }

    putInform debug ["getVals2latex end"]
    
    return $ if includeBakeTest3 pubFlags (version ix3) (visibility ix3)
                then Just ix3 else errorT ["getVals2 in panrep2html not included", showT ix2 ]



-- ------------------------------------ tex2pdf


-- refdir must be set to the dir where searches for 
-- biblio etc start - seems not correct
-- the refdir is where the intermediate files are put
-- this is fnres - just the doughPath
tex2pdf :: NoticeLevel -> Path Abs File ->  Path Abs File ->  Path Abs Dir ->  ErrIO ()
tex2pdf debug fn fnres doughP  =  do
    putInform debug ["\n tex2pdf start for", showT fn]

    let refDir = -- makeAbsDir "/home/frank/bakedTestSite"
                    -- dough does not work either
                    -- must be local dir of file to process
            makeAbsDir . getParentDir . toFilePath $ fn :: Path Abs Dir
    -- refDir must be the place where biblio is place (or searched from - best ) - i.e. the root for dough 
    putInform debug ["\n tex2pdf refDir", showT refDir]
    texf <- read8 fn texFileType
    putInform debug ["\n tex2pdf texf content", showT texf]
    writePDF2 debug  fn fnres refDir
    -- for debug put only the file unprocessed
    -- write8 fnres pdfFileType (PDFfile . unLatex $ texf)
    putInform debug ["\n tex2pdf done"]
    return ()
