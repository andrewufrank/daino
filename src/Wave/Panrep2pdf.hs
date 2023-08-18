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

import Foundational.Filetypes4sites
-- import GHC.Generics (Generic)
import Uniform.Pandoc ( writeTexSnip2 )
import Uniform.MetaStuff
-- import Uniform.TemplateStuff
-- import Uniform.TemplateStuff (Template)
-- import UniformBase
-- import qualified  Data.Map  as M
-- import Text.DocTemplates as DocTemplates ( Doc )
-- import Uniform.Latex
import Uniform.WritePDF
-- import Paths_daino (version)
-- import Foundational.SettingsPage 
import GHC.Generics (Generic)

import Uniform.Json ( ToJSON(toJSON), Value, ErrIO )
import Uniform.Pandoc
-- import Uniform.Latex 
-- import qualified Text.Pandoc.Shared as P
import Uniform.Http ( HTMLout (HTMLout) )
import UniformBase
import Uniform.MetaPlus hiding (MetaPlus(..), Settings(..), ExtraValues(..))

import Data.Maybe (fromMaybe)
import qualified Data.Map as M
-- import Wave.Docrep2panrep
-- import Wave.Md2doc
import System.FilePath (replaceExtension)
import Foundational.SettingsPage
import Wave.Panrep2html 
default (Integer, Double, Text)

-- ------------------------------------ panrep2texsnip

panrep2texsnip :: NoticeLevel -> Panrep -> ErrIO TexSnip
panrep2texsnip debug metaplus3 = do
    putIOwords ["panrep2texsnip \n"] --, showT res, "\n--"]
    lat1 <- meta2xx writeTexSnip2 (metap metaplus3)
    let metaplus5 = metaplus3{metaLatex = lat1}

    return metaplus5

 
    -- when (inform debug) $ putIOwords ["\n panrep2texsnip start"]
    -- res1 <- writeTexSnip2 p
    -- when (inform debug) $ putIOwords ["\n panrep2texsnip res1", showT res1]
    -- let res = (TexSnip y res1)
    -- when (inform debug) $ putIOwords ["\n panrep2texsnip done"]


-- text2absFile :: Path Abs Dir -> Text -> Path Abs File 
-- text2absFile doughP t = doughP </> makeRelFile (t2s t)

texsnip2tex ::  NoticeLevel -> TexSnip -> ErrIO (Latex, [FilePath], Text)

-- NoticeLevel ->  Path Abs Dir -> Path Abs Dir -> DainoMetaPlus ->  Path Abs File -> ErrIO Latex
-- the (lead) snip which comes from the md which gives the name to the resulting tex and pdf 
-- and ist metadata are included (taken from the snip)
-- it may include other filenames, the snips of these
-- are then included in the pdf built. 

-- currently only one snip, 
-- currently the biblio and references seem not to work with the new citeproc stuff (which takes the info from the )
texsnip2tex  debug metaplus4 = do 
-- debug doughP bakedP metaplus4 latexDtpl = do
    when (inform debug) $ putIOwords ["\n texsnip2tex start"]

    let sett3 = sett metaplus4
        extra4 = extra metaplus4
        -- mf = masterTemplateFile $ siteLayout sett3
        mf = texTemplateFile $ siteLayout sett3 -- change to latex template
        masterfn = templatesDir (siteLayout sett3) </> mf

    when (inform debug) $ do
            putIOwords ["\ntexsnip2tex", "siteLayout sett3"
                , showPretty $ siteLayout sett3]
            putIOwords ["texsnip2tex", "mf", showPretty mf]
            putIOwords ["texsnip2tex", "masterfn", showPretty masterfn]

    targetTempl  <- compileTemplateFile2 masterfn
    testTempl  <- compileTemplateFile2 testTemplateFn

    -- htm1 <- meta2xx writeHtml5String2 (metap metaplus4)

    --if this is an index file it has files and dirs 
    when (inform debug) $ 
            putIOwords ["texsnip2tex", "extra4", showPretty extra4]

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
                .  map link $ (dirs ++ files)
                     :: [FilePath]

    when (inform debug) $ 
            putIOwords ["texsnip2tex", "\n\tneeds ", showPretty needs ]

    valsDirs :: [Maybe IndexEntry2]<- mapM (getVals2latex debug bakedP) dirs
    valsFiles :: [Maybe IndexEntry2] <- mapM (getVals2latex debug bakedP) files

    when (inform debug) $ putIOwords ["texsnip2tex 1"  ]

    when (inform debug) $ do
            putIOwords ["texsnip2tex", "valsDirs", showT valsDirs]
            -- putIOwords ["texsnip2tex", "valsFiles", showT valsFiles]
    when (inform debug) $ putIOwords ["texsnip2tex 2"  ]

    let extra5 = extra4{fileEntries = catMaybes valsFiles
                        , dirEntries = catMaybes valsDirs}
                        -- uses the same record selectors as html
                        -- but started with an empty slate?
    let metaplus5 = metaplus4{extra = extra5}
    when (inform debug) $ putIOwords ["texsnip2tex", "extra5", showPretty extra5]
    when (inform debug) $ 
            putIOwords ["texsnip2tex", "metaplus5", showPretty metaplus5]

    let hpl1 = renderTemplate targetTempl (toJSON metaplus5)  -- :: Doc Text
    let ht1 = render (Just 50) hpl1  -- line length, can be Nothing

    let ttpl1 = renderTemplate testTempl (toJSON metaplus5)  -- :: Doc Text
    let tt1 = render (Just 50) ttpl1  -- line length, can be Nothing

    when (inform debug) $ putIOwords ["texsnip2tex render html done"
        , "ht1",  ht1
        ]
    when (inform debug) $ putIOwords ["texsnip2tex render testTemplate done"
        , "tt1",  tt1
        ]
    
    -- bakeOnetexsnip2tex will write to disk
    return (Latex ht1, needs, tt1)

getVals2latex :: NoticeLevel -> Path Abs Dir -> IndexEntry2
                -> ErrIO (Maybe IndexEntry2)
-- get the panrep and fill the vals 
getVals2latex debug bakedP ix2 = do
    putIOwords ["getVals2latex start"]
    let fn = makeAbsFile $ addDir (toFilePath bakedP) (link ix2)  :: Path Abs File
    when (inform debug) $ putIOwords ["getVals2latex fn", showT fn ]
    pan1 <- read8 fn panrepFileType
    when (inform debug) $ putIOwords ["getVals2latex pan1", showT pan1 ]

    let m = metaLatex pan1  -- select latex code 
    
        ix3 = ix2   { abstract = lookup7withDef ""  "abstract" m
                    , title = lookup7withDef "TITLE MISSING" "title" m
                    , author = lookup7withDef "" "author" m -- todo suppressed?
                    , date = lookup7 "date" m
                    , sortOrder = lookup7withDef "filename" "sortOrder" m
                    , version = lookup7 "version" m
                    , visibility = lookup7 "visibility" m
                    }

    when (inform debug) $ putIOwords ["getVals2latex end"]
    
    return $ if True -- includeBakeTest3 def -- bring down 
                            -- (version ix3) (visibility ix3)
                then Just ix3 else Nothing




-- old fromMd: 
    -- let meta2 = addMetaFieldT "documentclass" "article" snip1
    -- t  :: M.Map Text Text <- meta2xx   writeTexSnip2 meta2
    -- putIOwords ["texsnip2tex~meta2hres tHtml \n", showT t, "\n--"]

    -- templL :: Template Text <- compileTemplateFile2 latexDtpl
    -- -- templL :: Template Text <- compileDefaultTempalteLatex
    --     -- templL :: Template Text  <-compileDefaultTempalteLatex
    --     -- -- renderTemplate :: (TemplateTarget a, ToContext a b) => Template a -> b -> Doc a
    -- let restpl = renderTemplate templL (toJSON metaplus4) -- :: Doc Text
    -- let resH = render (Just 50) restpl :: Text  -- line length, can be Nothing
    --     -- let restplL = renderTemplate templL ctLatex :: Doc Text
    --     -- let resL = render (Just 50) restplL  :: Text  -- line length, can be Nothing    -- todo 
    -- putIOwords ["texsnip2tex~meta2hres resHl \n",  resH, "\n--"]
    -- return . Latex $ resH 

-- very old 
--     let yam = snipyam snip1 
--     when (inform debug) $ putIOwords ["\n texsnip2tex for link", showT (dyFn yam), showT (dyBook yam)]
--     let latexparam = LatexParam   -- defined in uniform.latex
--             { latTitle = dyTitle yam 
--             , latAuthor = dyAuthor yam
--             , latAbstract = dyAbstract yam
--             , latLanguage = latexLangConversion $ dyLang yam 
--             , latFn = s2t $ dyFn yam
--             , latBakedDir = s2t . toFilePath $ doughP 
--             , latDainoVersion = showT version
--             , latBibliography = (s2t . toFilePath $ doughP) <> (maybe "resources/BibTexLatex.bib" id (dyBibliography yam))
--                 -- fix an absolute path for the bib files 
--                 -- will be dificult if not in the resources?
--             -- make this an abs file name 
--             , latBiblioTitle = "References"
--             -- todo depends on latLanguage
--             , latStyle    = dyStyleBiber (snipyam  snip1)
--                 --  maybe "authoryear" id $ dyStyleBiber yam
--             , latReferences = maybe "" (shownice ) $ dyReferences yam
--             -- , latBook = dyBook yam
--             , latBookBig = if dyBook yam == "bookbig" then "bookbig" else zero
--             , latBooklet = if dyBook yam == "booklet" then "booklet" else zero  -- must be undefined or zero for the unused values
--             , latIndex = zero -- the collected index 
--             , latContent = unTexSnip snip1 -- the content of this file
--             -- , latTheme = dy 
--             -- , latSnips = zero 
--         }
 

--     let webroot = doughP  -- use the images befor they are copied
--         -- snip1 = unTexSnip p 
--     when (inform debug) $ putIOwords ["\n texsnip2tex dyIndexEntry"
--         , showT (dyIndexEntry yam)]
--     when (inform debug) $ putIOwords ["\n texsnip2tex latexparam"
--         , showT latexparam]
--     latexparam6 <- if ( "bookbig" == latBookBig  latexparam) 
--                             ||   ("booklet" == latBooklet  latexparam)
--         then do 
--             let latexparam2 = latexparam{latIndex=dyIndexEntry yam}
--             latexparam3 <- collectIIndexWithContent2nd debug bakedP latexparam2
--             return latexparam3
--         else  return latexparam  

--     when (inform debug) $ putIOwords ["\n texsnip2tex latexparam6 completed with content previously found"
--         , showT latexparam6]

--     latexparam7 <- tex2latex debug webroot latexparam6 latexDtpl 
--     when (inform debug) $ putIOwords ["texsnip2tex latexparam7 produced tex", showT latexparam7]
   
--     -- tex file must be full, ordinary latex content

--     when (inform debug) $ putIOwords ["\n texsnip2tex done"]
--     return . Latex $ latexparam7

-- collectIIndexWithContent2nd :: NoticeLevel -> Path Abs Dir -> LatexParam -> ErrIO LatexParam
-- -- | complete a file and a dir  
-- --      works for both 1st and 2nd 
-- collectIIndexWithContent2nd debug bakedP latexparam2 = do 
--     let 
--         latix2 = latIndex latexparam2 
--         dirixs = dirEntries latix2 
--         fileixs = fileEntries latix2 

--     dirixs7 :: [IndexEntry] <- mapM (completeOneIx2nd  debug bakedP) dirixs
--     fileixs7 :: [IndexEntry] <- mapM (completeOneIx2nd  debug bakedP) fileixs
--     let latix7 = latix2{dirEntries = dirixs7, fileEntries = fileixs7}

--     latix8 <- contentIx_Snip debug bakedP latix7

--     when (inform debug) $ putIOwords ["\n collectIIndexWithContent2nd end latix8", showT latix8]
--     return $ latexparam2{latIndex =   latix8}



-- completeOneIx2nd :: NoticeLevel -> Path Abs Dir -> IndexEntry -> ErrIO IndexEntry
-- --     -- get the snips for one dir entry 
-- completeOneIx2nd debug bakedP ix = do 
--     when (inform debug) $ putIOwords ["\n completeOneIx2nd start", showT ix]

--     let fileixs = fileEntries ix 
--     let dirixs = dirEntries ix 
--     fileixs2 <- mapM (contentIx_Snip debug bakedP)  fileixs
--     dirixs2 <- mapM (contentIx_Snip debug bakedP)  dirixs

--     let ix4 = ix{fileEntries = fileixs2, dirEntries = dirixs2}
--     ix5 <- contentIx_Snip debug bakedP ix4
    
--     return ix5

-- contentIx_Snip :: NoticeLevel -> Path Abs Dir -> IndexEntry -> ErrIO IndexEntry
--     -- get the snip for one index entry (dir or file)
--     -- only the content (abstract and title collected before) 
-- contentIx_Snip debug bakedP ix = do 
--     when (inform debug) $ putIOwords ["\n contentIx_Snip start", showT ix]
--     let
--         ln = makeRelFile . link $ ix 
--         lnfp = bakedP </> ln :: Path Abs File 

--     texsnip1 :: TexSnip <-   read8 lnfp texSnipFileType 
--     -- let res = unlines' [zero, titsnip, "", abssnip, "", unTexSnip texsnip1]
--     let ix2 = ix{content =  unTexSnip texsnip1}
--     when (inform debug) $ putIOwords ["\n contentIx_Snip end", showT ix2]
--     return ix2




-- ------------------------------------ tex2pdf


-- refdir must be set to the dir where searches for 
-- biblio etc start - seems not correct
-- the refdir is where the intermediate files are put
-- this is fnres - just the doughPath
tex2pdf :: NoticeLevel -> Path Abs File ->  Path Abs File ->  Path Abs Dir ->  ErrIO ()
tex2pdf debug fn fnres doughP  =  do
    when (inform debug) $ putIOwords ["\n tex2pdf start for", showT fn]

    let refDir = -- makeAbsDir "/home/frank/bakedTestSite"
                    -- dough does not work either
                    -- must be local dir of file to process
            makeAbsDir . getParentDir . toFilePath $ fn :: Path Abs Dir
    -- refDir must be the place where biblio is place (or searched from - best ) - i.e. the root for dough 
    when (inform debug) $ putIOwords ["\n tex2pdf refDir", showT refDir]
    texf <- read8 fn texFileType
    when (inform debug) $ putIOwords ["\n tex2pdf texf content", showT texf]
    writePDF2 debug  fn fnres refDir
    -- for debug put only the file unprocessed
    -- write8 fnres pdfFileType (PDFfile . unLatex $ texf)
    when (inform debug) $ putIOwords ["\n tex2pdf done"]
    return ()
