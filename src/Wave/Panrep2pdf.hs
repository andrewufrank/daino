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
import Foundational.MetaPage
import GHC.Generics (Generic)
import Uniform.Pandoc ( writeTexSnip2 )
import UniformBase

import Uniform.Latex
import Uniform.WritePDF
import Paths_daino (version)

-- ------------------------------------ panrep2texsnip

panrep2texsnip :: NoticeLevel -> Panrep -> ErrIO TexSnip
panrep2texsnip debug (Panrep y p) = do
    when (inform debug) $ putIOwords ["\n panrep2texsnip start"]
    res1 <- writeTexSnip2 p
    when (inform debug) $ putIOwords ["\n panrep2texsnip res1", showT res1]
    let res = (TexSnip y res1)
    when (inform debug) $ putIOwords ["\n panrep2texsnip done"]
    return res


text2absFile :: Path Abs Dir -> Text -> Path Abs File 
text2absFile doughP t = doughP </> makeRelFile (t2s t)

texsnip2tex :: NoticeLevel ->  Path Abs Dir -> Path Abs Dir -> TexSnip ->  Path Abs File -> ErrIO Latex
-- the (lead) snip which comes from the md which gives the name to the resulting tex and pdf 
-- and ist metadata are included (taken from the snip)
-- it may include other filenames, the snips of these
-- are then included in the pdf built. 

-- currently only one snip, 
-- currently the biblio and references seem not to work with the new citeproc stuff (which takes the info from the )
texsnip2tex  debug doughP bakedP snip1 latexDtpl = do
    when (inform debug) $ putIOwords ["\n texsnip2tex start"]
    let yam = snipyam snip1 
    when (inform debug) $ putIOwords ["\n texsnip2tex for link", showT (dyFn yam)]
    let latexparam = LatexParam 
            { latTitle = dyTitle yam 
            , latAuthor = dyAuthor yam
            , latAbstract = dyAbstract yam
            , latLanguage = latexLangConversion $ dyLang yam 
            , latFn = s2t $ dyFn yam
            , latBakedDir = s2t . toFilePath $ doughP 
            , latDainoVersion = showT version
            , latBibliography = (s2t . toFilePath $ doughP) <> (maybe "resources/BibTexLatex.bib" id (dyBibliography yam))
                -- fix an absolute path for the bib files 
                -- will be dificult if not in the resources?
            -- make this an abs file name 
            , latBiblioTitle = "References"
            -- todo depends on latLanguage
            , latStyle    = dyStyleBiber (snipyam  snip1)
                --  maybe "authoryear" id $ dyStyleBiber yam
            , latReferences = maybe "" (shownice ) $ dyReferences yam
            , latBook = dyBook yam
            , latIndex = zero -- the collected index 
            , latContent = unTexSnip snip1 -- the content of this file
            -- , latTheme = dy 
            -- , latSnips = zero 
        }
 

    let webroot = doughP  -- use the images befor they are copied
        -- snip1 = unTexSnip p 
    when (informAll debug) $ putIOwords ["\n texsnip2tex dyIndexEntry"
        , showT (dyIndexEntry yam)]
    when (informAll debug) $ putIOwords ["\n texsnip2tex latexparam"
        , showT latexparam]
    latexparam4 <- if "booklet" == latBook  latexparam
        then do 
            let latexparam2 = latexparam{latIndex=dyIndexEntry yam}
            latexparam3 <- completeIndexWithContent debug bakedP latexparam2
            return latexparam3
        else if "bookbig" == latBook  latexparam
        then do 
            let latexparam4 = latexparam{latIndex=dyIndexEntry yam}
            latexparam5 <- completeIndexWithContent2nd debug bakedP latexparam4
            return latexparam5
        else return latexparam  

    when (informAll debug) $ putIOwords ["\n texsnip2tex latexparam4 completed with content"
        , showT latexparam4]

    when (informAll debug) $ putIOwords ["\n texsnip2tex latexparam4 completed with content"
        , showT latexparam4]


    res2 <- tex2latex debug webroot latexparam4 latexDtpl 
    when (inform debug) $ putIOwords ["texsnip2tex unprocessed texsnip ", showT res2]
   
    -- tex file must be full, ordinary latex content

    when (inform debug) $ putIOwords ["\n texsnip2tex done"]
    return . Latex $ res2

completeIndexWithContent2nd :: NoticeLevel -> Path Abs Dir -> LatexParam -> ErrIO LatexParam
-- | complete a dir of a dir of files 
--      the dir must not contain files, only other dirs
completeIndexWithContent2nd debug bakedP latexparam2 = do 
    let 
        latix2 = latIndex latexparam2 
        dirixs = dirEntries latix2 
    dirixs7 :: [IndexEntry] <- mapM (completeOneIx2nd  debug bakedP) dirixs
    let latix7 = latix2{dirEntries = dirixs7}
    return $ latexparam2{latIndex =   latix7}

completeIndexWithContent :: NoticeLevel -> Path Abs Dir -> LatexParam -> ErrIO LatexParam
completeIndexWithContent debug bakedP latexparam2 = do 
    let 
        latix2 = latIndex latexparam2 
        fileixs = fileEntries latix2 
    fileixs2 <- mapM (completeOneIx debug bakedP)  fileixs
    
    let latix3 = latix2{fileEntries = fileixs2}
    return $ latexparam2{latIndex = latix3}

completeOneIx2nd :: NoticeLevel -> Path Abs Dir -> IndexEntry -> ErrIO IndexEntry
--     -- get the snips for one dir entry 
completeOneIx2nd debug bakedP ix = do 
    when (inform debug) $ putIOwords ["\n completeOneIx2nd start", showT ix]

    let fileixs = dirEntries ix 
    dirs <- mapM (completeOneIx debug bakedP)  fileixs

    let ix4 = ix{dirEntries = dirs}
    return ix4
--     let
--         ln = makeRelFile . link $ ix 
--         lnfp = bakedP </> ln :: Path Abs File 

--     texsnip1 :: TexSnip <-   read8 lnfp texSnipFileType 
--     -- let res = unlines' [zero, titsnip, "", abssnip, "", unTexSnip texsnip1]
--     let ix2 = ix{content =  unTexSnip texsnip1}
--     when (inform debug) $ putIOwords ["\n completeOneIx2nd end", showT ix2]
--     return ix2

completeOneIx :: NoticeLevel -> Path Abs Dir -> IndexEntry -> ErrIO IndexEntry
    -- get the snip for one index entry 
    -- only the content (abstract and title collected before) 
completeOneIx debug bakedP ix = do 
    when (inform debug) $ putIOwords ["\n completeOneIx start", showT ix]
    let
        ln = makeRelFile . link $ ix 
        lnfp = bakedP </> ln :: Path Abs File 

    texsnip1 :: TexSnip <-   read8 lnfp texSnipFileType 
    -- let res = unlines' [zero, titsnip, "", abssnip, "", unTexSnip texsnip1]
    let ix2 = ix{content =  unTexSnip texsnip1}
    when (inform debug) $ putIOwords ["\n completeOneIx end", showT ix2]
    return ix2




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
