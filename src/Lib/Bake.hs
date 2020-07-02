---------------------------------------------------------------------------
--
-- Module      :   the  process to convert
--              files from md to all the formats required 
--              orginals are found in dire doughDir and go to bakeDir
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports 
            -fno-warn-unused-imports 
            -fno-warn-unused-matches 
            #-}

-- {-# LANGUAGE TypeSynonymInstances  #-}
module Lib.Bake
    ( module Lib.Bake
    , bakeOneFile2docrep
    -- , bakeOneFile2html
    -- , bakeOneFile2texsnip
    -- , bakeOneTexSnip2pdf
    -- , bakeDocValue2html
    )
where

import           Uniform.FileStrings            ( ) -- for instances
import           Uniform.Filenames
import           Uniform.FileIO                 ( read8
                                                , write8
                                                , copyOneFileOver
                                                )

import           Uniform.Shake                --  ( replaceExtension' )
import           Uniform.DocRep
import           Uniform.Pandoc
            --  ( writeTexSnip2
            --                                     , TexSnip
            --                                     , texSnipFileType
            --                                     , extTexSnip
            --                                     )
-- import           Uniform.DocValue               ( docValueFileType
--                                                 , docvalExt
--                                                 )
-- todo - check replaceextension in fileio 
-- import           Lib.Pandoc                   
--   ( markdownToPandocBiblio
--                                                 , pandocToContentHtml
--                                                 , htmloutFileType
--                                                 , HTMLout(..)
--                                                 )

-- import           Lib.Templating                 ( putValinMaster )
import           Uniform.ProcessPDF
    -- (writePDF2text, extPDF, pdfFileType, texFileType,  extTex, Latex(..),tex2latex)
-- import           Uniform.Pandoc                 ( Pandoc
--                                                 , write8
--                                                 )
import           Lib.CmdLineArgs                ( PubFlags(..) )
import           Lib.CheckInput --                 ( getTripleDoc )
import           Lib.Foundation                 ( SiteLayout(..)
                                                , templatesDir
                                                )
import           Lib.Indexing
import qualified Path.IO                       as Path
                                                ( getTempDir )

type BakeOp
    =  Bool
    -> PubFlags
    -> Path Abs File  -- ^ md file 
    -> SiteLayout
    -> Path Abs File
    -> ErrIO ()

bakeOneFile2docrep      --    MD -> DOCREP
    :: Path Abs Dir -- ^ the bakedP 
    -> Bool
    -> PubFlags
    -> Path Abs File  -- ^ md file 
    -> SiteLayout
    -> Path Abs File
    -> ErrIO () -- return the needed files 
-- convert a md file, process citations if any
-- produce the docval (from which html texsnip are derived)
-- todo include the index 

bakeOneFile2docrep bakedP debug flags  inputFn layout resfn2 = do
    putIOwords
        [ "\n-----------------"
        , "bakeOneFile2docrep 1 fn"
        , showT inputFn
        , "debug"
        , showT debug
        , "\n resfn2"
        , showT resfn2
        ]

    md1 <- read8 inputFn markdownFileType

    -- readMarkdown2docrep :: MarkdownText -> ErrIO DocRep
-- | read a md file into a DocRep
-- all values from meta are moved to yam (meta is zero to avoid problems)
    dr1 <- readMarkdown2docrep md1

-- check 
-- the fields for the index are prepared 

    dr2 <- checkDocRep bakedP inputFn dr1
    -- does this use the listed refs? 
    dr3 <- addRefs dr2

    -- TODO needs refs 
    -- let needs1  = docRepNeeds docrep1  :: [FilePath]
    -- need  needs1  -- TDO this is in the wrong monad
    -- dr4 <- addIndex2yam debug dr3
    -- this will be done twice in html and tex

    write8 resfn2 docRepFileType dr3
    when debug $ putIOwords
        [ "\n-----------------"
        , "bakeOneFile2docrep done fn"
        , showT resfn2
            -- , "\n needs returned", showT needs1
        ]
    return () -- (needs1) --"ok bakeOneFile2docrep"

bakeOneFile2panrep :: BakeOp   --  DOCREP -> PANREP
-- TODO 
bakeOneFile2panrep debug flags inputFn layout resfn2 = do
    putIOwords
        [ "\n-----------------"
        , "bakeOneFile2panrep 1 fn"
        , showT inputFn
        , "debug"
        , showT debug
        , "\n resfn2"
        , showT resfn2
        ]

    dr1 <- read8 inputFn docRepFileType

    p1 <- docRep2panrep dr1     -- adds refs!   
        -- do index 
    -- p2 <- addIndex2yam debug p1  

    let p2=p1

    write8 resfn2 panrepFileType p2  -- content is html style

    when debug $ putIOwords
        ["\n-----------------", "bakeOneFile2panrep done fn", showT p2]
    return () --"ok bakeOneFile2docrep"

 

bakeOneFile2html :: BakeOp  --  PANREP -> HTML
-- TODO 
bakeOneFile2html debug flags inputFn layout resfn2 = do
    putIOwords
        [ "\n-----------------"
        , "bakeOneFile2html 1 fn"
        , showT inputFn
        , "debug"
        , showT debug
        , "\n resfn2"
        , showT resfn2
        ]

    dr1 <- read8 inputFn panrepFileType
        -- docRep2html:: DocRep -> ErrIO HTMLout
        -- ^ transform a docrep to a html file 
        -- needs teh processing of the references with citeproc

        -- do index 
    -- dr4 <- addIndex2yam debug dr1  -- move to 

    h1  <- panrep2html dr1

    write8 resfn2 htmloutFileType h1   -- content is html style

    when debug $ putIOwords
        ["\n-----------------", "bakeOneFile2html done fn", showT resfn2]
    return () --"ok bakeOneFile2docrep"


bakeOneFile2texsnip :: BakeOp  --  PANREP -> TEXSNIP
-- TODO 
bakeOneFile2texsnip debug flags inputFn layout resfn2 = do
    putIOwords
        [ "\n-----------------"
        , "bakeOneFile2texsnip 1 fn"
        , showT inputFn
        , "debug"
        , showT debug
        , "\n resfn2"
        , showT resfn2
        ]

    dr1   <- read8 inputFn panrepFileType

    -- docRep2texsnip :: DocRep -> ErrIO TexSnip
    -- -- ^ transform a docrep to a texsnip 
    -- -- does not need the references include in docRep
    -- -- which is done by tex to pdf conversion

    snip1 <- panrep2texsnip dr1

    write8 resfn2 texSnipFileType snip1   -- content is html style

    when debug $ putIOwords
        ["\n-----------------", "bakeOneFile2html done fn", showT resfn2]
    return () --"ok bakeOneFile2docrep"


bakeOneFile2tex :: BakeOp  -- TEXSNIP -> TEX 
-- TODO 
bakeOneFile2tex debug flags inputFn layout resfn2 = do
    putIOwords
        [ "\n-----------------"
        , "bakeOneFile2tex 1 fn"
        , showT inputFn
        , "debug"
        , showT debug
        , "\n resfn2"
        , showT resfn2
        ]

    snip1 <- read8 inputFn texSnipFileType

    
    -- latexParam :: LatexParam <- fromJSONerrio . snipyam $ snip1
    -- putIOwords ["bakeOneFile2tex", showT latexParam]
    let tex1 =  tex2latex zero [snip1]

    write8 resfn2 texFileType tex1   -- content is html style

    when debug $ putIOwords
        ["\n-----------------", "bakeOneFile2tex done fn", showT resfn2]
    return () --"ok bakeOneFile2docrep"


bakeOneFile2pdf :: BakeOp
-- TODO 
bakeOneFile2pdf debug flags inputFn layout resfn2 = do
    putIOwords
        [ "\n-----------------"
        , "bakeOneFile2pdf 1 fn:"
        , showT inputFn
        , "\n\t debug:"
        , showT debug
        , "\n\t resfn2:"
        , showT resfn2
        ]

    -- tex1     <- read8 inputFn texFileType

    -- docRep2texsnip :: DocRep -> ErrIO TexSnip
    -- -- ^ transform a docrep to a texsnip 
    -- -- does not need the references include in docRep
    -- -- which is done by tex to pdf conversion

    -- writePDF2text :: Bool  ->   Path Abs File -> Path Abs File -> ErrIO ()
    let refDir =
            makeAbsDir . getParentDir . toFilePath $ inputFn :: Path Abs Dir
    writePDF2 debug inputFn resfn2 refDir  -- content is html style

    when debug $ putIOwords
        ["\n-----------------", "bakeOneFile2pdf done fn", showT resfn2]
    return () --"ok bakeOneFile2docrep"

