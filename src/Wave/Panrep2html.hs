---------------------------------------------------------------------
--
-- Module      :  Uniform.Doc2html
--  converts an md document in 2steps 
--      docrep -> panrep
        --     includes preparing of index pages 
        --     the processsing of the refs are already done in doc processing 
        -- panrep -> html 
---------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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


module Wave.Panrep2html (
    module Wave.Panrep2html,
) where

-- import Data.Default
import Foundational.Filetypes4sites
import Foundational.SettingsPage
    -- ( Settings(siteLayout), SiteLayout(blogAuthorToSuppress) )
import Foundational.CmdLineFlags

import GHC.Generics (Generic)

import Uniform.Json ( ToJSON(toJSON), Value, ErrIO )
import Uniform.Pandoc
-- import Uniform.Latex 
-- import qualified Text.Pandoc.Shared as P
import Uniform.Http ( HTMLout (HTMLout) )
import UniformBase
import Uniform.MetaPlus hiding (MetaPlus(..), Settings(..), ExtraValues(..))
import Wave.Md2doc 
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
-- import Wave.Docrep2panrep
-- import Wave.Md2doc
import System.FilePath (replaceExtension)
import Uniform.Shake  
import Path (addFileExtension, addExtension)

default (Integer, Double, Text)

testTemplateFn = makeAbsFile "/home/frank/Workspace11/daino/tests/data/metaplusHtml.dtpl"
-- ------------------------------------ panrep2html
-- panrep2html :: Panrep -> ErrIO HTMLout
-- implements the bake
-- siteHeader (sett3, above sett3) is the content of the settingsN.yml file
-- added here the transformations to tufte sidenotes (from pandoc-sidenotes)
-- compiles the test template and fills 
-- returns the result to be written by bake.hs

panrep2html :: NoticeLevel -> PubFlags -> Panrep -> ErrIO (HTMLout, [FilePath], Text)
panrep2html debug pubFlags  metaplus4 = do
    let debug = NoticeLevel0   -- avoid_output_fromHere_down
    let sett3 = sett metaplus4
        extra4 = extra metaplus4
        mf = masterTemplateFile $ siteLayout sett3
        masterfn = templatesDir (siteLayout sett3) </> mf

    putInform debug["\npanrep2html", "siteLayout sett3"
                , showPretty $ siteLayout sett3]
    putInform debug ["panrep2html", "mf", showPretty mf]
    putInform debug ["panrep2html", "masterfn", showPretty masterfn]

    targetTempl  <- compileTemplateFile2 masterfn
    testTempl  <- compileTemplateFile2 testTemplateFn

    -- htm1 <- meta2xx writeHtml5String2 (metap metaplus4)

    --if this is an index file it has files and dirs 
    putInform debug ["panrep2html", "extra4", showPretty extra4]

    let files = fileEntries  $ extra4 :: [IndexEntry2]
        dirs = dirEntries  $ extra4 :: [IndexEntry2]

    let bakedP =   bakedDir . siteLayout $ sett3
 
    valsDirs :: [Maybe IndexEntry2]<- mapM 
                    (getVals2 debug pubFlags bakedP) dirs
    valsFiles :: [Maybe IndexEntry2] <- mapM 
                    (getVals2 debug pubFlags bakedP) files

    putInform debug["panrep2html", "valsDirs", showPretty valsDirs]
    putInform debug ["panrep2html", "valsFiles", showPretty valsFiles]

    let extra5 = extra4{fileEntries = catMaybes valsFiles
                        , dirEntries = catMaybes valsDirs}
    let metaplus5 = metaplus4{extra = extra5}

   -- calculate needs 
    let
        -- bakedP =   bakedDir . siteLayout $ sett3
        bakedFP = toFilePath bakedP
        allixs =  catMaybes $ valsFiles ++ valsDirs :: [IndexEntry2]
        needs = map (<.> "panrep") -- (`replaceExtension` "panrep")
                . map (addDir bakedFP )
                .  map ixfn $ allixs
                     :: [FilePath]
    putInform NoticeLevel2 ["panrep2html allixs ixfn"
                    , showT .map (<.> "panrep") . map (addDir bakedFP ) . map ixfn $ allixs]
    putInform NoticeLevel2 ["panrep2html allixs link"
                    , showT . map (addDir bakedFP ) . map (<.> "panrep") . map ixfn $ allixs]
    when ((informAll debug) && (needs /= []) )$
            putIOwords ["panrep2html", "needs ", showT needs ]

    putInform debug ["panrep2html", "extra5", showPretty extra5]
    putInform debug ["panrep2html", "metaplus5", showPretty metaplus5]

    let hpl1 = renderTemplate targetTempl (toJSON metaplus5)  -- :: Doc Text
    let ht1 = render (Just 50) hpl1  -- line length, can be Nothing

    let ttpl1 = renderTemplate testTempl (toJSON metaplus5)  -- :: Doc Text
    let tt1 = render (Just 50) ttpl1  -- line length, can be Nothing

    putInform debug ["panrep2html render html done"
        , "ht1",  ht1
        ]
    putInform debug ["panrep2html render testTemplate done"
        , "tt1",  tt1
        ]

    -- bakeOnePanrep2html will write to disk
    return (HTMLout ht1, needs, tt1)

getVals2 :: NoticeLevel -> PubFlags -> Path Abs Dir -> IndexEntry2
                -> ErrIO (Maybe IndexEntry2)
-- get the panrep and fill the vals 
getVals2 debug pubFlags bakedP ix2 = do
    putInform debug ["GetVals2 ix2", showPretty ix2]    
    let fn = makeAbsFile fnix2
        fnix2 =  fnix3 <.> "panrep"  :: FilePath
        fnix3 = addDir (toFilePath bakedP) fnix4 :: FilePath
        fnix4 = (ixfn ix2) :: FilePath
        pdf = replaceExtension2 ".pdf" fn 
    pan1 <- read8 fn panrepFileType

    let m = metaHtml pan1
        ix3 = ix2   { abstract = lookup7 "abstract" m
                    , title = lookup7 "title" m
                    -- , author = lookup7 "author" m -- todo suppressed?
                    ,     date = lookup7 "date" m
                    -- , sortOrder = lookup7 "sortOrder" m
                    , version = lookup7 "version" m
                    , visibility = lookup7 "visibility" m
                    , pdf1 = s2t $ toFilePath pdf 
                -- todo complete 
                    }
    return $ if includeBakeTest3 pubFlags (version ix3) (visibility ix3)
                then Just ix3 else errorT ["getVals2 in panrep2html not included", showT ix2 ]



        -- incl = includeBakeTest3 (def:PubFlags) bring down 
                            -- (version ix3) (visibility ix3)


lookup7 :: Text -> M.Map Text Text ->  Text
lookup7 k m = fromJustNoteT ["lookup7 in panrep2html", k, showT m]
            . M.lookup k $ m

lookup7withDef  :: Text -> Text -> M.Map Text Text -> Text
--get the Metavalue (with  default)
lookup7withDef def1 key m =  fromMaybe def1 $ M.lookup key m
  