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
import Uniform.Shake  

default (Integer, Double, Text)

testTemplateFn = makeAbsFile "/home/frank/Workspace11/daino/tests/data/metaplusHtml.dtpl"
-- ------------------------------------ panrep2html
-- panrep2html :: Panrep -> ErrIO HTMLout
-- implements the bake
-- siteHeader (sett3, above sett3) is the content of the settingsN.yml file
-- added here the transformations to tufte sidenotes (from pandoc-sidenotes)
-- compiles the test template and fills 
-- returns the result to be written by bake.hs

panrep2html :: NoticeLevel -> Panrep -> ErrIO (HTMLout, [FilePath], Text)
panrep2html debug   metaplus4 = do
    let sett3 = sett metaplus4
        extra4 = extra metaplus4
        mf = masterTemplateFile $ siteLayout sett3
        masterfn = templatesDir (siteLayout sett3) </> mf

    when (inform debug) $ do
            putIOwords ["\npanrep2html", "siteLayout sett3"
                , showPretty $ siteLayout sett3]
            putIOwords ["panrep2html", "mf", showPretty mf]
            putIOwords ["panrep2html", "masterfn", showPretty masterfn]

    targetTempl  <- compileTemplateFile2 masterfn
    testTempl  <- compileTemplateFile2 testTemplateFn

    -- htm1 <- meta2xx writeHtml5String2 (metap metaplus4)

    --if this is an index file it has files and dirs 
    when (inform debug) $
            putIOwords ["panrep2html", "extra4", showPretty extra4]

    let files = fileEntries  $ extra4 :: [IndexEntry2]
        dirs = dirEntries  $ extra4 :: [IndexEntry2]

    -- calculate needs 
    let
        bakedP =   bakedDir . siteLayout $ sett3
        bakedFP = toFilePath bakedP
        needs = map (`replaceExtension` "panrep")
                . map (addDir bakedFP )
                .  map link $ (dirs ++ files)
                     :: [FilePath]

    when (inform debug) $
            putIOwords ["panrep2html", "\n\tneeds ", showPretty needs ]

    valsDirs :: [Maybe IndexEntry2]<- mapM (getVals2 debug bakedP) dirs
    valsFiles :: [Maybe IndexEntry2] <- mapM (getVals2 debug bakedP) files

    when (informAll debug) $ do
            putIOwords ["panrep2html", "valsDirs", showPretty valsDirs]
            putIOwords ["panrep2html", "valsFiles", showPretty valsFiles]

    let extra5 = extra4{fileEntries = catMaybes valsFiles
                        , dirEntries = catMaybes valsDirs}
    let metaplus5 = metaplus4{extra = extra5}
    putIOwords ["panrep2html", "extra5", showPretty extra5]
    when (inform debug) $
            putIOwords ["panrep2html", "metaplus5", showPretty metaplus5]

    let hpl1 = renderTemplate targetTempl (toJSON metaplus5)  -- :: Doc Text
    let ht1 = render (Just 50) hpl1  -- line length, can be Nothing

    let ttpl1 = renderTemplate testTempl (toJSON metaplus5)  -- :: Doc Text
    let tt1 = render (Just 50) ttpl1  -- line length, can be Nothing

    when (inform debug) $ putIOwords ["panrep2html render html done"
        , "ht1",  ht1
        ]
    when (informAll debug) $ putIOwords ["panrep2html render testTemplate done"
        , "tt1",  tt1
        ]

    -- bakeOnePanrep2html will write to disk
    return (HTMLout ht1, needs, tt1)

getVals2 :: NoticeLevel -> Path Abs Dir -> IndexEntry2
                -> ErrIO (Maybe IndexEntry2)
-- get the panrep and fill the vals 
getVals2 debug bakedP ix2 = do
    let fn = makeAbsFile $ addDir (toFilePath bakedP) (link ix2)  :: Path Abs File
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
    return $ if True -- includeBakeTest3 def -- bring down 
                            -- (version ix3) (visibility ix3)
                then Just ix3 else Nothing




        -- incl = includeBakeTest3 (def:PubFlags) bring down 
                            -- (version ix3) (visibility ix3)


lookup7 :: Text -> M.Map Text Text ->  Text
lookup7 k m = fromJustNoteT ["lookup7 in panrep2html", k, showT m]
            . M.lookup k $ m

lookup7withDef  :: Text -> Text -> M.Map Text Text -> Text
--get the Metavalue (with  default)
lookup7withDef def1 key m =  fromMaybe def1 $ M.lookup key m
  