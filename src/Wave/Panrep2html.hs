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
import UniformBase
import Foundational.Filetypes4sites
import Foundational.SettingsPage
    -- ( Settings(siteLayout), SiteLayout(blogAuthorToSuppress) )
import Foundational.CmdLineFlags ( PubFlags )

import GHC.Generics (Generic)

import Uniform.Json ( ToJSON(toJSON), Value, ErrIO )
import Uniform.Pandoc
import Uniform.Http ( HTMLout (HTMLout) )
import Uniform.Shake  
-- import Uniform.MetaPlus  
import Wave.Md2doc ( includeBakeTest3 ) 
import Data.Maybe (fromMaybe)
import Lib.IndexCollect
import qualified Data.Map as M
-- import ShakeBake.Shake2indexes

-- import Wave.Docrep2panrep
-- import Wave.Md2doc
-- import System.FilePath (replaceExtension)
-- import Path (addFileExtension, addExtension)

default (Integer, Double, Text)

testTemplateFn = makeAbsFile "/home/frank/Workspace11/daino/tests/data/metaplusHtml.dtpl"

lookup7 :: Text -> M.Map Text Text ->  Text
lookup7 k m = fromJustNoteT ["lookup7 in panrep2html", k, showT m]
            . M.lookup k $ m

lookup7withDef  :: Text -> Text -> M.Map Text Text -> Text
--get the Metavalue (with  default)
lookup7withDef def1 key m =  fromMaybe def1 $ M.lookup key m


getIndexFiles4meta :: Panrep -> [Path Rel File]
-- get the index files (for dir and files)
getIndexFiles4meta pan = getIndexFiles (f1 ++ d1)
    where
        f1 = fileEntries .  extra  $ pan
        d1 = dirEntries . extra $ pan 

getVals2html :: NoticeLevel -> PubFlags -> Path Abs Dir -> IndexEntry2
                -> ErrIO (Maybe IndexEntry2)
-- get the panrep and fill the vals 
getVals2html debug pubFlags bakedP ix2 = do
    putInformOne debug ["getVals2html  ix2", showPretty ix2]    
    let fnix4 = (ixfn ix2) :: FilePath
        fnix3 = addDir (toFilePath bakedP) fnix4 :: FilePath
        fnix2 =  fnix3 <.> "panrep"  :: FilePath
        fn = makeAbsFile fnix2
        pdf = replaceExtension2 ".pdf" fn 

    putInformOne debug ["getVals2html fn", showT fn ]
    pan1 <- read8 fn panrepFileType
    -- putInformOne debug ["getVals2html pan1", showT pan1 ]

    let m = metap  pan1
        ix3 = ix2   {
                    --  textualMd= zero -- fillTextual m
                    -- , abstract = getTextFromMeta5 ""  "abstract" m
                    -- , title = getTextFromMeta5 "TITLE MISSING" "title" m
                    -- , author = getTextFromMeta5 "" "author" m -- todo suppressed?
                     date = getTextFromMeta5 "2000-01-01" "date" m
                    , sortOrder = getTextFromMeta5 "filename" "sortOrder" m
                    , version = getTextFromMeta5 "draft" "version" m
                    , visibility = getTextFromMeta5 "private" "visibility" m
                    , pdf1 = s2t $ toFilePath pdf 
                    }

    return $ if includeBakeTest3 pubFlags (version ix3) (visibility ix3)
                then Just ix3 else errorT ["getVals2html in panrep2html not included", showT ix2 ]


-- panrep0html_fromIndex :: NoticeLevel -> PubFlags -> Panrep -> ErrIO [FilePath]
-- -- ^ calculate the needs 

-- panrep0html_fromIndex debug pubFlags  metaplus4 = do
--     -- let debug = NoticeLevel0   -- avoid_output_fromHere_down
--     let sett3 = sett metaplus4
--         -- extra4 = extra metaplus4
--         bakeP =  bakedDir . siteLayout $ sett3 :: Path Abs Dir
--         -- mf = masterTemplateFile $ siteLayout sett3
--         -- masterfn = templatesDir (siteLayout sett3) </> mf

--     --if this is an index file it has files and dirs 
--     -- putInformOne debug ["panrep1html", "extra4", showPretty extra4]

--     -- let files = fileEntries  $ extra4 :: [IndexEntry2]
--     --     dirs = dirEntries  $ extra4 :: [IndexEntry2]
--     -- putInformOne debug["panrep0html", "ixfn files", showT $ map ixfn files]
--     -- putInformOne debug ["panrep0html", "ixfn dirs", showT $ map ixfn dirs]


--     let fs1 = getIndexFiles4meta metaplus4 
--         --  getIndexFiles (f1 ++ d1) 
--         -- f1 = (fileEntries .  extra  $ metaplus4)
--         -- d1 = dirEntries . extra $ metaplus4 
--         fs = map (replaceExtension' "html" )   $   fs1 :: [Path Rel File]
--     let fs2needs = map (addFileName bakeP) $  fs
--                         :: [Path Abs File] 
--     putInformOne debug ["panrep0html", "fs2needs", showT fs2needs]
--     return . map toFilePath $ fs2needs



-- -- panrep1html :: NoticeLevel -> PubFlags -> Panrep -> ErrIO [FilePath]
-- -- -- ^ calculate the needs 

-- -- panrep1html debug pubFlags  metaplus4 = do
-- --     -- let debug = NoticeLevel0   -- avoid_output_fromHere_down
-- --     let sett3 = sett metaplus4
-- --         extra4 = extra metaplus4
-- --         -- mf = masterTemplateFile $ siteLayout sett3
-- --         -- masterfn = templatesDir (siteLayout sett3) </> mf

-- --     --if this is an index file it has files and dirs 
-- --     -- putInformOne debug ["panrep1html", "extra4", showPretty extra4]

-- --     let files = fileEntries  $ extra4 :: [IndexEntry2]
-- --         dirs = dirEntries  $ extra4 :: [IndexEntry2]
-- --     putInformOne debug["panrep1html", "ixfn files", showT $ map ixfn files]
-- --     putInformOne debug ["panrep1html", "ixfn dirs", showT $ map ixfn dirs]

-- --     let bakedP =   bakedDir . siteLayout $ sett3
 
-- --     valsDirs :: [Maybe IndexEntry2]<- mapM 
-- --                     (getVals2html debug pubFlags bakedP) dirs
-- --     valsFiles :: [Maybe IndexEntry2] <- mapM 
-- --                     (getVals2html debug pubFlags bakedP) files

-- --     putInformOne debug["panrep1html", "valsDirs", showPretty valsDirs]
-- --     putInformOne debug ["panrep1html", "valsFiles", showPretty valsFiles]

-- --     -- let extra5 = extra4{fileEntries = catMaybes valsFiles
-- --     --                     , dirEntries = catMaybes valsDirs}
-- --     -- let metaplus5 = metaplus4{extra = extra5}

-- --    -- calculate needs 
-- --     let
-- --         -- bakedP =   bakedDir . siteLayout $ sett3
-- --         bakedFP = toFilePath bakedP
-- --         allixs =  catMaybes $ valsFiles ++ valsDirs :: [IndexEntry2]
-- --         needs = map (<.> "panrep") -- (`replaceExtension` "panrep")
-- --                 . map (addDir bakedFP )
-- --                 .  map ixfn $ allixs
-- --                      :: [FilePath]
-- --     putInformOne debug ["panrep1html allixs ixfn"
-- --                     , showT .map (<.> "panrep") . map (addDir bakedFP ) . map ixfn $ allixs]
-- --     putInformOne debug ["panrep1html allixs link"
-- --                     , showT . map (addDir bakedFP ) . map (<.> "panrep") . map ixfn $ allixs]
-- --     when ((inform debug) && (needs /= []) )$
-- --             putIOwords ["panrep1html", "needs ", showT needs ]
-- --     return needs


-- -- -- ------------------------------------ panrep2html
-- -- -- panrep2html :: Panrep -> ErrIO HTMLout
-- -- -- implements the bake
-- -- -- siteHeader (sett3, above sett3) is the content of the settingsN.yml file
-- -- -- added here the transformations to tufte sidenotes (from pandoc-sidenotes)
-- -- -- compiles the test template and fills 
-- -- -- returns the result to be written by bake.hs

-- panrep2html :: NoticeLevel -> PubFlags -> Panrep -> ErrIO (HTMLout, [FilePath], Text)
-- panrep2html debug pubFlags  metaplus4 = do
--     -- let debug = NoticeLevel0   -- avoid_output_fromHere_down
--     let sett3 = sett metaplus4
--         extra4 = extra metaplus4
--         mf = masterTemplateFile $ siteLayout sett3
--         masterfn = templatesDir (siteLayout sett3) </> mf

--     putInformOne debug["\npanrep2html", "siteLayout sett3"
--                 , showPretty $ siteLayout sett3]
--     putInformOne debug ["panrep2html", "mf", showPretty mf]
--     putInformOne debug ["panrep2html", "masterfn", showPretty masterfn]

--     targetTempl  <- compileTemplateFile2 masterfn
--     testTempl  <- compileTemplateFile2 testTemplateFn

--     -- htm1 <- meta2xx writeHtml5String2 (metap metaplus4)

--     --if this is an index file it has files and dirs 
--     -- putInformOne debug ["panrep2html", "extra4", showPretty extra4]

--     let files = fileEntries  $ extra4 :: [IndexEntry2]
--         dirs = dirEntries  $ extra4 :: [IndexEntry2]

--     let bakedP =   bakedDir . siteLayout $ sett3
 
--     valsDirs :: [Maybe IndexEntry2]<- mapM 
--                     (getVals2html debug pubFlags bakedP) dirs
--     valsFiles :: [Maybe IndexEntry2] <- mapM 
--                     (getVals2html debug pubFlags bakedP) files

--     putInformOne debug["panrep2html", "valsDirs", showPretty valsDirs]
--     putInformOne debug ["panrep2html", "valsFiles", showPretty valsFiles]

--     let extra5 = extra4{fileEntries = catMaybes valsFiles
--                         , dirEntries = catMaybes valsDirs}
--     let metaplus5 = metaplus4{extra = extra5}



--     -- putInformOne debug ["panrep2html", "extra5", showPretty extra5]
--     -- putInformOne debug ["panrep2html", "metaplus5", showPretty metaplus5]

--     let hpl1 = renderTemplate targetTempl (toJSON metaplus5)  -- :: Doc Text
--     let ht1 = render (Just 50) hpl1  -- line length, can be Nothing

--     let ttpl1 = renderTemplate testTempl (toJSON metaplus5)  -- :: Doc Text
--     let tt1 = render (Just 50) ttpl1  -- line length, can be Nothing

--     -- putInformOne debug ["panrep2html render html done", "ht1",  ht1 ]
--     -- putInformOne debug ["panrep2html render testTemplate done", "tt1",  tt1 ]

--     -- bakeOnePanrep2html will write to disk
--     return (HTMLout ht1, [], tt1) -- needs are dealt with so far above




--         -- incl = includeBakeTest3 (def:PubFlags) bring down 
--                             -- (version ix3) (visibility ix3)



  