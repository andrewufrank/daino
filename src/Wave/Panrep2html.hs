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
import Wave.Docrep2panrep
import Wave.Md2doc
import System.FilePath (replaceExtension)

-- ------------------------------------ panrep2html
-- panrep2html :: Panrep -> ErrIO HTMLout
-- implements the bake
-- siteHeader (sett3, above sett3) is the content of the settingsN.yml file
-- added here the transformations to tufte sidenotes (from pandoc-sidenotes)

panrep2html :: NoticeLevel -> Panrep -> ErrIO (HTMLout, [FilePath])
panrep2html debug   metaplus4 = do
    let sett3 = sett metaplus4
        extra4 = extra metaplus4
        mf = masterTemplateFile $ siteLayout sett3
        masterfn = templatesDir (siteLayout sett3) </> mf

    putIOwords ["\npanrep2html", "siteLayout sett3", showPretty $ siteLayout sett3]
    putIOwords ["panrep2html", "mf", showPretty mf]
    putIOwords ["panrep2html", "masterfn", showPretty masterfn]

    htmlTempl  <- compileTemplateFile2 masterfn

    -- htm1 <- meta2xx writeHtml5String2 (metap metaplus4)

    --if this is an index file it has files and dirs 
    putIOwords ["panrep2html", "extra4", showPretty extra4]
    
    let files = fileEntries  $ extra4 :: [IndexEntry2]
        dirs = dirEntries  $ extra4 :: [IndexEntry2]

    -- calculate needs 
    let 
        bakedP =   bakedDir . siteLayout $ sett3
        bakedFP = toFilePath bakedP
        needs = map (flip replaceExtension "panrep") . map (addDir bakedFP ) .  map link $ (dirs ++ files) 
                     :: [FilePath] 

    putIOwords ["panrep2html", "\n\tneeds ", showPretty needs ]


    -- let dirs2 = map makeAbsFile ds
    --     files2 = map makeAbsFile fs
    -- panDirs <- mapM (get4panrepsDir debug) dirs2 
    -- panfiles <- mapM (get4panrepsFile debug) files2 

    valsDirs :: [Maybe IndexEntry2]<- mapM (getVals2 debug bakedP) dirs 
    valsFiles :: [Maybe IndexEntry2] <- mapM (getVals2 debug bakedP) files  

    putIOwords ["panrep2html", "valsDirs", showPretty valsDirs]
    putIOwords ["panrep2html", "valsFiles", showPretty valsFiles]

    

    let extra5 = extra4{fileEntries = catMaybes valsFiles
                        , dirEntries = catMaybes valsDirs}
    let metaplus5 = metaplus4{extra = extra5} 
-- copied
    -- htpl2 <- compileTemplateFile2 metaplusHtml -- fnminilatex
    let hpl1 = renderTemplate htmlTempl (toJSON metaplus5)  -- :: Doc Text
    -- putIOwords ["tpl1 \n", showT tpl1]
    let ht1 = render (Just 50) hpl1  -- line length, can be Nothing
    -- putIOwords ["res1 \n", res1]
    -- write8   fnPlusres htmloutFileType (HTMLout ht1)


-- 
    -- hres <- meta2hres htmlTempl metaplus4
    when (informAll debug) $ putIOwords ["panrep2html render html done"
        , "hres", ht1
        ]
    -- bakeOnePanrep2html will write to disk
    return (HTMLout ht1, needs)

getVals2 :: NoticeLevel -> Path Abs Dir -> IndexEntry2 
                -> ErrIO (Maybe IndexEntry2)
-- get the panrep and fill the vals 
getVals2 debug bakedP ix2 = do
    let fn = makeAbsFile $ addDir (toFilePath bakedP) (link ix2)  :: Path Abs File
    pan1 <- read8 fn panrepFileType

    let m = metaHtml pan1
        ix3 = ix2   { abstract = lookup7 "abstract" m
                    , title = lookup7 "title" m 
                    -- , author = lookup7 "author" m -- todo suppressed?
                    ,     date = lookup7 "date" m
                    -- , sortOrder = lookup7 "sortOrder" m
                    , version = lookup7 "version" m 
                    , visibility = lookup7 "visibility" m
                -- todo complete 
                    }
    return $ if includeBakeTest3 def -- bring down 
                            (version ix3) (visibility ix3) 
                then Just ix3 else Nothing 

  
        
        
        -- incl = includeBakeTest3 (def:PubFlags) bring down 
                            -- (version ix3) (visibility ix3)


lookup7 :: Text -> M.Map Text Text ->  Text
lookup7 k m = fromJustNoteT ["lookup7 in panrep2html", k, showT m] 
            . M.lookup k $ m

-- data IxRec = IxRec {ixTitle :: Text
--                     , ixAbstract :: Text
--                     , ixDate :: Text 
--                     , ixAuthor :: Text 
--                     , ixSortOrder :: Text
--                     , ixVersion :: Text
--                     }
--     deriving (Show, Read, Ord, Eq, Generic, Zeros)


--     let mf = masterTemplateFile $ siteLayout sett3
--     -- let mfn = templatesDir layout </> mf
--     let masterfn = templatesDir (siteLayout sett3) </> mf
--     let h = dyHeaderShift m1
--     when (inform debug) $
--         putIOwords ["\n\t---------------------------panrep2html"
--                 , "shiftHeaderLevel"
--                 , showT h]    
--     let p2 = P.headerShift h p1
--     let p3 = usingSideNotes p2  -- :: Pandoc -> Pandoc
--     vals <- panrep2vals  debug sett3 (Panrep m1 p3)
--     p :: HTMLout <- panrep2html2 debug masterfn vals
--     return p

-- panrep2vals ::  NoticeLevel -> Settings -> Panrep -> ErrIO [Value]
-- panrep2vals debug sett3 (Panrep m1 p1) = do
--     let ixe1 = dyIndexEntry m1
--     let indexSortField = Data.Maybe.fromMaybe "" (dyIndexSort m1)

--     when (inform debug) $
--         putIOwords ["\n\t---------------------------panrep2vals"
--                 , "AuthorOppressed"
--                 , showT (blogAuthorToSuppress . siteLayout $ sett3)]

--     menu4 :: MenuEntry <- convertIndexEntries  debug (blogAuthorToSuppress.siteLayout $ sett3) indexSortField ixe1
--     html <- writeHtml5String2 p1
--     -- in uniform.Pandoc (dort noch mehr moeglicherweise duplicated)
--     p2 <-  fillContent ixe1 html

--     when (inform debug) $ putIOwords ["panrep2vals", "m1", showPretty m1]
--     when (inform debug) $ putIOwords ["panrep2vals", "sett3", showPretty sett3]
--     when (inform debug) $ putIOwords ["panrep2vals", "menu4", showPretty menu4]
--     when (inform debug) $ putIOwords ["panrep2vals", "p2", showPretty p2]

--     let vals = [toJSON sett3, toJSON m1, toJSON menu4, toJSON p2]
--     -- m1 is what is from the yaml meta from the file
--     -- menu4 is menu collected 
--     -- order matters left preference?

--     when (inform debug) $ putIOwords ["panrep2vals", "vals", showPretty vals]
--     return vals

-- panrep2html2 :: NoticeLevel -- ^ 
--   -> Path Abs File -- ^ 
--   -> [Value] -- ^ 
--   -> ErrIO HTMLout
-- panrep2html2 debug masterfn vals = do
--     p :: HTMLout <- putValinMaster debug vals masterfn
--     when (inform debug) $ putIOwords ["\n panrep2html done"]
--     return p

-- fillContent :: IndexEntry -> Text -> ErrIO ContentHtml
-- fillContent ix cont = do 
--         today1 :: UTCTime <- getCurrentTimeUTC
--         let res = ContentHtml
--                 { content3 = cont 
--                 , today3 = showT today1
--                 , linkpdf3 = convertLink2pdf ix   
--                 , filename3 = convertLink2html ix
--                 }
--         return res

-- data ContentHtml = ContentHtml  
--         { content3 :: Text
--         , today3 :: Text 
--         , linkpdf3 :: Text 
--         , filename3 :: Text 
--         } deriving (Show, Generic)
-- -- | the record which contains the blog text in html format 
-- -- the ref to the pdf File 
-- -- todays date 
-- -- filename3 the original file name 
-- -- mit id,h1, h2,.. span und p tags 

-- instance ToJSON ContentHtml
-- instance Zeros ContentHtml where
--   zero = ContentHtml zero zero zero zero
