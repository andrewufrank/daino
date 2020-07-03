------------------------------------------------------
-- Module DocRep  (which is pandoc and metarec 

    -- originally copied from Slick  because it concentrates all funny pandoc stuff here (including the
    -- writing of the json

----------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -fno-warn-dodgy-exports #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans
 -fno-warn-missing-signatures
 -fno-warn-missing-methods 
-fno-warn-duplicate-exports
-fno-warn-unused-imports 
-fno-warn-unused-matches 
#-}
module Lib.Pandoc
    ( module Lib.Pandoc
    , getMeta
    -- , docValToAllVal
    -- , markdownToPandocBiblio
    -- , pandocToContentHtml
    , getAtKey
    , Pandoc(..)
    , flattenMeta
    , readMarkdown2
    , HTMLout(..)
    , htmloutFileType
    -- , MenuEntry
    )
where

import           Lib.Foundation                 ( SiteLayout(..) )
-- import           Lib.Indexing -- (MarkdownText(..), unMT, HTMLout(..), unHTMLout
-- import           Paths_SSG (version)
import           Uniform.Convenience.DataVarious
                                                ( showVersionT )
import           Uniform.FileIO          hiding ( Meta
                                                , at
                                                )
import           Uniform.HTMLout               
    --  ( HTMLout(..)
    --                                             , writeHtml5String2
    --                                             , htmloutFileType
    --                                             )
-- import           Uniform.DocValue               ( mergeAll )
import           Uniform.Markdown               ( readMarkdown2 )
import           Uniform.Pandoc -- hiding (Meta(..))
import           Uniform.BibTex
import           Uniform.Time                   ( year2000 )
-- import           Lib.CheckInput                 ( MetaRec(..)
--                                                 , TripleDoc
--                                                 )
import           Lib.CmdLineArgs                ( PubFlags(..) )

-------------------
docRepNeeds2 :: Path Abs File -> ErrIO [FilePath]
-- get the needs that are visible in the file 
-- fix the path? TODO 
docRepNeeds2 fnx = do 
        dr1 <- read8 fnx docRepFileType
        let n1 = docRepNeeds dr1 
        return n1

docRepNeeds :: DocRep -> [FilePath]
-- ^ collect the needs (bib, images, css?)
docRepNeeds (DocRep y1 p1) =   map t2s . catMaybes $ [imgs, bibs]
    where
        imgs = getAtKey y1 "image"  :: Maybe Text
        bibs = getAtKey y1 "bibliography"  :: Maybe Text
        -- TODO should list be images? 


-- moved to uniform-pandoc 
-- -- | Convert markdown text into a 'Value';
-- -- The 'Value'  has a "content" key containing rendered HTML
-- -- Metadata is assigned on the respective keys in the 'Value'
-- -- includes reference replacement (pandoc-citeproc)
-- -- runs in the pandoc monad!

-- markdownToPandocBiblio
--     :: Bool -> PubFlags -> Path Abs Dir -> DocRep -> ErrIO DocRep

-- process the markdown (including if necessary the BibTex treatment)
-- the bibliography must be in the metadata
-- the settings are in the markdownText (at end - to let page specific have precedence)


-- -- the filename is in the record, if needed 
--     -- the current dir is necessary to be set - same as tex2pdf issues!
-- markdownToPandocBiblio debug flags doughP (DocRep yam pan) = do
--     when debug $ putIOwords ["markdownToPandocBiblio", showT metaRec]
--     pandoc2 <- case (docBibliography yam) of
--         Nothing    -> return pandoc
--         Just bibfp -> do
--             when debug $ putIOwords
--                 [ "markdownToPandocBiblio"
--                 , "start pandocProcessCites"
--                 , showT doughP
--                 , showT bibfp
--                 , showT (docBibliography yam)
--                 ]
--             pandocProcessCites doughP  -- required to set the current dir 
--                                (makeAbsFile bibfp) -- (doughP </> (makeRelFile . t2s $ bibfp))
--                                (bibliographyGroup metaRec)
--                                pandoc
--            -- here the dir is used for processing in my code
--     return pandoc2

-- pandocToContentHtml :: Bool -> Pandoc -> ErrIO HTMLout

-- -- convert the pandoc to html in the contentHtml key
-- -- the settings are initially put into the pandoc
-- pandocToContentHtml debug pandoc2 = do
--     text2x <- writeHtml5String2 pandoc2
--     return text2x

-- docValToAllVal :: Bool
--                 -> SiteLayout 
--                -> PubFlags
--                -> HTMLout
--               --  -> Path Abs File
--             --    -> Path Abs Dir
--                -> MetaRec
--                -> ErrIO DocValue

-- -- from the docVal of the page
-- -- get the pageType and the settings (master) values
-- -- and combine them
-- -- the current file is only necessary if it is an index file
-- -- then to determine the current dir
-- -- and to exclude it from index
-- docValToAllVal debug layout flags htmlout   metaRec = do
--     let pageFn = makeAbsFile . fn $ metaRec 
--     let pageType = makeAbsFile $ 
--                 pageTemplate metaRec ::  (Path Abs File)
--     when debug $ putIOwords ["docValToAllVal"] 
--         -- , "mpt", showT mpageType]

--     when debug $ putIOwords [
--         "docValToAllVal filename", showT pageFn
--         , "\npageType\t" , showT pageType
--         , "\nsettings\t"    , showT (settingsFile flags)]

--     pageTypeYaml <- readYaml2value pageType
--     settingsYaml <- readYaml2value (settingsFile flags)

--     when debug $ putIOwords [
--         "pandoc pagetype", showT pageTypeYaml
--         , "\nsettingsYaml", showT settingsYaml
--         ,"\n---1"] 

--     --  svalue <- decodeThrow . t2b . unYAML $ settings

--     ix :: MenuEntry <- makeIndex debug layout flags metaRec  
--     when debug $ putIOwords [
--         "docValToAllVal pandoc index produced", showT ix
--         ,"\n---2"]   

--     fn2 <- stripProperPrefix' (doughDir layout) pageFn
--     let bottomLines = BottomLines
--             { filename = showT fn2
--             , ssgversion = "3.0" -- showVersionT version
--             , today = showT year2000
--             -- TODO avoids changes during debug
--             }
--     when debug
--         $ do  putIOwords ["docValToAllVal pandoc settings2.yaml"
--                             , showT settingsYaml]
--     let val = mergeAll
--             [ settingsYaml
--             , pageTypeYaml
--             -- , unDocValue docval
--             , toJSON htmlout
--             , toJSON ix
--             , toJSON bottomLines]
--     when debug
--         $ do  putIOwords ["docValToAllVal val"
--                             , showT val]
--     return val

data BottomLines =
  BottomLines { ssgversion :: Text
              , today :: Text -- ^ the data when converted(baked)
              , filename :: Text
              }
  deriving (Generic, Read, Show, Eq, Ord)

instance ToJSON BottomLines
