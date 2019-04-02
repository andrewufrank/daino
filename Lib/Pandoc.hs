-- Module copied from Slick
-- i use it because it concentrates all funny pandoc stuff here (including the
-- writing of the json, cannot be imported, because it fixes there the Action monad
-- which i use here as a synonym to ErrIO
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- {-# LANGUAGE TypeSynonymInstances  #-}
module Lib.Pandoc
    ( markdownToPandocBiblio
    , pandocToContentHtml
    , getMeta
    , docValToAllVal
    , getAtKey
    , Pandoc(..)
    , flattenMeta
    , readMarkdown2) where

import           Lib.Foundation (settingsFileName, defaultPageType, SiteLayout(..))
import           Lib.Indexing -- (MarkdownText(..), unMT, HTMLout(..), unHTMLout
--            , unDocValue, DocValue (..) )
import           Paths_SSG (version)
import           Uniform.Convenience.DataVarious (showVersionT)
import           Uniform.FileIO hiding (Meta, at)
import           Uniform.Pandoc -- hiding (Meta(..))
import           Uniform.BibTex
import           Uniform.Json
import           Uniform.Time (getDateAsText, year2000)
import           Lib.CheckInput (MetaRec(..), PublicationState(..)
                                 -- , readMeta2rec
                               , checkOneMdFile, TripleDoc)
import           Lib.CmdLineArgs (PubFlags(..))

-- import Text.Pandoc.Definition (Meta(..))
-- import           GHC.Generics
-- (flattenMeta, getMeta, getAtKey
--                 , putAtKey, readMarkdown2, unPandocM)
-- import Lib.YamlBlocks (readMd2meta, yaml2value, mergeAll, readYaml2value)
-- | Convert markdown text into a 'Value';
-- The 'Value'  has a "content" key containing rendered HTML
-- Metadata is assigned on the respective keys in the 'Value'
-- includes reference replacement (pandoc-citeproc)
-- runs in the pandoc monad!
markdownToPandocBiblio
  :: Bool
  -> PubFlags
  -> Path Abs Dir
  -> TripleDoc
  -> ErrIO (Pandoc)

-- process the markdown (including if necessary the BibTex treatment)
-- the bibliography must be in the metadata
-- the settings are in the markdownText (at end - to let page specific have precedence)
markdownToPandocBiblio debug flags doughP (pandoc, metaRec, _) = do
  -- (pandoc, meta2) <- readMd2meta mdfile
  -- (pandoc, metaRec, report) <- checkOneMdFile doughP templatemdfile
  -- -- let publishTest = getAtKey meta2 "publish" :: Maybe Text
  -- if  -- checkPubStateWithFlags flags (publicationState metaRec)
  --     True -- all md files must produce an output in shake
  --     then 
  -- do
  -- let bib          = getAtKey meta2 "bibliography" :: Maybe Text
  -- let nociteNeeded = getAtKey meta2 "bibliographyGroup" :: Maybe Text
  putIOwords ["markdownToPandocBiblio", showT metaRec]
  pandoc2 <- case (bibliography metaRec) of
    Nothing    -> return pandoc
    Just bibfp -> do 
      when debug $ putIOwords ["markdownToPandocBiblio", "start pandocProcessCites"
                , showT doughP, showT bibfp, showT (bibliographyGroup metaRec)]
      pandocProcessCites
            doughP  -- required to set the current dir 
            (makeAbsFile bibfp) -- (doughP </> (makeRelFile . t2s $ bibfp))
            (bibliographyGroup metaRec)
            pandoc
         -- here the dir is used for processing in my code
  return pandoc2

-- else do
--     -- putIOwords ["markdownToPandoc", "NOT PUBLISH", showT publishTest]
--     return Nothing
--    pandoc   <- readMarkdown2
--    let meta2 = flattenMeta (getMeta pandoc)
--            putIOwords ["markdownToPandoc", "publish", showT publish]
pandocToContentHtml :: Bool -> Pandoc -> ErrIO HTMLout

-- convert the pandoc to html in the contentHtml key
-- the settings are initially put into the pandoc
pandocToContentHtml debug pandoc2 = do
  text2x <- writeHtml5String2 pandoc2
  return text2x

-- let meta2       = flattenMeta (getMeta pandoc2) :: Value
-- let withContent = putAtKey "contentHtml" (unHTMLout text2x) meta2
-- return . DocValue $ withContent
--    ( meta2) & _Object . at "contentHtml" ?~ String (unHTMLout text2)
docValToAllVal :: Bool
                -> SiteLayout 
               -> PubFlags
               -> HTMLout
               -> Path Abs File
            --    -> Path Abs Dir
               -> MetaRec
               -> ErrIO DocValue

-- from the docVal of the page
-- get the pageType and the settings (master) values
-- and combine them
-- the current file is only necessary if it is an index file
-- then to determine the current dir
-- and to exclude it from index
docValToAllVal debug layout flags htmlout pageFn  metaRec = do
  let mpageType = fmap makeAbsFile $ pageTemplate metaRec :: Maybe (Path Abs File)
  when debug $ putIOwords ["docValToAllVal"] -- , "mpt", showT mpageType]
  let pageType = maybe (defaultPageType layout) id mpageType :: Path Abs File
  -- page0default defined in theme
  putIOwords ["docValToAllVal filename", showT pageFn, showT pageType
            , showT (settingsFile flags)]

  pageTypeYaml <- readYaml2value pageType
  settingsYaml <- readYaml2value (settingsFile flags)
  --        svalue <- decodeThrow . t2b . unYAML $ settings
  ix <- makeIndex debug layout flags metaRec pageFn (doughDir layout)
  -- now          <- getDateAsText
  fn2 <- stripProperPrefix' (doughDir layout) pageFn
  let bottomLines = BottomLines
        { ssgversion = showVersionT version
        , today = showT year2000
          -- avoids changes during debug
        , filename = showT fn2
        }
  -- let htmlout2 = 
  -- when True
  --   $ do
  -- putIOwords ["pandoc settings2.yaml", showT settingsYaml]
  let val = mergeAll
        [ settingsYaml
        , pageTypeYaml
        -- , unDocValue docval
        , toJSON htmlout
        , toJSON ix
        , toJSON bottomLines]
  return val

data BottomLines =
  BottomLines { ssgversion :: Text
              , today :: Text -- ^ the data when converted(baked)
              , filename :: Text
              }
  deriving (Generic, Read, Show, Eq, Ord)

instance ToJSON BottomLines

-- instance ToJSON HTMLout 
-- instance Zeros Pandoc where 
--     zero = Pandoc zero zero 
-- instance Zeros Meta where 
--     zero = mempty
