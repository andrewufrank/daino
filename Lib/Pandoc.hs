-- Module copied from Slick

-- i use it because it concentrates all funny pandoc stuff here (including the
-- writing of the json, cannot be imported, because it fixes there the Action monad
-- which i use here as a synonym to ErrIO

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lib.Pandoc
  ( markdownToPandoc, pandocToContentHtml, getMeta, docValToAllVal
  , getMaybeStringAtKey
  , Pandoc, flattenMeta, readMarkdown2, _String, key, (^?)
  )
    where

import Control.Lens ((^?), (?~), (&), at)
import Data.Aeson
import Data.Aeson.Lens
import  Data.Yaml.Union
import Text.Pandoc as Pandoc
import Text.Pandoc.Highlighting (tango)
--import Text.Pandoc.Shared (stringify)

import Uniform.Filenames hiding (Meta, at)
--import Uniform.Error hiding (Meta, at)
import Uniform.FileIO hiding (Meta, at)
import Lib.FileMgt -- (MarkdownText(..), unMT, HTMLout(..), unHTMLout
--            , unDocValue, DocValue (..) )
import Lib.Indexing
import Lib.BibTex
--import Lib.Foundation
import System.Time
import Paths_SSG (version)
import Data.Version (showVersion)
import Lib.YamlBlocks (flattenMeta, getMeta, getMaybeStringAtKey
                , putStringAtKey, readMarkdown2, unPandocM)


-- | Convert markdown text into a 'Value';
-- The 'Value'  has a "content" key containing rendered HTML
-- Metadata is assigned on the respective keys in the 'Value'
-- includes reference replacement (pandoc-citeproc)
-- runs in the pandoc monad!

markdownToPandoc :: Bool -> Path Abs Dir  -> MarkdownText -> ErrIO (Maybe Pandoc)
-- process the markdown (including if necessary the BibTex treatment)
-- the bibliography must be in the metadata
-- the settings are in the markdownText (at end - to let page specific have precedence)
-- questionable if the draft/publish switch should be here
-- or in the creation of the index (where more details from md is needed
markdownToPandoc debug doughP mdtext  = do
    pandoc   <- readMarkdown2  mdtext
    let meta2 = flattenMeta (getMeta pandoc)
    let publish = getMaybeStringAtKey meta2 "publish" :: Maybe Text
    if  isNothing publish || (fmap toLower' publish) == Just "true"
        then do
--            putIOwords ["markdownToPandoc", "publish", showT publish]

            let bib = getMaybeStringAtKey meta2 "bibliography" :: Maybe Text
            let nociteNeeded = getMaybeStringAtKey meta2 "bibliographyGroup" :: Maybe Text
            pandoc2 <- case bib of
                Nothing ->  return pandoc
                Just bibfp -> pandocProcessCites doughP (doughP </> (makeRelFile . t2s $ bibfp))
                            nociteNeeded mdtext pandoc
                            -- here the dire is used for processing in my code

            return . Just $ pandoc2

        else do
                putIOwords ["markdownToPandoc", "NOT PUBLISH", showT publish]
                return Nothing

pandocToContentHtml :: Bool -> Pandoc ->  ErrIO DocValue
-- convert the pandoc to html in the contentHtml key
-- the settings are initially put into the pandoc
pandocToContentHtml debug pandoc2 = do
    text2 <-  writeHtml5String2 pandoc2
    let meta2 = flattenMeta (getMeta pandoc2) :: Value
    let withContent = putStringAtKey  "contentHtml" (unHTMLout text2) meta2
--    ( meta2) & _Object . at "contentHtml" ?~ String (unHTMLout text2)
    return  . DocValue $ withContent

docValToAllVal :: Bool -> DocValue -> Path Abs File -> Path Abs Dir -> Path Abs Dir -> ErrIO DocValue
-- from the docVal of the page
-- get the pageType and the settings (master) values
-- and combine them
-- the current file is only necessary if it is an index file
-- then to determine the current dir
-- and to exclude it from index
docValToAllVal debug docval pageFn dough2 templateP = do
        let mpageType = getMaybeStringAtKey docval "pageTemplate" :: Maybe Text
        putIOwords ["docValToAllVal", "mpt", showT mpageType]
        let pageType = makeRelFile . t2s $ fromMaybe "page0default" mpageType  :: Path Rel File
        -- page0default defined in theme

        pageTypeYaml <- read8  ( templateP </> (pageType)) yamlFileType

        settingsYaml <- read8 (dough2 </> makeRelFile "settings2") yamlFileType
--        svalue <- decodeThrow . t2b . unYAML $ settings

        ix <- makeIndex debug docval pageFn

        -- combine all the
        let val = DocValue . fromJustNote "decoded union 2r2e"
                      . decodeBytestrings
                    $ [ t2b $ unYAML settingsYaml
                        , t2b $ unYAML pageTypeYaml
                        , bl2b . encode $ unDocValue docval
                        , bl2b . encode . toJSON $ ix
                       ]  -- last winns!
        -- add the bottom line
        now <- callIO $ toCalendarTime =<< getClockTime
        let val3 = putStringAtKey  "ssgversion" (s2t$ showVersion version) .
                    putStringAtKey  "today" (s2t $ calendarTimeToString now) $ val
        return val3





-- | Reasonable options for rendering to HTML
html5Options :: WriterOptions
html5Options = def { writerHighlightStyle = Just tango
                   , writerExtensions     = writerExtensions def
                   }


writeHtml5String2 :: Pandoc -> ErrIO HTMLout
writeHtml5String2 pandocRes = do
    p <-  unPandocM $ writeHtml5String html5Options pandocRes
    return . HTMLout $ p

