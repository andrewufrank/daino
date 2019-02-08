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
--  ( markdownToPandoc, pandocToContentHtml, getMeta
--  , Pandoc, flattenMeta, readMarkdown2, _String, key, (^?)
--  )
    where

import Control.Lens ((^?), (?~), (&), at)
import Data.Aeson
import Data.Aeson.Lens
import  Data.Yaml.Union
import Text.Pandoc as Pandoc
import Text.Pandoc.Highlighting
import Text.Pandoc.Shared
import Text.CSL.Pandoc (processCites')

import Uniform.Filenames hiding (Meta, at)
import Uniform.Error hiding (Meta, at)
import Uniform.FileIO hiding (Meta, at)
import Lib.FileMgt -- (MarkdownText(..), unMT, HTMLout(..), unHTMLout
--            , unDocValue, DocValue (..) )
import Lib.Indexing
import Lib.BibTex

-- | Convert markdown text into a 'Value';
-- The 'Value'  has a "content" key containing rendered HTML
-- Metadata is assigned on the respective keys in the 'Value'
-- includes reference replacement (pandoc-citeproc)
-- runs in the pandoc monad!

markdownToPandoc :: Bool -> MarkdownText -> ErrIO Pandoc
-- process the markdown (including if necessary the BibTex treatment)
-- the bibliography must be in the metadata
-- the settings are in the markdownText (at end - to let page specific have precedence)
markdownToPandoc debug (MarkdownText t)  = do
    pandoc   <- readMarkdown2   t
    let meta2 = flattenMeta (getMeta pandoc)
    let bib = getMaybeStringAtKey meta2 "bibliography" :: Maybe Text
    let nociteNeeded = getMaybeStringAtKey meta2 "bibliographyGroup" :: Maybe Text
    pandoc2 <- case bib of
        Nothing ->  return pandoc
        Just bibfp ->   pandocProcessCites (makeAbsFile . t2s $ bibfp) nociteNeeded pandoc

    return pandoc2



    -- test if biblio is present and apply
--    let bib = fmap t2s $  ( meta2) ^? key "bibliography" . _String :: Maybe FilePath


    return pandoc2

pandocProcessCites :: Path Abs File  -> Maybe Text -> Pandoc -> ErrIO Pandoc
-- process the citations
-- including the filling the references for publication lists
pandocProcessCites biblio groupname pandoc1 = do
        pandoc2 <- case groupname of
            Nothing -> return pandoc1
            Just gn -> do
                    bibids <- bibIdentifierFromBibTex biblio (t2s gn)
                    let bibidsat = map ("@" <>) bibids
                    let meta3 = putStringAtKey (getMeta pandoc1) "nocite" (s2t . unwords $ bibidsat)
                    let pandoc2 = putMeta meta3 pandoc1
                    return pandoc2
        callIO $ processCites'  pandoc2



pandocToContentHtml :: Bool -> Pandoc ->  ErrIO DocValue
-- convert the pandoc to html in the contentHtml key
-- the settings are initially put into the pandoc
pandocToContentHtml debug pandoc2 = do
    text2 <-  writeHtml5String2 pandoc2
    let meta2 = flattenMeta (getMeta pandoc2) :: Value
    let withContent = putStringAtKey meta2 "contentHtml" (unHTMLout text2)
--    ( meta2) & _Object . at "contentHtml" ?~ String (unHTMLout text2)
    return  . DocValue $ withContent

docValToAllVal :: Bool -> DocValue -> Path Abs File -> Path Abs Dir -> Path Abs Dir -> ErrIO DocValue
-- from the docVal of the page
-- get the pageType and the settings (master) values
-- and combine them
-- the current file is only necessary if it is an index file
-- then to determine the current dir
-- and to exclude it from index
docValToAllVal debug docval pageFn dough2 template2 = do
        let mpt = getMaybeStringAtKey docval "pageTemplate"
        putIOwords ["docValToAllVal", "mpt", showT mpt]
        let pageType = t2s $ fromMaybe "page0default" mpt  :: FilePath
        -- TODO where is default page set?

        yaml <- read8  ( template2 </> (pageType)) yamlFileType

        settings <- read8 (dough2 </> makeRelFile "settings2") yamlFileType
--        svalue <- decodeThrow . t2b . unYAML $ settings
        let doindex = getMaybeStringAtKey docval "indexPage"
        let doindex2 = fromMaybe False doindex

        putIOwords ["docValToAllVal", "doindex", showT doindex]

        ix :: MenuEntry <- if doindex2
            then
                do
                    let currentDir2 = makeAbsDir $ getParentDir pageFn
                    ix2 <- makeIndexForDir currentDir2 pageFn
                    putIOwords ["docValToAllVal", "index", showT ix2]

                    return ix2

          else return zero
        let ixVal = toJSON ix :: Value
        let val = DocValue . fromJustNote "decoded union 2r2e"
                      . decodeBytestrings
                    $ [ t2b $ unYAML settings
                        , t2b $ unYAML yaml
                        , bl2b . encode $ unDocValue docval
                        , (bl2b . encode $ ixVal)
                       ]  -- last winns!
        return val


-- | Reasonable options for reading a markdown file
markdownOptions :: ReaderOptions
markdownOptions = def { readerExtensions = exts }
 where
  exts = mconcat
    [ extensionsFromList
      [ Ext_yaml_metadata_block
      , Ext_fenced_code_attributes
      , Ext_auto_identifiers
      ,  Ext_raw_html   -- three extension give markdown_strict
      , Ext_shortcut_reference_links
      , Ext_spaced_reference_links
      , Ext_citations           -- <-- this is the important extension for bibTex
      ]
    , githubMarkdownExtensions
    ]


-- | Reasonable options for rendering to HTML
html5Options :: WriterOptions
html5Options = def { writerHighlightStyle = Just tango
                   , writerExtensions     = writerExtensions def
                   }

-- | Handle possible pandoc failure within the Action Monad
unPandocM :: PandocIO a -> ErrIO a
unPandocM op1 = do
        res   <- callIO $ runIO (do  -- liftIO $putStrLn "unPandocM op"
                                     a <- op1 --       error "xx"
                                     -- liftIO $putStrLn "error xx"
                                     return a)
        either (\e -> do
                        putIOwords ["unPandocM error", showT e ]
                        throwError . showT $ e
                ) return res
     `catchError` (\e -> do
                        putIOwords ["unPandocM catchError", showT e ]
                        throwError . showT $  e)



getMeta :: Pandoc -> Meta
getMeta (Pandoc m _) = m

putMeta :: Meta -> Pandoc -> Pandoc
putMeta m1 (Pandoc _ p0) = Pandoc m1 p0

class AtKey vk v where
    getMaybeStringAtKey :: vk -> Text -> Maybe v
    putStringAtKey :: vk -> Text -> v -> vk

instance AtKey Value Text where
    getMaybeStringAtKey meta2 k2 =   meta2 ^? key k2 . _String

    putStringAtKey meta2 k2 txt = meta2 & _Object . at k2 ?~ String  txt
--        (unHTMLout text2)

instance AsValue Meta
instance AsPrimitive Meta
instance AsNumber Meta


instance AtKey Meta Text where
    getMaybeStringAtKey meta2 k2 =   meta2 ^? key k2 . _String

    putStringAtKey meta2 k2 txt = meta2 & _Object . at k2 ?~ String  txt

instance AtKey DocValue  Text where
    getMaybeStringAtKey meta2 k2 = getMaybeStringAtKey (unDocValue meta2) k2

    putStringAtKey meta2 k2 txt = DocValue $ (unDocValue meta2) & _Object . at k2 ?~ String txt

instance AtKey DocValue Bool  where
    getMaybeStringAtKey meta2 k2 =  (unDocValue meta2) ^? key k2 . _Bool

    putStringAtKey meta2 k2 b = DocValue $  (unDocValue meta2) & _Object . at k2 ?~ Bool b


readMarkdown2 :: Text -> ErrIO Pandoc
readMarkdown2 text1 =  unPandocM $ readMarkdown markdownOptions text1

writeHtml5String2 :: Pandoc -> ErrIO HTMLout
writeHtml5String2 pandocRes = do
    p <-  unPandocM $ writeHtml5String html5Options pandocRes
    return . HTMLout $ p


-- | Flatten a Pandoc 'Meta' into a well-structured JSON object, rendering Pandoc
-- text objects into plain strings along the way.
flattenMeta :: Meta -> Value
flattenMeta (Meta meta) = toJSON $ fmap go meta
 where
  go :: MetaValue -> Value
  go (MetaMap     m) = toJSON $ fmap go m
  go (MetaList    m) = toJSONList $ fmap go m
  go (MetaBool    m) = toJSON m
  go (MetaString  m) = toJSON m
  go (MetaInlines m) = toJSON $ stringify m
  go (MetaBlocks  m) = toJSON $ stringify m
