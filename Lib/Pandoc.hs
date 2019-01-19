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
  ( markdownToHTML3
--  , markdownToHTML'
--  , makePandocReader
--  , makePandocReader'
--  , loadUsing
--  , loadUsing'
--  , convert
--  , html5Options
--  , markdownOptions
--  , PandocReader
--  , PandocWriter
  ) where

import Control.Lens
--import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
--import qualified Data.Text as T
--import Development.Shake hiding (Resource)
import Text.Pandoc as Pandoc
import Text.Pandoc.Highlighting
import Text.Pandoc.Shared

import Text.CSL.Pandoc (processCites', processCites)
import Text.CSL (readCSLFile)
--import Text.CSL (readBiblioFile)
import Text.CSL.Input.Bibtex (readBibtex)

import Uniform.Error hiding (Meta, at)
--import Uniform.Strings hiding (Meta, at)
import Lib.FileMgt (MarkdownText(..), unMT, HTMLout(..), unHTMLout
            , unDocValue, DocValue (..) )

--type Action = ErrIO   -- the actual Action Monad in Shake is more elaborate.

-- | Reasonable options for reading a markdown file
markdownOptions :: ReaderOptions
markdownOptions = def { readerExtensions = exts }
 where
  exts = mconcat
    [ extensionsFromList
      [ Ext_yaml_metadata_block
      , Ext_fenced_code_attributes
      , Ext_auto_identifiers
      ]
    , githubMarkdownExtensions
    ]

-- | Reasonable options for rendering to HTML
html5Options :: WriterOptions
html5Options = def { writerHighlightStyle = Just tango
                   , writerExtensions     = writerExtensions def
--                   , writerTemplate = Just "/home/frank/Workspace8/SSG/theme/templates/pandocDefault.html"
-- applying a template here not a good idea
-- process in two steps
                   }

-- | Handle possible pandoc failure within the Action Monad
unPandocM :: PandocIO a -> ErrIO a
unPandocM op1 = do
        res   <- callIO $ runIO (do  -- liftIO $putStrLn "unPandocM op"
                                     a <- op1
--                                     error "xx"
                                     -- liftIO $putStrLn "error xx"
                                     return a)
--                    `catchError` (\e -> throwError . showT $  (e))
        either (\e -> do
                        putIOwords ["unPandocM error", showT e ]
                        throwError . showT $ e
                ) return res

     `catchError` (\e -> do
                        putIOwords ["unPandocM catchError", showT e ]
                        throwError . showT $  e)

markdownToHTML3 :: MarkdownText -> ErrIO DocValue
markdownToHTML3 t = do
    r <- unPandocM $ markdownToHTML4 (unMT t)
    putIOwords ["markdownToHTML3 ", "result", showT r]
    return . DocValue $ r



-- | Convert markdown text into a 'Value';
-- The 'Value'  has a "content" key containing rendered HTML
-- Metadata is assigned on the respective keys in the 'Value'
-- includes reference replacement (pandoc-citeproc)
-- runs in the pandoc monad!
markdownToHTML4 :: Text -> PandocIO Value
markdownToHTML4 t = do
  pandoc   <- readMarkdown markdownOptions  t
  let meta2 = flattenMeta (getMeta pandoc)

  -- test if biblio is present and apply
  let bib = fmap t2s $  ( meta2) ^? key "bibliography" . _String :: Maybe FilePath
  let csl = fmap t2s $  ( meta2) ^? key "csl" . _String :: Maybe FilePath
  pandoc2 <- case bib of
    Nothing -> return pandoc
    Just _ -> do
--                res <- liftIO $ processCites' pandoc --  :: Pandoc -> IO Pandoc
                res <- liftIO $ processCites2 csl bib pandoc
                        -- :: Style -> [Reference] -> Pandoc -> Pandoc


                when (res == pandoc) $
                    liftIO $ putStrLn "\n*** markdownToHTML3 result without references ***\n"
                return res

  htmltex <- writeHtml5String html5Options pandoc2

  let withContent = ( meta2) & _Object . at "contentHtml" ?~ String ( htmltex)
  return  withContent

getMeta :: Pandoc -> Meta
getMeta (Pandoc m _) = m

cslDefault, apaCSL :: FilePath
cslDefault = "/home/frank/Workspace8/SSG/site/resources/chicago-fullnote-bibliography-bb.csl"
apaCSL = "/home/frank/Workspace8/SSG/site/resources/apa-x.csl"

processCites2 :: Maybe FilePath -> Maybe FilePath -> Pandoc -> IO Pandoc
-- porcess the cites in the parsed pandoc, filepath is cls and bibfile name
processCites2 cslfn bibfn  pandoc1 = do
        let styleFn2 = maybe apaCSL id cslfn
            bibfn2 = fromJust bibfn
        putIOwords ["processCite2 - filein\n", showT styleFn2, "\n", showT bibfn2]
--        styleIn <- readFile2 styleFn2
        style1 <- readCSLFile Nothing   styleFn2
--        putIOwords ["processCite2 - style1", showT style1]

        bibReferences <- readBibtex (const True) False False bibfn2
        putIOwords ["processCite2 - bibReferences", showT bibReferences]
--        :: (String -> Bool) -> Bool -> Bool -> FilePath -> IO [Reference]

--Parse a BibTeX or BibLaTeX file into a list of References.
--The first parameter is a predicate to filter identifiers.
--If the second parameter is true, the file will be treated as BibTeX;
--otherwse as BibLaTeX.
--If the third parameter is true, an "untitlecase" transformation will be performed.

        let pandoc2 = processCites style1 bibReferences pandoc1
        putIOwords ["processCite2 - result pandoc2", showT pandoc2]
        return pandoc2
--    where
--        meta2 = getMeta pandoc1 :: Meta
--        stylefn =  fmap t2s $ meta2 ^? key "csl" . _String  :: Maybe FilePath
--        bibfn =  fmap t2s $ meta2 ^? key "bibliography" . _String  :: Maybe FilePath

--readMarkdown2 :: Text -> ErrIO (Pandoc, DocValue)
--readMarkdown2 text1 = do
----    putIOwords ["readMarkdown2 1"]
--    pdoc <-  unPandocM $ readMarkdown markdownOptions text1
----    putIOwords ["readMarkdown2 2"]
--    let meta2 = flattenMeta (getMeta pdoc)
----    putIOwords ["readMarkdown2 3", showNice meta2]
----    meta3 :: Value <- convert (meta2  :: Value)
----
----    putIOwords ["markdownToHTML3 4", showT $ meta3 == meta2]
--
--    return (pdoc, DocValue meta2)

--writeHtml5String2 :: Pandoc -> ErrIO HTMLout
--writeHtml5String2 pandocRes = do
--    p <-  unPandocM $ writeHtml5String html5Options pandocRes
--    return . HTMLout $ p

--processCites2 :: Pandoc -> ErrIO Pandoc
--processCites2 p = do
----                putIOwords ["processCites2 1", showT p]
--                r <- callIO $ processCites' p
--                when (p==r) $ putIOwords ["processCites2 not changed !", showT (p==r)]
--                return r

---- | Attempt to convert between two JSON serializable objects (or 'Value's).
---- Failure to deserialize fails the Shake build.
--convert :: (FromJSON a, ToJSON a, FromJSON b) => a -> Action b
--convert a = case fromJSON (toJSON a) of
--  Success r   -> pure r
--  Error   err -> fail $ "json conversion error:" ++ err

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
