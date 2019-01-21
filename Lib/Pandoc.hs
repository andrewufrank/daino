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
  ( markdownToHTML4x

  ) where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Text.Pandoc as Pandoc
import Text.Pandoc.Highlighting
import Text.Pandoc.Shared
import Text.CSL.Pandoc (processCites')
import System.Process  as System (readProcess)

import Uniform.Error hiding (Meta, at)
import Lib.FileMgt (MarkdownText(..), unMT, HTMLout(..), unHTMLout
            , unDocValue, DocValue (..) )

--main :: IO ()
--main = do
--    page <- T.readFile "page.markdown"  -- biblio line
--    bibString <-  readFile "refs.bib"
--    bibReferences :: [Reference] <-  readBibtexString (const True) True False bibString
--
--    styleString <- liftIO $ readFile   "chicago.csl"
--    let style1 = parseCSL  styleString :: Style
--
--    -- process with my code
--    html1 <- unPandocM $    processCites2 style1 bibReferences page
--
--    -- process with system call to pandoc
--    html2 <- processCites2x page
--
--
--    T.putStrLn "The two results should be very close: \n With haskell code : \n"
--    T.putStrLn html1
--    putStrLn "\nwith pandoc cmd : \n"
--    T.putStrLn html2
--
--    return ()

-- | Convert markdown text into a 'Value';
-- The 'Value'  has a "content" key containing rendered HTML
-- Metadata is assigned on the respective keys in the 'Value'
-- includes reference replacement (pandoc-citeproc)
-- runs in the pandoc monad!
markdownToHTML4x :: Bool -> MarkdownText -> ErrIO DocValue
markdownToHTML4x debug (MarkdownText t)  = do
  pandoc   <- readMarkdown2   t
  let meta2 = flattenMeta (getMeta pandoc)

  -- test if biblio is present and apply
  let bib = fmap t2s $  ( meta2) ^? key "bibliography" . _String :: Maybe FilePath
--  let csl = fmap t2s $  ( meta2) ^? key "csl" . _String :: Maybe FilePath

  text2 <- case bib of
    Nothing ->   writeHtml5String2  pandoc

    Just _ ->   processCites2a pandoc
--                        (fromJustNote "markdownToHTML4x no csl file" csl)
--                        (fromJustNote "markdownToHTML4x no bib file" bib)



  let withContent = ( meta2) & _Object . at "contentHtml" ?~ String (unHTMLout text2)
  return  . DocValue $ withContent

processCites2a ::    Pandoc ->  ErrIO HTMLout
-- process the citations, with biblio and csl in the file
processCites2a  pandoc3  =  do

        pandoc4 <- callIO $ processCites'  pandoc3

        writeHtml5String2 pandoc4



-- | Reasonable options for reading a markdown file
markdownOptions :: ReaderOptions
markdownOptions = def { readerExtensions = exts }
 where
  exts = mconcat
    [ extensionsFromList
      [ Ext_yaml_metadata_block
      , Ext_fenced_code_attributes
      , Ext_auto_identifiers
      , Ext_citations           -- <-- this is the important extension
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


---- | Convert markdown text into a 'Value';
---- The 'Value'  has a "content" key containing rendered HTML
---- Metadata is assigned on the respective keys in the 'Value'
---- includes reference replacement (pandoc-citeproc)
---- runs in the pandoc monad!
--markdownToHTML4x :: Bool -> MarkdownText -> ErrIO DocValue
--markdownToHTML4x debug (MarkdownText t)  = do
--  pandoc   <- readMarkdown2   t
--  let meta2 = flattenMeta (getMeta pandoc)
--
--  -- test if biblio is present and apply
--  let bib = fmap t2s $  ( meta2) ^? key "bibliography" . _String :: Maybe FilePath
--  let csl = fmap t2s $  ( meta2) ^? key "csl" . _String :: Maybe FilePath
--  text2 <- case bib of
--    Nothing -> do
--                    htmltext <- writeHtml5String2  pandoc
--                    return htmltext
--    Just _ -> do
--                res <- processCites2x debug csl bib t
--                when (res == t) $
--                    liftIO $ putStrLn "\n*** markdownToHTML3 result without references ***\n"
--                return . HTMLout $ res
--
--
--  let withContent = ( meta2) & _Object . at "contentHtml" ?~ String (unHTMLout text2)
--  return  . DocValue $ withContent

getMeta :: Pandoc -> Meta
getMeta (Pandoc m _) = m

cslDefault, apaCSL :: FilePath
cslDefault = "/home/frank/Workspace8/SSG/site/resources/chicago-fullnote-bibliography-bb.csl"
apaCSL = "/home/frank/Workspace8/SSG/site/resources/apa-x.csl"

processCites2x :: Bool -> Maybe FilePath -> Maybe FilePath -> Text ->   ErrIO Text
-- porcess the cites in the text
-- using systemcall because the standalone pandoc works
-- call is: pandoc -f markdown -t html  --filter=pandoc-citeproc
-- the csl and bib file are used from text, not from what is passed

processCites2x debug _ _  t  = do
--        let styleFn2 = maybe apaCSL id cslfn
--            bibfn2 = fromJustNote "processCites2x ew224" bibfn   -- tested befire
        when debug $ putIOwords ["processCite2x" ] -- - filein\n", showT styleFn2, "\n", showT bibfn2]

        let cmd = "pandoc"
        let cmdargs = ["--from=markdown", "--to=html5", "--filter=pandoc-citeproc" ]
--                    ++ cmdargsbib ++ cmdargsCSL
--        let cmdargsbib =  "--bibliography=" <> showT cslfn
--        let cmdargsCSL = "--csl=" <> showT bibfn

        let cmdinp = t2s t
        res :: String <- callIO $ System.readProcess cmd cmdargs cmdinp

        return . s2t $ res
        -- error are properly caught and reported in ErrIO



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
