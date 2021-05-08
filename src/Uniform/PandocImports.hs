--------------------------------------------------------------------------
--
-- Module      :  Uniform.PandocImports
-- read and write pandoc files (intenal rep of pandoc written to disk)
-- von hier Pandoc spezifisches imortieren
-- nich exportieren nach aussen
-------------------------------
-- {-# LANGUAGE BangPatterns                   #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports 
            -fno-warn-unused-imports #-}

module Uniform.PandocImports
  ( module Uniform.PandocImports,
    Pandoc (..),
    -- PandocBlock (..)
    -- Pandoc.Block
  )
where

import Text.Pandoc
  ( CiteMethod (Natbib),
    Meta,
    MetaValue,
    Pandoc (..),
    WriterOptions
      ( writerCiteMethod,
        writerExtensions,
        writerHighlightStyle
      ),
    def,
    writeLaTeX,
  )
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Extensions as Pandoc
import Text.Pandoc.Highlighting (tango)
import Text.Pandoc.Shared (stringify)
import Uniform.Json (ErrIO, ToJSON (toJSON, toJSONList), Value)
import Uniform.Yaml (ErrIO, yaml2value, yamlFileType)
import UniformBase

-- -- type PandocBlock = Pandoc.Block
-- instance Zeros Pandoc.Block where 
--         zero = Pandoc.Null
 
instance Zeros Pandoc where
  zero = Pandoc zero zero

instance Zeros Text.Pandoc.Meta where
  zero = mempty

-- | Handle possible pandoc failure within the PandocIO Monad
unPandocM :: Pandoc.PandocIO a -> ErrIO a
unPandocM op1 =
  do
    res <-
      callIO $
        Pandoc.runIO op1
    --   ( do
    --       -- liftIO $putStrLn "unPandocM op"
    --       a <- op1 --       error "xx"
    --       -- liftIO $putStrLn "error xx"
    --       -- should be
    --       --     result <- P.runIO $ op
    --       --     rst1   <- P.handleError result
    --       -- and put then in the two parts of ErrIO
    --       return a
    --   )
    either
      ( \e -> do
          putIOwords ["unPandocM error", showT e]
          throwError . showT $ e
      )
      return
      res
    `catchError` ( \e -> do
                     putIOwords ["unPandocM catchError", showT e]
                     throwError . showT $ e
                 )

getMeta :: Pandoc -> Pandoc.Meta
getMeta (Pandoc.Pandoc m _) = m

putMeta :: Pandoc.Meta -> Pandoc -> Pandoc
putMeta m1 (Pandoc _ p0) = Pandoc m1 p0

-- | Flatten a Pandoc 'Meta' into a well-structured JSON object,  
-- adapted from https://hackage.haskell.org/package/slick-1.1.1.0/docs/src/Slick.Pandoc.html#flattenMeta
flattenMeta :: Pandoc.Meta -> Value
flattenMeta (Pandoc.Meta meta) = toJSON $ fmap go meta
  where
    go :: MetaValue -> Value
    go (Pandoc.MetaMap m) = toJSON $ fmap go m
    go (Pandoc.MetaList m) = toJSONList $ fmap go m
    go (Pandoc.MetaBool m) = toJSON m
    go (Pandoc.MetaString m) = toJSON m
    go (Pandoc.MetaInlines m) = toJSON $ stringify m
    go (Pandoc.MetaBlocks m) = toJSON $ stringify m

readYaml2value :: Path Abs File -> ErrIO Value
-- read a yaml file to a value
-- error when syntax issue
readYaml2value fp = do
  t <- read8 fp yamlFileType
  return . yaml2value $ t

latexOptions :: WriterOptions
-- | reasonable extension - crucial!
latexOptions =
  def
    { writerHighlightStyle = Just tango,
      writerCiteMethod = Natbib,
      -- Citeproc                        -- use citeproc to render them
      --           | Natbib                        -- output natbib cite commands
      --           | Biblatex                      -- output biblatex cite commands
      writerExtensions =
        Pandoc.extensionsFromList
          [ Pandoc.Ext_raw_tex --Allow raw TeX (other than math)
          -- , Pandoc.Ext_shortcut_reference_links
          -- , Pandoc.Ext_spaced_reference_links
          -- , Pandoc.Ext_citations           -- <-- this is the important extension for bibTex
          ]
    }

-- instance ToJSON Text
-- writeLaTeX :: PandocMonad m => WriterOptions -> Pandoc -> m Text

instance TypedFiles7 Text Text where
  wrap7 = id
  unwrap7 = id

writeTexSnip2 :: Pandoc -> ErrIO Text
-- write a latex file from a pandoc doc
writeTexSnip2 pandocRes = do
  p <- unPandocM $ writeLaTeX latexOptions pandocRes
  return p

