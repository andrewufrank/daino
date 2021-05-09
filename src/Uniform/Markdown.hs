--------------------------------------------------------------------------
--
-- Module      :  Uniform.Markdown
-- these are operations which are not influenced by SSG
-- just stuff that depends, for example, on pandoc
-------------------------------------------------------------------------
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
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports
            -fno-warn-deprecations #-}

module Uniform.Markdown
  ( module Uniform.Markdown,
    Pandoc.ReaderOptions,
    markdownFileType,
    extMD,
  )
where

import qualified Text.Pandoc as Pandoc
import Uniform.PandocImports
    ( extMD, markdownFileType, unPandocM, Pandoc, MarkdownText )
import UniformBase ( ErrIO, TypedFiles7(wrap7) )

-- readMd2meta :: Path Abs File -> ErrIO (Pandoc, Value)
-- -- ^ read a markdown file to metadata
-- -- should not be used ?
-- readMd2meta md = do
--   -- putIOwords ["readMd2meta", "readPandocFile", showT md]
--     mdtext :: MarkdownText <- read8 md markdownFileType
--     pandoc                 <- readMarkdown2 mdtext
--     let meta2 = flattenMeta (getMeta pandoc)
--     -- putIOwords ["readMd2meta", "readPandocFile", showT md, "done"]
--     return (pandoc, meta2)

writeAST2md :: Pandoc -> ErrIO MarkdownText

-- | write the AST to markdown
writeAST2md dat = do
  r <- unPandocM $ do
    r1 <-
      Pandoc.writeMarkdown
        Pandoc.def {Pandoc.writerSetextHeaders = False}
        dat
    return r1
  return . wrap7 $ r

writeAST3md :: Pandoc.WriterOptions -> Pandoc -> ErrIO MarkdownText

-- | write the AST to markdown
writeAST3md options dat = do
  r <- unPandocM $ do
    r1 <-
      Pandoc.writeMarkdown
        options -- Pandoc.def { Pandoc.writerSetextHeaders = False }
        dat
    return r1
  return . wrap7 $ r
