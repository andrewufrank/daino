--------------------------------------------------------------------------
--
-- Module      :  Uniform.Markdown
-- these are operations which are not influenced by SSG
-- just stuff that depends, for example, on pandoc
-------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DoAndIfThenElse #-}
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
            -fno-warn-deprecations #-}

module Uniform2.Markdown (
    module Uniform2.Markdown,
    Pandoc.ReaderOptions,
    markdownFileType,
    extMD,
) where

import qualified Text.Pandoc as Pandoc
import Uniform.PandocImports (
    MarkdownText,
    Pandoc,
    extMD,
    markdownFileType,
    unPandocM,
 )
import UniformBase (ErrIO, TypedFiles7 (wrap7))


writeAST2md :: Pandoc -> ErrIO MarkdownText

-- | write the AST to markdown
writeAST2md dat = do
    r <- unPandocM $ do
        r1 <-
            Pandoc.writeMarkdown
                Pandoc.def{Pandoc.writerSetextHeaders = False}
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
