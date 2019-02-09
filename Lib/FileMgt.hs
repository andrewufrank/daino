
------------------------------------------------------------------------------
--
-- Module      :   dealing with file reading and writing
--              including the where things are (below site)

-- file type: md for markdown, html for templates, html for output

-- text types MD, Template, HTMLout

-- the site dir (and other similar) could be read in as yaml
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lib.FileMgt
     where

import Uniform.Zero
import Uniform.Filenames
import Uniform.FileStrings
import Uniform.TypedFile
import Data.Aeson (Value, ToJSON (..), Value (..), decode)
import Data.Aeson.Encode.Pretty (encodePretty)
--import  Path.IO (ensureDir)

instance NiceStrings Value where
    shownice = bb2t . bl2b . encodePretty


newtype DocValue = DocValue Value  deriving (Show,  Eq, Read)
-- ^ a value type with "content" is a html translation
-- and all the other keys
unDocValue (DocValue v) = v

instance Zeros DocValue where zero = DocValue Null

instance NiceStrings DocValue where
    shownice = showNice .  unDocValue

docValueFileType = TypedFile5 {tpext5 = Extension "docval"} :: TypedFile5   Text DocValue
--instance FileHandles MarkdownText
-- what is missing here?


instance TypedFiles7 Text  DocValue    where
-- handling Markdown and read them into DocValue
    wrap7 = DocValue . fromJustNote "wrap7 docvalue decode" . decode . b2bl . t2b
    unwrap7 (DocValue a) = shownice a

-------------

newtype MarkdownText = MarkdownText Text deriving (Show, Read, Eq, Ord)
-- a wrapper around Markdonw text
unMT (MarkdownText a) = a   --needed for other ops

instance Zeros MarkdownText where zero = MarkdownText zero
markdownFileType = TypedFile5 {tpext5 = extMD} :: TypedFile5   Text MarkdownText
--instance FileHandles MarkdownText
-- what is missing here?


instance TypedFiles7 Text  MarkdownText    where
-- handling Markdown and read them into MarkdownText
    wrap7 = MarkdownText
    unwrap7 (MarkdownText a) = a

-----------
newtype YamlText = YamlText Text deriving (Show, Read, Eq, Ord)
-- a wrapper around Markdonw text
unYAML (YamlText a) = a   --needed for other ops

extYAML = Extension "yaml"
instance Zeros YamlText where zero = YamlText zero

yamlFileType = TypedFile5 {tpext5 = extYAML} :: TypedFile5   Text YamlText
--instance FileHandles YamlText
-- what is missing here?


instance TypedFiles7 Text  YamlText    where
-- handling Markdown and read them into YamlText
    wrap7 = YamlText
    unwrap7 (YamlText a) = a

-----------
newtype HTMLout = HTMLout Text deriving (Show, Read, Eq, Ord)

-- a wrapper around html ready to publish
unHTMLout (HTMLout a) = a

htmloutFileType = TypedFile5 {tpext5 = extHTML} :: TypedFile5 Text HTMLout

instance Zeros HTMLout where zero = HTMLout zero

instance TypedFiles7 Text HTMLout  where

    wrap7 = HTMLout
    unwrap7 (HTMLout a) = a


extMD, extHTML :: Extension
extMD = Extension "md"
extHTML = Extension "html"


---- handling the glabrous templates gtpl
--extGtemplate = Extension "gtpl"
--
--newtype Gtemplate = Gtemplate Text deriving (Show, Read, Eq, Ord)
---- ^ a template which contains variables in glabrous {{xx}} format
---- a wrapper around html ready to publish
----unGtemplate (Gtemplate a) = a
--
--gtmplFileType = makeTyped extGtemplate :: TypedFile5 Text Gtemplate
--
--instance Zeros Gtemplate where zero = Gtemplate zero
--
--instance TypedFiles5 Text Gtemplate  where
--instance TypedFiles7 Text Gtemplate  where
--
--    wrap7 = Gtemplate
--    unwrap7 (Gtemplate a) = a

-- handling the doctype templates dtpl
extDtemplate = Extension "dtpl"

newtype Dtemplate = Dtemplate Text deriving (Show, Read, Eq, Ord)
-- ^ a template which contains variables in doctype  $x$  format

-- a wrapper around html ready to publish
--unDtemplate (Dtemplate a) = a

dtmplFileType = makeTyped extDtemplate :: TypedFile5 Text Dtemplate

instance Zeros Dtemplate where zero = Dtemplate zero

instance TypedFiles5 Text Dtemplate  where
instance TypedFiles7 Text Dtemplate  where

    wrap7 = Dtemplate
    unwrap7 (Dtemplate a) = a

