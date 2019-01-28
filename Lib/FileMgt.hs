
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
import Data.Aeson (Value, ToJSON, Value (..))
import Data.Aeson.Encode.Pretty (encodePretty)
import  Path.IO (ensureDir)

instance NiceStrings Value where
    shownice = bb2t . bl2b . encodePretty


newtype DocValue = DocValue Value  deriving (Show,  Eq, Read)
-- ^ a value type with "content" is a html translation
-- and all the other keys
unDocValue (DocValue v) = v

instance Zeros DocValue where zero = DocValue Null

instance NiceStrings DocValue where
    shownice = showNice .  unDocValue

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

class FileHandles a => TypedFiles7 a b where
-- ^ the a is the base type (which is written on file, b is the type for input and output
    wrap7 :: a -> b
    unwrap7 :: b -> a

class FileHandles a => TypedFiles7a a b where

    read7 :: Path Abs Dir -> Path Rel File -> TypedFile5 a b ->   ErrIO b
    write7 :: Path Abs Dir -> Path Rel File -> TypedFile5 a b -> b -> ErrIO ()

    read8 :: Path Abs File -> TypedFile5 a b ->   ErrIO b
    write8 :: Path Abs File -> TypedFile5 a b -> b -> ErrIO ()

instance TypedFiles7 Text b => TypedFiles7a Text b where
-- an instance for all what has text as underlying rep
    write7 fp fn tp ct = do
--        let fn2 = fp </> fn <.> tpext5 tp -- :: Path ar File
        write8 (fp </> fn  ) tp ct
--        let parent = getParentDir fn2
--        createDirIfMissing' parent
--        t <- doesDirExist' fp
----        putIOwords ["TypedFiles7 write7 Text parent", showT parent, "exists", showT t]
--
--        writeFile2 fn2 (unwrap7 ct :: Text )
----        putIOwords ["TypedFiles7 write7 Text Gtemplate", showT fn2]
----        putIOwords ["TypedFiles7 write7 Text Gtemplate text \n", unwrap7 ct]

    read7 fp fn tp   = do
--        putIOwords ["TypedFiles7 read7 Text MarkdownText", showT fp, showT fn]
--        let fn2 = fn <.> tpext5 tp
        read8 (fp </> fn) tp
--        return . wrap7 $ ares

    write8 fp   tp ct = do
        let fn2 = fp   <.> tpext5 tp -- :: Path ar File
--        write8 (fp </> fn  ) tp ct
        let parent = getParentDir fn2
        createDirIfMissing' parent
--        t <- doesDirExist' fp
--        putIOwords ["TypedFiles7 write7 Text parent", showT parent, "exists", showT t]

        writeFile2 fn2 (unwrap7 ct :: Text )
--        putIOwords ["TypedFiles7 write7 Text Gtemplate", showT fn2]
--        putIOwords ["TypedFiles7 write7 Text Gtemplate text \n", unwrap7 ct]

    read8 fp  tp   = do
--        putIOwords ["TypedFiles7 read7 Text MarkdownText", showT fp, showT fn]
        let fp2 = fp <.> tpext5 tp
        ares :: Text <- readFile2 $ fp2
        return . wrap7 $ ares



