
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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lib.FileMgt
     where

--import Uniform.Strings
import Uniform.Filenames
import Uniform.FileStrings
import Uniform.TypedFile
import Data.Aeson (Value, ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import  Path.IO (ensureDir)

instance NiceStrings Value where
    showNice = bb2t . bl2b . encodePretty


newtype DocValue = DocValue Value  deriving (Show,  Eq, Read)
-- ^ a value type with "content" is a html translation
-- and all the other keys
unDocValue (DocValue v) = v

instance NiceStrings DocValue where
    showNice = showNice .  unDocValue

-------------

newtype MarkdownText = MarkdownText Text deriving (Show, Read, Eq, Ord)
-- a wrapper around Markdonw text
unMT (MarkdownText a) = a   --needed for other ops

markdownFileType = TypedFile5 {tpext5 = extMD} :: TypedFile5   Text MarkdownText
--instance FileHandles MarkdownText
-- what is missing here?

instance TypedFiles7 Text  MarkdownText    where
-- handling Markdown and read them into MarkdownText
    wrap7 = MarkdownText
    unwrap7 (MarkdownText a) = a
    read7 fp fn tp   = do
--        putIOwords ["TypedFiles7 read7 Text MarkdownText", showT fp, showT fn]
        let fn2 = fn <.> tpext5 tp
        ares :: Text <- readFile2 $ fp </> fn2
        return . wrap7 $ ares
    write7 f = errorT ["TypedFiles - no implementation for write7", showT f]

newtype HTMLout = HTMLout Text deriving (Show, Read, Eq, Ord)
-- a wrapper around html ready to publish
unHTMLout (HTMLout a) = a

htmloutFileType = TypedFile5 {tpext5 = extHTML} :: TypedFile5 Text HTMLout

--instance FileHandles HTMLout

instance TypedFiles7 Text HTMLout  where

    wrap7 = HTMLout
    unwrap7 (HTMLout a) = a
    write7 fp fn tp ct = do

        let fn2 = fp </> fn <.> tpext5 tp -- :: Path ar File
--        write8 (fp </> fn  ) tp ct
        let parent = getParentDir fn2
        createDirIfMissing' parent
        t <- doesDirExist' fp
--        putIOwords ["TypedFiles7 write7 Text parent", showT parent, "exists", showT t]

        writeFile2 fn2 (unwrap7 ct :: Text )
--        putIOwords ["TypedFiles7 write7 Text HTMLout", showT fn2]
--        putIOwords ["TypedFiles7 write7 Text HTMLout text \n", unwrap7 ct]

    read7 f = errorT ["TypedFiles - no implementation for read7", showT f]

doughDir, bakedDir :: Path Rel Dir
-- ^ the names of the two dir, under siteDir
doughDir = makeRelDir "dough"
bakedDir = makeRelDir "baked"

siteDir :: Path Abs Dir
-- ^ the path to the siteDir (absolute - need not inside package
siteDir = makeAbsDir "/home/frank/Workspace8/SSG/site"

doughPath, bakedPath :: Path Abs Dir
-- the path (absolute) to dough an baked
doughPath = addDir siteDir doughDir :: Path Abs Dir
bakedPath = addDir siteDir bakedDir :: Path Abs Dir

extMD, extHTML :: Extension
extMD = Extension "md"
extHTML = Extension "html"

class FileHandles a => TypedFiles7 a b where
-- ^ the a is the base type (which is written on file, b is the type for input and output
    wrap7 :: a -> b
    unwrap7 :: b -> a

    read7 :: Path Abs Dir -> Path Rel File -> TypedFile5 a b ->   ErrIO b

    write7 :: Path Abs Dir -> Path Rel File -> TypedFile5 a b -> b -> ErrIO ()

--    write8 ::  FileHandles a =>  Path Abs File -> TypedFile5 a b -> b -> ErrIO ()
--    -- write a file, directory is created if not exist
--    -- file, if exist, is replaced
--    write8 fp  tp ct = do
----        when rdfGraphDebug $
--        putIOwords ["write8", showT fp]
----        let fn2 = fp </> addExt lpX fn (tpext tp)  -- :: LegalPathname
--        let fn2 = setExtension (tpext5 tp)  fp
--        createDirIfMissing' (getParentDir fp)  -- add everywhere?
--        when rdfGraphDebug $ putIOwords ["sparql Turtle createDIrIfMissing' "
--                , showT (getParentDir fp)]
--        hand <- openFile2handle fn2 WriteMode
----        when rdfGraphDebug $ putIOwords ["sparql Turtle write6", showT fn2]
--
--        write2handle  hand   (unwrap7 ct) -- changed for Text not []
--
----        when rdfGraphDebug $ putIOwords ["sparql Turtle write6", showT fn2]
--        closeFile2  hand
--        when rdfGraphDebug $ putIOwords ["sparql Turtle write6", showT fn2]

--                      "/home/frank/Workspace8/SSG/theme/templates/pandocDefault.html"
--                      :: Path Abs File
--    html2 <-  applyTemplate2 templateFN val
--
--    writeFile2 fpo html2
--    putIOwords ["bakeOne outhtml\n", html2]

