
------------------------------------------------------------------------------
--
-- Module      :   applying a template (using pandoc)
--

-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lib.Templating  -- (openMain, htf_thisModuelsTests)
     where

import Uniform.Strings
import Uniform.Filenames
--import Uniform.FileStrings
import Uniform.TypedFile

--import Text.Glabrous (Template, insert) -- , insertMany)
import Text.DocTemplates (applyTemplate)
--import Data.Aeson

import Lib.FileMgt
import qualified Text.Glabrous as G

-- the final application
applyTemplate3 :: Dtemplate -> DocValue -> ErrIO HTMLout
-- apply the template in the file to the text
applyTemplate3  templText val = do
     case applyTemplate (unwrap7 templText)  (unDocValue val) of
                    Left msg -> throwError  . s2t $ msg
                    Right val2 -> return  . HTMLout $  (val2 :: Text)

putPageInMaster ::  Path Abs File -> Path Abs File -> Text -> Path Abs File -> ErrIO ()
-- ^ insert the first doctype template into the (master) glabrous template at the tag
-- result is a doctype (clean) template
putPageInMaster  page master tag full = do
        putIOwords ["putPageInMaster put", showT page, "into", showT master
                        , "\n\tat",  tag, "giving", showT full]
        fm :: Gtemplate <- read8 master gtmplFileType -- must have correct extension
        resTempl2 <- putPageInMaster2  page fm tag
        write8 full dtmplFileType ( resTempl2 :: Dtemplate)

putPageInMaster2 ::  Path Abs File -> Gtemplate -> Text ->  ErrIO Dtemplate
-- ^ insert the first doctype template into the (master) glabrous template at the tag
-- result is a doctype (clean) template
putPageInMaster2  page master tag  = do
    putIOwords ["putPageInMaster2 put", showT page, "into", showT master
                        , "\n\tat",  tag]

--    fm :: Gtemplate <- read8 master gtmplFileType -- must have correct extension
    putIOwords ["putPageInMaster", "fm read"]
    fp :: Dtemplate <- read8 page dtmplFileType
    putIOwords ["putPageInMaster", "fpread"]

    let master2 = compileGlabrous master :: G.Template
    let temp2 = unwrap7 fp  :: Text -- the text of the page (doctemplate)

    let replaceList = G.fromList [(tag,temp2)] :: G.Context

    let resTempl = G.partialProcess master2 replaceList :: G.Template

    let tags = G.tagsOf resTempl -- should be null
    when (not $ null tags) $
        throwErrorT ["putPageInMaster", "template not completely replaced"
                    , "tags open", showT tags]
    return . Dtemplate $ G.toText resTempl

compileGlabrous :: Gtemplate -> G.Template
-- compile a glabrous template from text
compileGlabrous t = either (\msg -> errorT ["glabrous master not ok", s2t msg]) id
                    $ G.fromText . unwrap7 $ t

-- handling the glabrous templates gtpl
extGtemplate = Extension "gtpl"

newtype Gtemplate = Gtemplate Text deriving (Show, Read, Eq, Ord)
-- ^ a template which contains variables in glabrous {{xx}} format
-- a wrapper around html ready to publish
--unGtemplate (Gtemplate a) = a

gtmplFileType = makeTyped extGtemplate :: TypedFile5 Text Gtemplate

instance Zeros Gtemplate where zero = Gtemplate zero

instance TypedFiles5 Text Gtemplate  where
instance TypedFiles7 Text Gtemplate  where

    wrap7 = Gtemplate
    unwrap7 (Gtemplate a) = a

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






