
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
import Uniform.FileStrings
import Uniform.TypedFile

import Text.Glabrous (Template, insert) -- , insertMany)
import Text.DocTemplates (applyTemplate)
--import Data.Aeson

import Lib.FileMgt
import qualified Text.Glabrous as G

-- the final application
applyTemplate2 :: Path Abs Dir -> Path Rel File -> DocValue -> ErrIO HTMLout
-- apply the template in the file to the text
applyTemplate2 templateDir templateFn val = do
     templText <- read7 templateDir templateFn  dtmplFileType
     case applyTemplate (unwrap7 templText)  (unDocValue val) of
                    Left msg -> throwError  . s2t $ msg
                    Right val2 -> return  . HTMLout $  (val2 :: Text)

-- combine a doctype template in a glabrous master

putPageInMaster :: Path Abs Dir -> Path Rel File -> Path Rel File -> Text -> Path Rel File -> ErrIO ()
-- ^ insert the first doctype template into the (master) glabrous template at the tag
-- result is a doctype (clean) template
putPageInMaster templateDir page master tag full = do
    putIOwords ["putPageInMaster put", showT page, "into", showT master
                        , "\n\tat",  tag, "giving", showT full]

    fm :: Gtemplate <- read7 templateDir master gtmplFileType -- must have correct extension
    putIOwords ["putPageInMaster", "fm read"]
    fp :: Dtemplate <- read7 templateDir page dtmplFileType
    putIOwords ["putPageInMaster", "fpread"]

    let master2 = compileGlabrous fm :: G.Template
    let temp2 = unwrap7 fp  :: Text -- the text of the page (doctemplate)

    let replaceList = G.fromList [(tag,temp2)] :: G.Context

    let resTempl = G.partialProcess master2 replaceList :: G.Template

    let tags = G.tagsOf resTempl -- should be zero
    if null tags
        then do
                let resTempl2 = G.toText resTempl
                write7 templateDir full dtmplFileType (wrap7 resTempl2 :: Dtemplate)
        else throwErrorT ["putPageInMaster", "template not completely replaced"
                    , "tags open", showT tags]

    return ()

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






