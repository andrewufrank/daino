---------------------------------------------------------------------
--
-- Module      :  Uniform.Panrep2
--  converts an md document in 2steps 
--      docrep -> panrep
        --     includes preparing of index pages 
        --     the processsing of the refs are already done in doc processing 
        -- panrep -> html 
---------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
            -fno-warn-duplicate-exports
            -fno-warn-unused-imports
            -fno-warn-unused-matches #-}


module Wave.Panrep2 (
    module Wave.Panrep2,
) where

-- import Data.Default
import UniformBase
import Foundational.Filetypes4sites
import Foundational.SettingsPage
    -- ( Settings(siteLayout), SiteLayout(blogAuthorToSuppress) )
import Foundational.CmdLineFlags ( PubFlags )

import GHC.Generics (Generic)

import Uniform.Json ( ToJSON(toJSON), Value, ErrIO )
import Uniform.Pandoc
-- import Uniform.Http ( HTMLout (HTMLout) )
import Uniform.Shake  
import Uniform.MetaPlus hiding (MetaPlus(..), Settings(..), ExtraValues(..))
import Wave.Md2doc ( includeBakeTest3 ) 
import Data.Maybe (fromMaybe)
import Lib.IndexCollect
import qualified Data.Map as M
-- import Wave.Docrep2panrep
-- import Wave.Md2doc
-- import System.FilePath (replaceExtension)
-- import Path (addFileExtension, addExtension)

default (Integer, Double, Text)


lookup7 :: Text -> M.Map Text Text ->  Text
lookup7 k m = fromJustNoteT ["lookup7 in panrep2", k, showT m]
            . M.lookup k $ m

lookup7withDef  :: Text -> Text -> M.Map Text Text -> Text
--get the Metavalue (with  default)
lookup7withDef def1 key m =  fromMaybe def1 $ M.lookup key m


getIndexFiles4meta :: Panrep -> [Path Rel File]
-- get the index files (for dir and files)
getIndexFiles4meta pan = getIndexFiles (f1 ++ d1)
    where
        f1 = fileEntries .  extra  $ pan
        d1 = dirEntries . extra $ pan 

getVals2 :: NoticeLevel -> PubFlags -> Path Abs Dir -> IndexEntry2
                -> ErrIO (Maybe IndexEntry2)
-- get the panrep and fill the vals 
getVals2 debug pubFlags bakedP ix2 = do
    putInform debug ["getVals2  ix2", showPretty ix2]    
    let fnix4 = (ixfn ix2) :: FilePath
        fnix3 = addDir (toFilePath bakedP) fnix4 :: FilePath
        fnix2 =  fnix3 <.> "panrep"  :: FilePath
        fn = makeAbsFile fnix2
        pdf = replaceExtension2 ".pdf" fn 

    putInform debug ["getVals2 fn", showT fn ]
    pan1 <- read8 fn panrepFileType
    -- putInform debug ["getVals2 pan1", showT pan1 ]

    let m = metap  pan1
        ix3 = ix2   { abstract = getTextFromMeta5 ""  "abstract" m
                    , title = getTextFromMeta5 "TITLE MISSING" "title" m
                    , author = getTextFromMeta5 "" "author" m -- todo suppressed?
                    , date = getTextFromMeta5 "2000-01-01" "date" m
                    , sortOrder = getTextFromMeta5 "filename" "sortOrder" m
                    , version = getTextFromMeta5 "draft" "version" m
                    , visibility = getTextFromMeta5 "private" "visibility" m
                    , pdf1 = s2t $ toFilePath pdf 
                    }

    return $ if includeBakeTest3 pubFlags (version ix3) (visibility ix3)
                then Just ix3 else errorT ["getVals2 in panrep2 not included", showT ix2 ]



  