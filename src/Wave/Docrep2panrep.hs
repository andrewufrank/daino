---------------------------------------------------------------------
--
-- Module      :  Uniform.Doc2html
--  converts an md document in 2steps 
--      docrep -> panrep
        --     includes preparing of index pages 
        --     the processsing of the refs are already done in doc processing 
        -- panrep -> html 
---------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}
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

 
module Wave.Docrep2panrep (
    module Wave.Docrep2panrep,
) where

import Foundational.Filetypes4sites
    ( Docrep(Docrep), Panrep(Panrep, panyam) )
import Foundational.SettingsPage
    ( blankAuthorName, SiteLayout(blogAuthorToSuppress, doughDir) )
import Foundational.CmdLineFlags ( PubFlags )

import Foundational.MetaPage
import GHC.Generics (Generic)

import Uniform.Json ( ToJSON(toJSON), Value, ErrIO )
import Uniform.Pandoc ( writeHtml5String2 )
import Uniform.Http --- out ( HTMLout )
import UniformBase

import Data.Maybe (fromMaybe)

import Lib.IndexMake ( convertIndexEntries, MenuEntry )
import Lib.IndexCollect ( completeIndex )
import Lib.Templating ( putValinMaster )

------------------------------------------------docrep -> panrep

-- | transform a docrep to a panrep (which is the pandoc rep)
--  completes the index (if indexpage else nothing done)

--  the refs are processed before in md2docrep

docrep2panrep :: NoticeLevel -> PubFlags -> SiteLayout -> Docrep -> ErrIO (Panrep, [FilePath])
docrep2panrep debug pubf layout (Docrep y1 p1) = do
    when (informAll debug) $
        putIOwords ["\n\ty1,p1-------------------------docrep2panrep"
                , "\ny1: ", showT y1
                , "\np1: ", showT p1]
    -- let pr = Panrep
    --             { panyam = y1
    --             , panpan = p1
    --             }
    let hpname = blogAuthorToSuppress layout
    let authorReduced = blankAuthorName hpname (dyAuthor y1)
    let y2 = y1{dyAuthor = authorReduced}
    
    let pr2 = Panrep y2 p1

    when (informAll debug) $ putIOwords ["docrep2panrep"
                , "hpname", showT hpname
                , "\nauthorReduced", authorReduced]

    if isIndexPage (makeAbsFile . dyFn . panyam $ pr2 )
        then do
    -- if dyIndexPage . panyam $ pr
            let m1 = panyam pr2
            let ix1 =dyIndexEntry  m1
            -- let bakedP = bakedDir layout
            let doughP = doughDir layout
            ix2 <- completeIndex debug pubf layout doughP ix1
            -- todo put ix2 into pr
            let m2 = m1{dyIndexEntry = ix2}
            let ixs = dirEntries  ix2 ++ fileEntries ix2
            let needs :: [FilePath] = map ixfn ixs 
            when (informAll debug) $
                putIOwords ["\n\tm2------------------------docrep2panrep end if"
                , showT m2
                , "needs", showT needs]

            return (pr2{panyam = m2}, needs)
        else
            return (pr2, [])


