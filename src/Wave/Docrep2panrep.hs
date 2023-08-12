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
    -- ( Docrep(Docrep), Panrep(Panrep, panyam) )
import Foundational.SettingsPage
 
import Foundational.CmdLineFlags ( PubFlags )

import Foundational.MetaPage

import GHC.Generics (Generic)

import Uniform.Json ( ToJSON(toJSON), Value, ErrIO )
import Uniform.Pandoc  
import Uniform.Latex
import Uniform.Http --- out ( HTMLout )
import UniformBase

import Data.Maybe (fromMaybe)

-- import Lib.IndexMake ( convertIndexEntries, MenuEntry )
import Lib.IndexCollect -- ( completeIndex )
-- import Lib.Templating ( putValinMaster )

------------------------------------------------docrep -> panrep

-- | transform a docrep to a panrep (which is the pandoc rep)
--  completes the index (if indexpage else nothing done)

--  the refs are processed before in md2docrep
-- not yet producing index 


docrep2panrep :: NoticeLevel -> PubFlags -> Settings -> Docrep -> ErrIO (Panrep, [FilePath])
docrep2panrep debug pubf sett4 metaplus5 = do
    when (inform debug) $
        putIOwords ["\n\ty1,p1-------------------------docrep2panrep"
                -- , "\ny1: ", showT y1
                -- , "\np1: ", showT p1
                ]
--     -- let pr = Panrep
    --             { panyam = y1
    --             , panpan = p1
    --             }
    let layout = siteLayout . sett $ metaplus5
        meta = metap  metaplus5
        hpname = blogAuthorToSuppress layout
        defaut = defaultAuthor layout
        aut1 = getTextFromYaml6 defaut "author" meta
        authorRed1 = blankAuthorName hpname aut1
        metaplus6 = metaplus5{extra= (extra metaplus5){authorReduced = authorRed1}}
        -- panrep2 = Panrep y2 p1

    when (inform debug) $ putIOwords ["docrep2panrep"
            , showT (extra metaplus6)
                -- , "hpname", showT hpname
                -- , "\nauthorReduced", authorReduced
                ]

    -- if isIndexPage (makeAbsFile . dyFn . panyam $ panrep2 )
    --     then do
    -- -- if dyIndexPage . panyam $ pr
    --         let m1 = panyam panrep2
    --         let ix1 =dyIndexEntry  m1
    --         -- let bakedP = bakedDir layout
    --         let doughP = doughDir layout
    --         ix2 <- completeIndex debug pubf sett4 doughP (dyIndexSort . panyam $ panrep2) ix1
    --         -- todo put ix2 into pr
    --         let m2 = m1{dyIndexEntry = ix2}
    --         let ixs = dirEntries  ix2 ++ fileEntries ix2
    --         let needs :: [FilePath] = map ixfn ixs 
    --         when (inform debug) $
    --             putIOwords ["\n\tm2------------------------docrep2panrep end if"
    --             , showT m2
    --             , "needs", showT needs]

    --         return (panrep2{panyam = m2}, needs)
    --     else
    -- return (panrep2, [])
    return (metaplus6, [])


