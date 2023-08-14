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
import Uniform.Latex()
import Uniform.Shake
import Uniform.Http () --- out ( HTMLout )
import UniformBase

import Data.Maybe (fromMaybe)

-- import Lib.IndexMake ( convertIndexEntries, MenuEntry )
import Lib.IndexCollect ( collectIndex )
-- import Lib.Templating ( putValinMaster )

------------------------------------------------docrep -> panrep

-- | transform a docrep to a panrep (which is the pandoc rep)
--  completes the index (if indexpage else nothing done)

--  the refs are processed before in md2docrep
-- not yet producing index 


docrep2panrep :: NoticeLevel -> PubFlags -> Settings -> Docrep -> ErrIO (Panrep, [FilePath])
docrep2panrep debug pubf sett4x metaplus5 = do
    when (inform debug) $
        putIOwords
            ["-------------------------docrep2panrep"
             , "metaplus: \n", showPretty metaplus5
                -- , "\np1: ", showT p1
                ]
--     -- let pr = Panrep
    --             { panyam = y1  -- meta
    --             , panpan = p1
    --             }
    let sett4 = sett metaplus5
        layout = siteLayout sett4
        meta5 = metap  metaplus5 -- ~ panyam 
        extra5 = extra metaplus5
        hpname = blogAuthorToSuppress layout
        mdFile5 = makeAbsFile $ mdFile extra5
        mdFileDir =   makeAbsDir $ getParentDir mdFile5 :: Path Abs Dir
        doughP = doughDir layout
        defaut = defaultAuthor layout
        aut1 = getTextFromYaml6 defaut "author" meta5
        extra6 = extra5{authorReduced = blankAuthorName hpname aut1}

        -- panrep2 = Panrep y2 p1

    when (inform debug) $ putIOwords ["docrep2panrep"
            , showT extra6
                -- , "hpname", showT hpname
                -- , "\nauthorReduced", authorReduced
                ]

    if isIndexPage mdFile5
        then do
            -- let mdfn =  mdFile extra6 
            --     relfn = makeRelativeP doughP mdfn
            --     -- ix1 = zero{ixfn= toFilePath mdfn
            --     --            , link = toFilePath relfn 
            --     --             }
            --     doughP = doughDir layout 
            --     ixSort = getTextFromYaml6 "filename" "IndexSort" meta5

            -- when (inform debug) $
            --     putIOwords ["\n ix1------------------------docrep2panrep before collectIndex"
            --     , showPretty ix1 ]

            ix2 <- collectIndex debug pubf sett4 doughP mdFileDir

            when (inform debug) $
                putIOwords ["\n ix2------------------------docrep2panrep after collectIndex"
                , showPretty ix2 ]

            let extra7 = extra6{indexEntry = ix2}
                ixs =  (dirEntries  ix2) ++ (fileEntries ix2)
                needs :: [FilePath] =  ixs

            when (inform debug) $
                putIOwords ["\n extra7------------------------docrep2panrep end if"
                , showPretty extra7
                , "needs", showT needs]

            return (metaplus5{extra=extra7}, needs)
        else
            return (metaplus5{extra=extra6}, [])
-- return (metaplus6, [])


-- old 
    -- if isIndexPage (makeAbsFile . dyFn . panyam $ panrep2 )
    --     then do
    -- -- if dyIndexPage . panyam $ pr
    --         let m1 = panyam panrep2  -- meta
    --         let ix1 =dyIndexEntry  m1
    --         -- let bakedP = bakedDir layout
    --         let doughP = doughDir layout
    --         ix2 <- collectIndex debug pubf sett4 doughP (dyIndexSort . panyam $ panrep2) ix1
    --         -- todo put ix2 into pr
    --         let m2 = m1{dyIndexEntry = ix2}
    --         let ixs = dirEntries  ix2 ++ fileEntries ix2
