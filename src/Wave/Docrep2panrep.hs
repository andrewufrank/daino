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

import UniformBase
import Foundational.Filetypes4sites
    -- ( Docrep(Docrep), Panrep(Panrep, panyam) )
import Foundational.SettingsPage

import Foundational.CmdLineFlags ( PubFlags )


import GHC.Generics (Generic)

import Uniform.Json ( ToJSON(toJSON), Value, ErrIO )
import Uniform.Pandoc
import Uniform.Latex()
import Uniform.Shake
import Uniform.Http () --- out ( HTMLout )

import Data.Maybe (fromMaybe)
import Lib.IndexCollect 
import qualified Data.Map as M
import Development.Shake.FilePath (makeRelative)

------------------------------------------------docrep -> panrep

-- | transform a docrep to a panrep (which is the pandoc rep)
--  completes the index (if indexpage else nothing done)

--  the refs are processed before in md2docrep
--  convert meta to html and latex versions
--  collect the index file and dirs 

docrep2panrep :: NoticeLevel -> PubFlags -> Docrep -> ErrIO (Panrep, [FilePath])
docrep2panrep debug pubf metaplus5 = do
    -- let debug = NoticeLevel0   -- avoid_output_fromHere_down
    when (inform debug) $
        putIOwords
            ["-------------------------docrep2panrep 1"
            --  , "metaplus: \n", showPretty metaplus5
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
        bakedP = bakedDir layout
        defaut = defaultAuthor layout
        aut1 = getTextFromYaml6 defaut "author" meta5
        bookval = getTextFromYaml6  "" "book"   meta5 
        extra6 = extra5{authorReduced = blankAuthorName hpname aut1
                        ,booklet = "booklet" == bookval
                        ,bookBig= "bookBig" == bookval
                        ,webroot = s2t $ toFilePath doughP
                        }

    htm1 <- meta2xx writeHtml5String2 meta5
    tex1  :: M.Map Text Text <- meta2xx   writeTexSnip2 meta5

    let metaplus6 = metaplus5{metaHtml = htm1
                     ,metaLatex = tex1
                     , extra = extra6 }
        -- panrep2 = Panrep y2 p1

    -- putInform debug ["docrep2panrep 2"
    --         , showT extra6
    --             -- , "hpname", showT hpname
    --             -- , "\nauthorReduced", authorReduced
    --             ]

    -- if isIndexPage mdFile5
    --     then do
    --         extra7 <- collectIndex NoticeLevel0 pubf sett4 doughP mdFileDir extra6

    --         -- when (inform debug) $
    --         --     putIOwords ["\n ix2-2-----------------------docrep2panrep after collectIndex"
    --         --     , showPretty extra7 
    --         --     ]
    --         -- attention the dir/index is one level deeper than the files
    --         let
    --             ns  =  map (<.> ("docrep" :: FilePath ) ) ns2
    --             ns2 = map ((toFilePath bakedP) </>) . map (makeRelative (toFilePath doughP)) $ ns0
    --                         :: [FilePath]
    --             ds =  map ixfn (dirEntries  extra7) :: [FilePath]
    --             fs =   map ixfn (fileEntries extra7) :: [FilePath]
    --             ns0 = ds ++ fs 
    --             -- ixs =  map addIndex (dirEntries  extra7) ++ (fileEntries extra7)
    --             needs :: [FilePath] =   ns
    --         -- putIOwords ["\tds", showT ds]
    --         -- putIOwords ["\tfs", showT fs]

    --         -- putIOwords ["\tns", showT ns]
    --         -- putIOwords ["\tns2", showT ns2]
    --         -- putIOwords ["\tns0", showT ns0]
    --         -- when (inform debug) $
    --         --     putIOwords ["\n extra7------------------------docrep2panrep end if"
    --         --     , showPretty extra7
    --         --     , "needs ns with index.docrep", showT needs ]  

    --         return (metaplus6{extra=extra7}, needs )
    --     else
    --         return (metaplus6 , [])    
    return (metaplus6 , [])    

-- addIndex :: FilePath -> FilePath 
-- addIndex dir1 = dir1 </> "index.md"
 

 