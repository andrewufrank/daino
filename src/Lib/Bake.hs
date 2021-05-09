---------------------------------------------------------------------
--
-- Module      :   the  process to convert
--              files from md to all the formats required
--              orginals are found in dire doughDir and go to bakeDir
--
----------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports 
            -fno-warn-unused-imports 
            -fno-warn-unused-matches #-}

module Lib.Bake
 
where

import Lib.CheckInput (completeDocRep) --                 ( getTripleDoc )
import Lib.CmdLineArgs (PubFlags (..))
import Lib.Foundation
  ( SiteLayout (..),
    templatesDir,
  )
import Lib.IndexMake (convertIndexEntries)
import Lib.Indexing (addIndex2yam)
import Lib.Templating (putValinMaster)
import qualified Path.IO as Path
  ( getTempDir,
  )
import Uniform.Docrep

import Uniform.Markdown
import Uniform.PandocImports 
import Uniform.ProcessPDF
import Uniform.Filetypes4sites

import Uniform.Shake ()
import UniformBase

type BakeOp =
  Bool ->
  PubFlags ->
  -- | md file
  Path Abs File ->
  SiteLayout ->
  Path Abs File ->
  ErrIO ()

bakeOneMD2docrep :: BakeOp --    MD -> DOCREP
bakeOneMD2docrep debug flags inputFn layout resfn2 = do
  putIOwords
    [ "\n-----------------",
      "bakeOneFile2docrep 1 fn",
      showT inputFn,
      "debug",
      showT debug,
      "\n resfn2",
      showT resfn2
    ]

  md1 <- read8 inputFn markdownFileType
  let bakedP = bakedDir layout
  let doughP = doughDir layout

  dr1 <- readMarkdown2docrepJSON md1
  -- with a flattened version of json from Pandoc
  -- what does it contain?
  putIOwords ["readMarkdown2docrepJSON", "dr1", showT dr1]

  -- check
  -- the fields for the index are prepared
  -- merge the yaml metadata with default to have the 
  -- necessary values set 

  dr2 <- completeDocRep doughP bakedP inputFn dr1
  -- does this use the listed refs?
  dr3 <- addRefs debug dr2

  -- TODO needs refs
  -- let needs1  = docrepNeeds docrep1  :: [FilePath]
  -- need  needs1  -- TDO this is in the wrong monad
  -- dr4 <- addIndex2yam debug dr3
  -- this will be done twice in html and tex

  write8 resfn2 docrepFileType dr3
  when debug $
    putIOwords
      [ "\n-----------------",
        "bakeOneFile2docrep done fn",
        showT resfn2
      ]
  return () -- (needs1) --"ok bakeOneFile2docrep"

bakeOneDocrep2panrep :: BakeOp --  DOCREP -> PANREP
-- TODO
bakeOneDocrep2panrep debug flags inputFn layout resfn2 = do
  putIOwords
    [ "\n-----------------",
      "bakeOneFile2panrep 1 fn",
      showT inputFn,
      "debug",
      showT debug,
      "\n resfn2",
      showT resfn2
    ]
  let bakedP = bakedDir layout

  dr1 <- read8 inputFn docrepFileType

  p1 <- docrep2panrep dr1
  -- adds refs but not yet used in tex2pdf!

  p2 <- addIndex2yam bakedP debug p1
  -- but needs processing to use (indexMake)

  write8 resfn2 panrepFileType p2 -- content is html style
  when debug $
    putIOwords
      ["\n-----------------", "bakeOneFile2panrep done fn", showT p2]
  return () --"ok bakeOneFile2docrep"

bakeOnePanrep2html :: BakeOp -- TODO
bakeOnePanrep2html debug flags inputFn layout resfn2 = do
  putIOwords
    [ "\n-----------------",
      "bakeOneFile2html 1 fn",
      showT inputFn,
      "debug",
      showT debug,
      "\n resfn2",
      showT resfn2
    ]
  let templateP = templatesDir layout

  dr1 <- read8 inputFn panrepFileType

  -- do index
  dr4 <- convertIndexEntries dr1 -- move to

  -- h1  <- panrep2html dr4

  p :: HTMLout <- putValinMaster False dr4 templateP
  write8 resfn2 htmloutFileType p -- content is html style
  when debug $
    putIOwords
      ["\n-----------------", "bakeOneFile2html done fn", showT resfn2]
  return () --"ok bakeOneFile2docrep"

bakeOnePanrep2texsnip :: BakeOp --  PANREP -> TEXSNIP
-- TODO
bakeOnePanrep2texsnip debug flags inputFn layout resfn2 = do
  putIOwords
    [ "\n-----------------",
      "bakeOneFile2texsnip 1 fn",
      showT inputFn,
      "debug",
      showT debug,
      "\n resfn2",
      showT resfn2
    ]

  dr1 <- read8 inputFn panrepFileType

  snip1 <- panrep2texsnip dr1

  write8 resfn2 texSnipFileType snip1 -- content is html style
  when debug $
    putIOwords
      ["\n-----------------", "bakeOneFile2html done fn", showT resfn2]
  return () --"ok bakeOneFile2docrep"

bakeOneTexsnip2tex :: BakeOp -- TEXSNIP -> TEX
-- TODO
bakeOneTexsnip2tex debug flags inputFn layout resfn2 = do
  putIOwords
    [ "\n-----------------",
      "bakeOneFile2tex 1 fn",
      showT inputFn,
      "debug",
      showT debug,
      "\n resfn2",
      showT resfn2
    ]

  snip1 <- read8 inputFn texSnipFileType

  -- latexParam :: LatexParam <- fromJSONerrio . snipyam $ snip1
  -- putIOwords ["bakeOneFile2tex", showT latexParam]
  let tex1 = tex2latex zero [snip1]

  write8 resfn2 texFileType tex1 -- content is html style
  when debug $
    putIOwords
      ["\n-----------------", "bakeOneFile2tex done fn", showT resfn2]
  return () --"ok bakeOneFile2docrep"

bakeOneTex2pdf :: BakeOp
-- TODO
bakeOneTex2pdf debug flags inputFn layout resfn2 = do
  putIOwords
    [ "\n-----------------",
      "bakeOneFile2pdf 1 fn:",
      showT inputFn,
      "\n\t debug:",
      showT debug,
      "\n\t resfn2:",
      showT resfn2
    ]

  let refDir =
        makeAbsDir . getParentDir . toFilePath $ inputFn :: Path Abs Dir
  writePDF2 debug inputFn resfn2 refDir -- content is html style
  when debug $
    putIOwords
      ["\n-----------------", "bakeOneFile2pdf done fn", showT resfn2]
  return () --"ok bakeOneFile2docrep"
