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
  ( module Lib.Bake,
    bakeOneFile2docrep,
    -- , bakeOneFile2html
    -- , bakeOneFile2texsnip
    -- , bakeOneTexSnip2pdf
    -- , bakeDocValue2html
  )
where

--  ( replaceExtension' )

import Lib.CheckInput (checkDocrep) --                 ( getTripleDoc )
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
  ( HTMLout,
    addRefs,
    docrep2panrep,
    docrepFileType,
    htmloutFileType,
  )
import Uniform.Markdown
import Uniform.PandocImports ( panrepFileType, texSnipFileType )

import Uniform.ProcessPDF
  ( panrep2texsnip,
    tex2latex,
    texFileType,
    writePDF2,
  )
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

bakeOneFile2docrep :: BakeOp --    MD -> DOCREP
bakeOneFile2docrep debug flags inputFn layout resfn2 = do
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

  dr1 <- readMarkdown2docrep md1

  -- check
  -- the fields for the index are prepared

  dr2 <- checkDocrep doughP bakedP inputFn dr1
  -- does this use the listed refs?
  dr3 <- addRefs dr2

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

bakeOneFile2panrep :: BakeOp --  DOCREP -> PANREP
-- TODO
bakeOneFile2panrep debug flags inputFn layout resfn2 = do
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

bakeOneFile2html :: BakeOp --  PANREP -> HTML
-- TODO
bakeOneFile2html debug flags inputFn layout resfn2 = do
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

bakeOneFile2texsnip :: BakeOp --  PANREP -> TEXSNIP
-- TODO
bakeOneFile2texsnip debug flags inputFn layout resfn2 = do
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

bakeOneFile2tex :: BakeOp -- TEXSNIP -> TEX
-- TODO
bakeOneFile2tex debug flags inputFn layout resfn2 = do
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

bakeOneFile2pdf :: BakeOp
-- TODO
bakeOneFile2pdf debug flags inputFn layout resfn2 = do
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
