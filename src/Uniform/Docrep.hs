---------------------------------------------------------------------------
--
-- Module      :  Uniform.Docrep
-- the abstract representation of the documents
-- see Filetypes4sites DocrepJSON
-----------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports 
            -fno-warn-unused-imports 
            -fno-warn-unused-matches #-}

module Uniform.Docrep
  ( module Uniform.Docrep,
    HTMLout,
    htmloutFileType,
    -- , Dtemplate
    -- , Template
    -- , renderTemplate
  )
where

import Control.Lens -- needed for the query expressions
  ( (^?),
  -- , (?~)
  -- , (&)
  -- , at
  )
import Data.Aeson.Lens (key)
import Data.Aeson.Types
  ( FromJSON (parseJSON),
    ToJSON,
    Value,
    parseMaybe,
  )
import GHC.Generics (Generic)
import Text.CSL as Pars (Reference, readBiblioFile, readCSLFile)
import Text.CSL.Pandoc as Bib (processCites)
import qualified Text.Pandoc as Pandoc
import Uniform.Filetypes4sites
import Uniform.HTMLout
  ( HTMLout (HTMLout),
    html5Options,
    htmloutFileType,
    writeHtml5String,
  )
import Uniform.Json
  ( AtKey (getAtKey, putAtKey),
    ErrIO,
    FromJSON (parseJSON),
    ToJSON,
    Value,
    mergeRightPref,
  )
import Uniform.PandocImports  
import UniformBase



readMarkdown2docrepJSON :: MarkdownText -> ErrIO DocrepJSON
-- | read a md file into a DocrepJSON
-- reads the markdown file with pandoc and extracts the yaml metadaat
-- the metadata are then copied over to the meta part
-- and converted in regular json
-- attention: there is potential duplication 
-- as the metadata are partially duplicated
readMarkdown2docrepJSON md = do
    pd <- readMarkdown2 md
    let meta2                 = flattenMeta . getMeta $ pd
    return (DocrepJSON meta2 pd)



docrep2panrep :: DocrepJSON -> ErrIO Panrep
-- ^ transform a docrep to a panrep (which is the pandoc rep)
-- does process the references
-- and will do index, but this goes to ssg
docrep2panrep dr1@(DocrepJSON y1 p1) = do
  (DocrepJSON y2 p2) <- addRefs False dr1  -- was already done in bakeOneMD2docrep
  return $ Panrep y2 p2

------------------------------------

panrep2html :: Panrep -> ErrIO HTMLout
-- ^ transform a docrep to a html file
-- needs teh processing of the references with citeproc
panrep2html pr1@(Panrep y1 p1) = do
  -- dr2 <- addRefs pr1
  h1 <- unPandocM $ writeHtml5String html5Options p1
  return . HTMLout $ h1

--------------------------------
addRefs :: Bool -> DocrepJSON -> ErrIO DocrepJSON
-- ^ add the references to the pandoc block
-- the biblio is in the yam (otherwise nothing is done)
-- ths cls file must be in the yam

-- processCites :: Style -> [Reference] -> Pandoc -> Pandoc

-- Process a Pandoc document by adding citations formatted according to a CSL style. Add a bibliography (if one is called for) at the end of the document.
-- http://hackage.haskell.org/package/citeproc-hs-0.3.10/docs/Text-CSL.html
--   m <- readBiblioFile "mybibdb.bib"
--   s <- readCSLFile "apa-x.csl"
--   let result = citeproc procOpts s m $ [cites]
--   putStrLn . unlines . map (renderPlainStrict) . citations $ result

addRefs debugflag dr1@(DocrepJSON y1 p1) = do
  -- the biblio entry is the signal that refs need to be processed
  -- only refs do not work
  when debugflag $ putIOwords ["addRefs", showT dr1, "\n"]
  let biblio1 = getAtKey y1 "bibliography" :: Maybe Text
  maybe (return dr1) (addRefs2 debugflag dr1) biblio1

addRefs2 ::
  (MonadIO m, MonadError m, ErrorType m ~ Text) => Bool -> 
  DocrepJSON ->
  Text ->
  m DocrepJSON
addRefs2 debugx dr1@(DocrepJSON y1 p1) biblio1 = do
--   let debugx = False 
  when debugx $ putIOwords ["addRefs2-1", showT dr1, "\n"]
  let style1 = getAtKey y1 "style" :: Maybe Text
      refs1 = y1 ^? key "references" :: Maybe Value -- is an array
      nocite1 = getAtKey y1 "nocite" :: Maybe Text

  when debugx $
    putIOwords
      [ "addRefs2-2",
        "\n biblio",
        showT biblio1, -- is only biblio "resources/BibTexLatex.bib"
        "\n style",
        showT style1, -- style Just "/home/frank/Workspace8/ssg/docs/site/dough/resources/chicago-fullnote-bibliography-bb.csl"
        "\n refs",
        showT refs1,
        "\n nocite",
        showT nocite1
      ]

  let loc1 = Just "en" -- TODO depends on language to be used for
  -- for the conventions in the lit list
  -- must be 2 char (all other seems to be difficult with pandoc-citeproc)
  -- change to new citeproc TODO
  let refs2 = fromJustNote "refs in addRefs2 vcbnf refs2" refs1 :: Value
  let refs3 = fromJSONValue refs2 -- :: Result [Reference]
  let refs4 = fromJustNote "addRefs2 08werwe refs4" refs3 :: [Reference]

  let bibliofp =
        t2s biblio1 :: FilePath
  let stylefp =
        t2s . fromJustNote "style1 in addRefs2 wer23" $ style1 :: FilePath
  --  Raised the exception:
  -- ["runErr2action","Safe.fromJustNote Nothing, style1 in docrepAddRefs wer23\nCallStack (from HasCallStack):\n  fromJustNote, called at ./Uniform/DocrepJSON.hs:165:19 in uniform-pandoc-0.0.2-CQ6TrBvcdAe7Crud3c6Rca:Uniform.Docrep"]
  -- because the style was empty
  when debugx $ putIOwords ["addRefs2-3-1", "done"]

  biblio2 <- callIO $ Pars.readBiblioFile (const True) bibliofp
  when debugx $ putIOwords ["addRefs2-3-2", "done"]
  style2 <- callIO $ Pars.readCSLFile loc1 stylefp
  -- error with language (de_at, but de or en works)
  when debugx $ putIOwords ["addRefs2-3-3", "done"]

  let refsSum = refs4 ++ biblio2
  let p2 = processCites style2 refsSum p1

  when debugx $ putIOwords ["addRefs2-4", "p2\n", showT p2]

  return (DocrepJSON y1 p2)

mergeAll :: DocrepJSON -> [Value] -> DocrepJSON
-- ^ merge the values with the values in DocRec -- last winns
-- issue how to collect all css?
mergeAll (DocrepJSON y p) vs = DocrepJSON (mergeRightPref $ y : vs) p

instance AtKey DocrepJSON Text where
  getAtKey dr k2 = getAtKey (yam dr) k2

  putAtKey k2 txt (DocrepJSON y p) = DocrepJSON (putAtKey k2 txt y) p

-- instance AtKey DocrepJSON Bool where
--   getAtKey dr k2 = getAtKey (yam dr) k2

--   putAtKey k2 b dr = DocrepJSON $ putAtKey k2 b (unDocrep meta2)
