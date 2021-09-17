---------------------------------------------------------------------------
--
-- Module      :  Uniform.Pan2texsnip
-----------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans
            -fno-warn-missing-signatures
            -fno-warn-missing-methods
            -fno-warn-duplicate-exports
            -fno-warn-unused-imports
            -fno-warn-unused-matches #-}

{- | the representation with indices
 ready for processing to HTML or to TexSnip -> Tex -> Pdf
-}
module Wave.Pan2texsnip (
    module Wave.Pan2texsnip,
) where

import           Data.Default
import           Foundational.LayoutFlags
import           Foundational.MetaPage
import           GHC.Generics                 (Generic)
import           Lib.IndexMake
import           Lib.IndexCollect
import           Lib.Templating
-- import Text.CSL as Pars (Reference, readBiblioFile, readCSLFile)
-- import Text.CSL.Pandoc as Bib (processCites)
-- import qualified Text.Pandoc as Pandoc
import           Foundational.Filetypes4sites
import           Uniform.Json
import           Uniform.Pandoc
import           Uniform2.HTMLout
import           UniformBase

-- ------------------------------------------------docrep -> panrep

-- -- {- ^ transform a docrep to a panrep (which is the pandoc rep)
-- --  does process the references
-- --  and will do index, but this goes to ssg
-- -- -}

-- -- docrep2panrep debug layout (Docrep y1 p1) = do
-- --     let bakedP = bakedDir layout
-- --     let pr = Panrep
-- --                 { panyam = y1
-- --                 , panpan = p1
-- --                 }
-- --     --
-- --     if dyIndexPage . panyam $ pr 
-- --         then do 
-- --             let m1 = panyam pr
-- --             let ix1 = initializeIndex   m1
-- --             ix2 <- completeIndex debug bakedP ix1
-- --         -- todo put ix2 into pr
-- --             let m2 = m1{dyIndexEntry = ix2}
-- --             return pr{panyam = m2}
-- --         else 
-- --             return pr

-- -- do
-- -- --   (DocrepJSON y2 p2) <- addRefs False dr1 -- was already done in  bakeOneMD2docrep
-- -- return
-- --  $ Panrep y1 p1

-- -- ------------------------------------ panrep2html
-- -- panrep2html :: Panrep -> ErrIO HTMLout
-- -- implements the bake
-- panrep2html :: NoticeLevel -> SiteLayout -> Panrep -> ErrIO HTMLout
-- panrep2html debug layout dr1 = do
--     -- let templateP = templatesDir layout
--     m4 <- convertIndexEntries (panyam dr1) -- move to
--     -- p <- panrep2htmlP debug templateP dr4
--     let dr4 = dr1{panyam = m4}
--     p :: HTMLout <- putValinMaster debug dr4 (templatesDir layout)
--     when (informNone debug) $ putIOwords ["\n panrep2html done"]
--     return p

-- -- step1
-- panrep2panrep1 :: NoticeLevel -> SiteLayout -> Panrep -> ErrIO Panrep1
-- panrep2panrep1 debug layout dr1 = do
--     -- let templateP = templatesDir layout
--     m4 <- convertIndexEntries (panyam dr1) -- move to
--     -- p <- panrep2htmlP debug templateP dr4
--     let dr4 = dr1{panyam = m4}
--     when (informNone debug) $ putIOwords ["\n panrep2panrep1 done"]
--     return . Panrep1 $ dr4

-- panrep12html :: NoticeLevel -> SiteLayout -> Panrep1 -> ErrIO HTMLout
-- panrep12html debug layout dr4 = do
--     -- let templateP = templatesDir layout
--     -- dr4 <- convertIndexEntries dr1 -- move to
--     p :: HTMLout <- putValinMaster debug (unPanrep1 dr4) (templatesDir layout)
--     when (informNone debug) $ putIOwords ["\n panrep12html done"]
--     return p


-- -- panrep2htmP :: NoticeLevel  -> Path Abs Dir -> Panrep ->ErrIO Text
-- -- panrep2htmP debug templateP dr4 = do
-- --     -- dr4 <- convertIndexEntries dr1 -- move to
-- --     p :: Text <- putValinMaster False dr4 templateP
-- --     return p


-- -- -- where does this belong?

-- -- panrep22html :: Panrep -> ErrIO HTMLout
-- -- -- ^ transform a docrep to a html file
-- -- -- needs teh processing of the references with citeproc
-- -- panrep2html pr1@(Panrep y1 p1) = do
-- --   -- dr2 <- addRefs pr1
-- --   h1 <- unPandocM $ writeHtml5String html5Options p1
-- --   return . HTMLout $ h1
