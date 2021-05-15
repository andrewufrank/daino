{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

-- {-# LANGUAGE TypeSynonymInstances  #-}
-- {-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports #-}

-- | test the transformation from panrep to html
module Lib.Panrep2html_test where

 
import Test.Framework
import Uniform.Test.TestHarness
import UniformBase
import Foundational.Filetypes4sites
-- import Uniform2.Docrep
import Foundational.Foundation
import ShakeBake.ReadSettingFile
import Wave.Panrep  
import Uniform2.HTMLout

import Lib.Templating -- (applyTemplate2, convGmaster)
import Lib.IndexMake

programName = "ssg" :: Text
-- settingsFile =   "settings2File"

test_null = assertEqual 0 0

settingsFileStored = "settings2File"

-- panrep2html :: p -> SiteLayout -> Panrep -> ErrIO HTMLout
-- panrep2html debug layout dr1 = do
--     let templateP = templatesDir layout
--     dr4 <- convertIndexEntries dr1 -- move to
--     -- p <- panrep2htmlP debug templateP dr4 
--     p :: HTMLout <- putValinMaster False dr4 templateP
--     return p

test_panrep_layout2 = test2FileIO programName settingsFileStored "index.panrep" "HTMLout" (panrep2html True )

test_panrep2convertIndexEntries = test1FileIO programName "index.panrep" "index.converted.panrep" (convertIndexEntries)

test_putValinMaster = test2FileIO programName "index.converted.panrep" settingsFileStored "index.valput" 
    (\d t -> putValinMaster True d (templatesDir t))
-- nothing inserted

test_getPanrepVal = test1File programName "index.panrep" "index.val"
    (show . panyam)

--     test2FileIO :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b
--                 , Zeros c, Eq c, Show c, Read c, ShowTestHarness c
--                 , Zeros d, Read d, Show d, Eq d, ShowTestHarness d)
--             => Text ->  FilePath -> FilePath -> FilePath
--                     -> ( b -> c -> ErrIO d) -> IO ()
-- test2FileIO progName  startfile secfile resfile op = 

instance ShowTestHarness (Path Abs File)
instance ShowTestHarness SiteLayout
instance ShowTestHarness Panrep
instance ShowTestHarness HTMLout


-- test1FileIO :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b
--                 , Zeros c, Eq c, Show c, Read c, ShowTestHarness c)
--             => Text ->  FilePath -> FilePath -> (b -> ErrIO c) -> IO ()
-- test1FileIO progName  startfile resfile op = do

-- templateDir = addDir (themeDir testLayout) templatesDirName
--page3  = addDir templateDir (makeRelFile "page3")
-- master3 = addDir templateDir masterTemplateFileName_
--page33 = addDir templateDir(makeRelFile "page33")
--
-- applyTemplate3x :: DocValue -> ErrIO Text
-- applyTemplate3x dval = do
--      template <- read8 master3 dtmplFileType
--      ht       <- applyTemplate3 template dval
--      return . unHTMLout $ ht
-- ----                        (makeRelFile "pandocDefault.html"::Path Rel File)
----
----
-- test_teplating_16_AF_EF = test1FileIO progName     "resultAF6" "resultEF6" applyTemplate3x

-- test_templating_11_AG_EG, test_templating_12_AG_EG :: IO ()
-- test_templating_11_AG_EG =
--      test1FileIO progName "resultAG1" "resultEG1" applyTemplate3x
-- test_templating_12_AG_EG =
--      test1FileIO progName "resultAG2" "resultEG2" applyTemplate3x
-- test_templating_13_AG_EG =
--      test1FileIO progName "resultAG3" "resultEG3" applyTemplate3x
-- test_templating_14_AG_EG =
--      test1FileIO progName "resultAG4" "resultEG4" applyTemplate3x
-- test_templating_15_AG_EG =
--      test1FileIO progName "resultAG5" "resultEG5" applyTemplate3x
-- test_templating_16_AG_EG =
--      test1FileIO progName "resultAG6" "resultEG6" applyTemplate3x

-- test_readAF6 = do
--           res <-  runErr $ do
--                     st <- readFile2 (makeAbsFile "/home/frank/.SSG/resultAG6.x")
--                     let st2 = readNote "readAF6test" st :: DocValue
--                     return st2
--           assertEqual (Left text0) res

-- instance  ShowTestHarness DocValue where
-- instance ShowTestHarness HTMLout
--
--
--fromRightNoteString ::   Text -> Either String b -> b
--fromRightNoteString msg (Left a) = errorT ["fromRight", showT a, msg]
--fromRightNoteString _ (Right a) = a
