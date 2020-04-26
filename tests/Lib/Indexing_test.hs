-----------------------------------------------------------------------------
--
-- Module      :   indexing tests
-- only the tests for processing test cases with ErrIO 
-- not currently used 
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports #-}

module Lib.Indexing_test where

-- import Lib.FileMgt
import           Lib.Foundation (layoutDefaults, doughDir)
import           Lib.Foundation (progName, SiteLayout(..), templatesDirName)
import           Lib.Foundation_test (testLayout)
--import Uniform.Strings
import           Lib.Indexing -- (applyTemplate2, convGmaster)
--import Uniform.Filenames
import           Lib.Templating
import           Test.Framework
--import Text.DocTemplates
import           Uniform.Test.TestHarness
import           Uniform.Time (readDate3, UTCTime(..))
import           Lib.CmdLineArgs (allFlags)
import           Lib.CheckInput (MetaRec(..), SortArgs(..)
                    , PublicationState(..), makeRelPath
                    , checkOneMdFile)
-- import Uniform.Pointless (snd3)

    -- TEST DIRS 
test_dough2 =  assertEqual dough2path dough2  
dough2 = doughDir testLayout
dough2path = makeAbsDir "/home/frank/Workspace8/ssg/docs/site/dough/"

-- resmr = "/Blog/postwk.md"
test_blogDir = assertEqual blogDirPath blogDir
blogDir = doughDir testLayout </> makeRelDir "Blog"
blogDirPath = makeAbsDir "/home/frank/Workspace8/ssg/docs/site/dough/Blog"

test_url = assertEqual "/Blog/postwk.md" $ makeRelPath dough2 linkIn
linkIn = doughDir testLayout </> makeRelFile "Blog/postwk.md" :: Path Abs File

test_MetaRec_postwk = do 
    res <- runErr $   getMetaRecs testLayout linkIn 
    assertEqual (Right metaRecPost1) res 

linkIndex1 = doughDir testLayout </> makeRelFile "Blog/index.md" 
                        :: Path Abs File 

test_MetaRec_index1 = do 
    res <- runErr $   getMetaRecs testLayout linkIndex1 
    assertEqual (Right metaRecIndex1) res 


metaRecPost1 = MetaRec
  { fn = toFilePath linkIn
  , relURL = "/Blog/postwk.md"
  , title = "postwk with image"
  , abstract = "A silly text not needing an abstract."
  , author = "AUF"
  , date = "2019-01-04 00:00:00 UTC"
  , publicationState = PSpublish
  , bibliography = Nothing
  , bibliographyGroup = Nothing
  , keywords = Just "test"
  , pageTemplate = Just "/home/frank/Workspace8/ssg/theme/templates/page3.yaml"
  , indexPage = False
  , indexSort = SAzero
  }


metaRecIndex1 = MetaRec
  { fn = toFilePath linkIndex1
  , relURL = "/Blog/index.md"
  , title = "primary index for Blog"
  , abstract = "The directory for experiments."
  , author = "AUF"
  , date = "2019-01-04 00:00:00 UTC"
  , publicationState = PSpublish
  , bibliography = Nothing
  , bibliographyGroup = Nothing
  , keywords = Just "test"
  , pageTemplate = Just "/home/frank/Workspace8/ssg/theme/templates/page3.yaml"
  , indexPage = True
  , indexSort = SAtitle
  }
