-----------------------------------------------------------------------------
--
-- ModuKe      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports#-}

module Lib.Bake_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import Uniform.Test.TestHarness

import Lib.Foundation (progName, SiteLayout (..))
import Lib.Bake
-- import Lib.FileMgt
import Lib.Foundation_test (testLayout)
import Lib.Foundation (templatesDirName)
import Lib.CmdLineArgs (allFlags)
import Uniform.Json (AtKey(..), Value(..))
import Uniform.Pandoc -- (DocValue(..), unDocValue, docValueFileType)


-- the files to check - xxfn is in dough (the source)
-- the res files are in checks 
-- only the first (md -> docrep) uses the xxFn, the other use only the Res
blog1fn = makeAbsFile "/home/frank/Workspace8/ssg/docs/site/dough/Blog/blog1.md"  -- braucht extension
blog1res = makeAbsFile "/home/frank/Workspace8/ssg/docs/site/checks/Blog/blog1"  -- keine extension

drfnRefFn = makeAbsFile "/home/frank/Workspace8/ssg/docs/site/baked/PublicationList/postWithReference"
drfnRefRes = makeAbsFile "/home/frank/Workspace8/ssg/docs/site/checks/PublicationList/postWithReference"

    -- index scheint hier nicht testbar! 
indexedFn = makeAbsFile "/home/frank/Workspace8/ssg/docs/site/dough/Blog/index"
indexedBaked = makeAbsFile "/home/frank/Workspace8/ssg/docs/site/baked/Blog/index"
indexedRes = makeAbsFile "/home/frank/Workspace8/ssg/docs/site/checks/index"

-- test_addRefs   = testVar0FileIO "ssg" (drfnRefRes) "AddRefs"
--          op2test
--     where 
--         op2test :: (Path Abs File ) -> ErrIO DocRep
--         op2test (fn ) = do 
--             dr1 <- read8 fn docRepFileType  
--             dr2 <- docRepAddRefs dr1 
--             -- drRes <- readMarkdown2docrep resfn
--             return dr2

-- instance ShowTestHarness DocRep 
bakedP = bakedDir testLayout
doughP = doughDir testLayout 

---------- md -> docrep 
op2dr :: (Path Abs File, Path Abs File) -> ErrIO ()
op2dr (fn,resfn) = bakeOneFile2docrep doughP bakedP False allFlags fn testLayout resfn
-- test_bake2docrep = testVar0FileIO "ssg" (blog1fn,blog1res) "test_bake2docrep" op2dr
-- test_bake2docrepRef = testVar0FileIO "ssg" (drfnRefRes,drfnRefRes) "bakeOneFile2docrep" op2dr
test_bake2docrepIx = testVar0FileIO "ssg" (indexedFn,indexedRes) "bakeOneFile2docrep" op2dr

-- -- ein argument mehr : 
--     --  bakedP um die relative path absolut zu machen

-- ---- docrep -> panrep 
op2pan :: (Path Abs File, Path Abs File) -> ErrIO ()
op2pan (fn,resfn) = bakeOneFile2panrep  bakedP False allFlags fn testLayout resfn

-- test_docVal2panrep = testVar0FileIO "ssg" (blog1res,blog1res) "test_docVal2panrep" op2pan
-- test_docVal2htmlRef = testVar0FileIO "ssg" (drfnRefRes,drfnRefRes) "bakeDocValue2html" op2pan
test_docVal2panrepIx = testVar0FileIO "ssg" (indexedRes,indexedRes) "test_docVal2panrepIx" (op2pan bakedP)
        -- braucht baked weil dort die docrep existieren -- falsch


----- panrep -> html 
op2html :: (Path Abs File, Path Abs File) -> ErrIO () 
op2html (fn,resfn) = bakeOneFile2html True allFlags fn testLayout resfn


-- test_docVal2html = testVar0FileIO "ssg" (blog1res,blog1res) "test_docVal2html" op2html
-- test_docVal2htmlRef = testVar0FileIO "ssg" (drfnRefRes,drfnRefRes) "bakeDocValue2html" op2html
-- test_docVal2htmlIx = testVar0FileIO "ssg" (indexedFn,indexedRes) "test_docVal2htmlIx" op2html



-- test_bake2html = testVar0FileIO "ssg" (blog1res,blog1res) "bake2html" op2html
-- --     where 
-- --         op2html :: (Path Abs File, Path Abs File) -> ErrIO Text 
-- --         op2html (fn,resfn) = bakeOneFile2html False allFlags fn testLayout resfn
--     -- writes in same dir but different extension

-- -- testVar0FileIO :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b)
-- --             => Text -> a -> FilePath -> (a-> ErrIO b) -> IO ()
-- -- the arguments
-- -- testVar0FileIO progName  a resfile op = do

------- panrep -> texsnip 
op2texsnip :: (Path Abs File, Path Abs File) -> ErrIO () 
op2texsnip (fn,resfn) = bakeOneFile2texsnip False allFlags fn testLayout resfn

-- test_bake2texsnip = testVar0FileIO "ssg" (blog1res,blog1res) "test_bake2texsnip" op2texsnip
-- test_bake2texsnipRef = testVar0FileIO "ssg" (drfnRefRes,drfnRefRes) "bake2texsnip" op2texsnip
-- test_bake2texsnipIx = testVar0FileIO "ssg" (indexedRes,indexedRes) "bake2texsnip" op2texsnip


------ texsnip -> tex 
op2tex :: (Path Abs File, Path Abs File) -> ErrIO () 
op2tex (fn,resfn) = bakeOneFile2tex False allFlags fn testLayout resfn

-- test_bake2tex = testVar0FileIO "ssg" (blog1res,blog1res) "test_bake2tex" op2tex
-- test_bake2texRef = testVar0FileIO "ssg" (drfnRefRes,drfnRefRes) "bake2tex" op2tex
-- test_bake2texIx = testVar0FileIO "ssg" (indexedRes,indexedRes) "bake2tex" op2tex

------ tex -> pdf 
op2pdf :: (Path Abs File, Path Abs File) -> ErrIO () 
op2pdf (fn,resfn) = bakeOneFile2pdf False allFlags fn testLayout resfn

-- test_bake2pdf = testVar0FileIO "ssg" (blog1res,blog1res) "test_bake2pdf" op2pdf
-- test_bake2pdfRef = testVar0FileIO "ssg" (drfnRefRes,drfnRefRes) "bake2pdf" op2pdf
-- test_bake2pdfIx = testVar0FileIO "ssg" (indexedFn,indexedRes) "test_bake2pdfIx" op2pdf



instance ShowTestHarness () where


    --- OLD june 2020


--import Lib.Templating (Gtemplate(..), gtmplFileType, Dtemplate(..))
-- import Control.Lens
--import Data.Aeson
-- import Data.Aeson.Lens
-- import Data.Aeson
--import Data.Aeson.Encode.Pretty (encodePretty)
--import Data.ByteString.Lazy as BS (putStrLn)

-- test_findTemplate =
--     do
--         res <-  runErr $ do
--                     let source = "/home/frank/.SSG/landingPage.content.docval"
--                     val :: DocValue <- read8 (makeAbsFile source)  docValueFileType
--                     let val2 = unDocValue val  ::Value

--                     putIOwords ["test_findTemplate", "val2\n" ] -- ,  shownice $ val2]
-- --                    liftIO $ BS.putStrLn ( encodePretty$ val2)
--                     let ptemplate = getAtKey val2 "pageTemplate" 
--                         -- (val2) ^? key "pageTemplate" . _String
-- --                                :: Maybe FilePath
--                     putIOwords ["test_findTemplate", "found", showT ptemplate]
--                     return   ptemplate

--         assertEqual (Right (Just "page3"::Maybe Text))  res

--markdownToHTML4xdebug intext = do
--    pandoc <- markdownToPandoc False allFlags intext
--    pandocToContentHtml False pandoc
--
--test_pandoc_11_B_E, test_pandoc_12_B_E :: IO ()
--test_pandoc_11_B_E = test1FileIO progName   "resultB1" "resultBE1"  markdownToHTML4xdebug
--test_pandoc_12_B_E = test1FileIO progName   "resultB2" "resultBE2" markdownToHTML4xdebug
