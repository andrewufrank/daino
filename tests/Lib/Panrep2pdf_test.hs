-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- {-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports #-}

module Lib.Panrep2pdf_test 
     where

import Test.Framework
-- import Uniform.Test.TestHarness
import Foundational.CmdLineFlags
import Foundational.SettingsPage
import Foundational.Filetypes4sites
import Wave.Md2doc
-- import Wave.Docrep2panrep
import Wave.Panrep2pdf
import Uniform.Pandoc
-- import Uniform.Json
import UniformBase
import Lib.Md2doc_test
import Data.Hash

-- settFn = makeAbsFile 
--     "/home/frank/Workspace11/daino/settingsTest.yaml"
fnmd2a = makeAbsFile "/home/frank/Workspace11/dainoSite/ReadMe/index.md"
fnmd3 = makeAbsFile "/home/frank/Workspace11/dainoSite/ReadMe/03tree.md"
resLatex = makeAbsFile"/home/frank/tests/latexTest"


-- test regular processing
-- test_toLatex = do 
--     let debug = NoticeLevel0   -- avoid_output_fromHere_down 
--     res1 <- runErr $ do 
--         metaplus5 <- setup_md2metaplus settingsDainoSite fnmd3 
--             -- let debug = NoticeLevel0
--             -- sett3 <- readSettings debug settFn 
--                     -- this is a particular settingsTest.yaml
--             -- metaplus1 <- readMarkdownFile2docrep debug sett3 fn
--         (metap1,_) <- docrep2panrep NoticeLevel0 
--                         (def::PubFlags)  metaplus5
--         texsnip <- panrep2texsnip debug metap1
--         putIOwords ["test_toLatex setup texsnip done"]

--         (lat1, _, tt1) <- texsnip2tex debug def texsnip
--         putInform debug ["test_toHtmlTest pr \n", tt1]
--         write8 resLatex texFileType lat1
--         write8 resLatex ttlFileType tt1
--         let hash1 = show . hash . show $  lat1 :: String
--         return hash1

--     assertEqual (Right "Hash {asWord64 = 14632867472327410945}") 
--         res1

-- import Lib.IndexCollect

-- testing_panrep2texsnip :: FilePath -> IO ()
-- -- | test to produce texsnip
-- testing_panrep2texsnip f1  = test1FileIO "daino" (f1 <> "_panrep") (f1 <> "_texsnip" )(panrep2texsnip NoticeLevel0 ) 

-- test_blog1_panrep2texsnip = testing_panrep2texsnip   "01blog1"
-- test_index_panrep2texsnip = testing_panrep2texsnip "index"   
-- test_postwk_panrep2texsnip = testing_panrep2texsnip "03postwk"   
-- test_withRef_panrep2texsnip = testing_panrep2texsnip "02withRef"   

-- testing_texsnip2tex :: FilePath -> IO ()
-- -- | test to produce texSnipFileType 
-- testing_texsnip2tex f1  = test1FileIO "daino"  
--     (f1 <> "_texsnip" )(f1 <> "_tex" ) 
--     (\filein -> do
--                 lat1 <- texsnip2tex NoticeLevel0 filein
--                 write8 (testDir </> makeRelFile (f1 <> ".tex")) texFileType lat1
--                 return lat1                
--                 ) 

testDir = makeAbsDir $
                ("/home/frank" :: FilePath)
                    </> (".daino" :: FilePath)
                    -- must correspond to what testharness is using

                    
-- test_blog1_texsnip2tex = testing_texsnip2tex   "01blog1"
-- test_index_texsnip2tex = testing_texsnip2tex "index"   
-- test_postwk_texsnip2tex = testing_texsnip2tex "03postwk"   
-- test_withRef_texsnip2tex = testing_texsnip2tex "02withRef"   


-- testing_tex2pdf :: FilePath -> IO ()
-- -- | test to produce pdf
-- -- difficult: 
-- -- pdf process is on files (not text)
-- -- requires current dir 
-- testing_tex2pdf f1  = test1FileIO "daino"  
--     (f1 <> "_tex") (f1 <> "_pdfResult" <> f1) (do 
--               \(d::Text) ->
--                   do 
--                     (tex2pdf NoticeLevel0  
--                         ( (testDir   </> makeRelFile ( f1 <> ".tex")))
--                         ( (testDir   </> makeRelFile ( f1 <> ".pdf"))))
--                     let res = "pdfwritten" :: Text 
--                     return res      
--                     -- return ("pdfwritten" :: Text)
--             )

-- test_blog1_tex2pdf = testing_tex2pdf   "01blog1"
-- test_index_tex2pdf = testing_tex2pdf "index"   
-- test_postwk_tex2pdf = testing_tex2pdf "03postwk"   
-- test_withRef_tex2pdf = testing_tex2pdf "02withRef"   

-- instance ShowTestHarness ()
-- instance ShowTestHarness TexSnip 
-- instance ShowTestHarness Latex 
-- instance ShowTestHarness Panrep 
