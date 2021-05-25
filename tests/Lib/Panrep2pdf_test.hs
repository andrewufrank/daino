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
import Uniform.Test.TestHarness
import Foundational.MetaPage 
import Foundational.Foundation
import Foundational.Filetypes4sites
import Wave.Md2doc
import Wave.Panrep2pdf
import Uniform.Pandoc
-- import Uniform.Json
import UniformBase
import Lib.Md2doc_test

-- import Lib.Indexing

testing_panrep2texsnip :: FilePath -> IO ()
-- | test to produce texsnip
testing_panrep2texsnip f1  = test1FileIO "ssg" ("panrep_" <> f1) (f1 <> "_texsnip" )(panrep2texsnip NoticeLevel0 ) 

test_blog1_panrep2texsnip = testing_panrep2texsnip   "blog1"
test_index_panrep2texsnip = testing_panrep2texsnip "index"   
test_postwk_panrep2texsnip = testing_panrep2texsnip "postwk"   
test_withRef_panrep2texsnip = testing_panrep2texsnip "withRef"   

testing_texsnip2tex :: FilePath -> IO ()
-- | test to produce texSnipFileType 
testing_texsnip2tex f1  = test1FileIO "ssg"  (f1 <> "_texsnip" )("tex_" <> f1) 
    (\filein -> do
                lat1 <- texsnip2tex NoticeLevel0 filein
                write8 (testDir layoutDefaults </> makeRelFile (f1 <> ".tex")) texFileType lat1
                return lat1                
                ) 

test_blog1_texsnip2tex = testing_texsnip2tex   "blog1"
test_index_texsnip2tex = testing_texsnip2tex "index"   
test_postwk_texsnip2tex = testing_texsnip2tex "postwk"   
test_withRef_texsnip2tex = testing_texsnip2tex "withRef"   


testing_tex2pdf :: FilePath -> IO ()
-- | test to produce pdf
-- difficult: 
-- pdf process is on files (not text)
-- requires current dir 
testing_tex2pdf f1  = test1FileIO "ssg"  ("tex_" <> f1) ("pdfResult_" <> f1) (do 
              \(d::Text) ->
                  do 
                    (tex2pdf NoticeLevel0  
                        ( (testDir layoutDefaults  </> makeRelFile ( f1 <> ".tex")))
                        ( (testDir layoutDefaults  </> makeRelFile ( f1 <> ".pdf"))))
                    let res = "pdfwritten" :: Text 
                    return res      
                    -- return ("pdfwritten" :: Text)
            )

test_blog1_tex2pdf = testing_tex2pdf   "blog1"
test_index_tex2pdf = testing_tex2pdf "index"   
test_postwk_tex2pdf = testing_tex2pdf "postwk"   
test_withRef_tex2pdf = testing_tex2pdf "withRef"   

instance ShowTestHarness ()
instance ShowTestHarness TexSnip 
instance ShowTestHarness Latex 
instance ShowTestHarness Panrep 
