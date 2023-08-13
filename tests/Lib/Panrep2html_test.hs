-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- {-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports #-}

module Lib.Panrep2html_test 
     where

import Test.Framework
-- import Uniform.Test.TestHarness
import Foundational.MetaPage 
import Foundational.SettingsPage
import Foundational.Filetypes4sites
import Wave.Md2doc
import Wave.Docrep2panrep
import Wave.Panrep2pdf
import Uniform.Pandoc
-- import Uniform.Json
import UniformBase
import Lib.Md2doc_test
import Uniform.Http ( HTMLout (HTMLout) ) 
import Data.Hash

-- import Lib.IndexCollect

testDir = makeAbsDir $
                ("/home/frank" :: FilePath)
                    </> ("tests" :: FilePath)
                    -- must correspond to what testharness is using
                    -- does not

fnmd1 = makeAbsFile "/home/frank/Workspace11/dainoSite/ReadMe/index.md"
reshtml = makeAbsFile"/home/frank/tests/html1"
testTemplate = makeAbsFile "/home/frank/Workspace11/daino/tests/data/metaplusHtml.dtpl"

panrep2htmlForTest :: NoticeLevel -> Settings -> Panrep -> ErrIO HTMLout
panrep2htmlForTest debug  sett3x metaplus4 = do
    let sett3 = sett metaplus4
    let mf = testTemplate
    -- let mfn = templatesDir layout </> mf
    -- let masterfn = templatesDir (siteLayout sett3) </> mf
    -- let h = "0" -- maybe 0 $ M.lookup headerShift . unMeta $ meta4
    -- when (inform debug) $
    --     putIOwords ["\n\t---------------------------panrep2htmlForTest"
    --             , "shiftHeaderLevel"
    --             , showT h] 

    htmlTempl  <- compileTemplateFile2 mf

    htm1 <- meta2xx writeHtml5String2 (metap metaplus4)
    let metaplus5 = metaplus4{metaHtml = htm1}
-- copied
    -- htpl2 <- compileTemplateFile2 metaplusHtml -- fnminilatex
    let hpl1 = renderTemplate htmlTempl (toJSON metaplus5)  -- :: Doc Text
    -- putIOwords ["tpanrep2htmlForTest pl1 \n", showT tpl1]
    let ht1 = render (Just 50) hpl1  -- line length, can be Nothing
    -- putIOwords ["panrep2htmlForTest res1 \n", res1]
    -- write8   fnPlusres htmloutFileType (HTMLout ht1)



-- 
    -- hres <- meta2hres htmlTempl metaplus4
    putIOwords ["panrep2htmlForTest render html done"
        , "hres", ht1]
    -- bakeOnePanrep2html will write to disk
    return . HTMLout $ ht1

test_toHtmlTest = do 
    res1 <- runErr $ do 
        metaplus5 <- setup_md2metaplus fnmd1 

        (metap1,_) <- docrep2panrep NoticeLevel0 def zero metaplus5
        html1 <- panrep2htmlForTest NoticeLevel2  zero metap1
        putIOwords ["test_toHtmlTest pr \n", showPretty html1]
        let hash1 = show . hash . show $  html1 :: String
        return hash1

    assertEqual (Right "Hash {asWord64 = 1964609469859751127}") 
        res1

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
