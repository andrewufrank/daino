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

import Foundational.SettingsPage
import Foundational.Filetypes4sites
import Wave.Md2doc
import Wave.Docrep2panrep
import Wave.Panrep2pdf
import Uniform.Pandoc
-- import Uniform.Json
import UniformBase
import Lib.Md2doc_test
import Lib.Docrep2panrep_test
import Uniform.Http ( HTMLout (HTMLout), unHTMLout, htmloutFileType ) 
import Data.Hash
import Wave.Panrep2html
import Foundational.CmdLineFlags
-- import Lib.IndexCollect

settFn = makeAbsFile 
    "/home/frank/Workspace11/daino/settingsTest.yaml"
    -- special case?
reshtmlout = makeAbsFile "/home/frank/tests/htmlout"
-- test regular processing
test_toHtmlout = do 
    res1 <- runErr $ do 
        metaplus5 <- setup_md2metaplus settingsDainoSite fnmd2 
            -- let debug = NoticeLevel0
            -- sett3 <- readSettings debug settFn 
                    -- this is a particular settingsTest.yaml
            -- metaplus1 <- readMarkdownFile2docrep debug sett3 fn
        (metap1,_) <- docrep2panrep NoticeLevel0 
                        (def::PubFlags)  metaplus5
        (html1, _) <- panrep2html NoticeLevel0  metap1
        -- putIOwords ["test_toHtmlTest pr \n", unHTMLout html1]
        write8 reshtmlout htmloutFileType html1
        let hash1 = show . hash . show $  html1 :: String
        return hash1

    assertEqual (Right "Hash {asWord64 = 5605959440504025002}") 
        res1


testDir = makeAbsDir $
                ("/home/frank" :: FilePath)
                    </> ("tests" :: FilePath)
                    -- must correspond to what testharness is using
                    -- does not

fnmd1 = makeAbsFile "/home/frank/Workspace11/daino/tests/data/ReadMe/index.md"
reshtml = makeAbsFile"/home/frank/tests/html1"
testTemplate = makeAbsFile "/home/frank/Workspace11/daino/tests/data/metaplusHtml.dtpl"

panrep2htmlForTest :: NoticeLevel -> Path Abs File ->  Panrep -> ErrIO HTMLout
panrep2htmlForTest debug  mf metaplus4 = do
    let sett3 = sett metaplus4
    -- let mf = testTemplate
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

    -- hres <- meta2hres htmlTempl metaplus4
    when (informAll debug) $  putIOwords ["panrep2htmlForTest render html done"
        , "hres", ht1]
    -- bakeOnePanrep2html will write to disk
    return . HTMLout $ ht1

settingsFn = makeAbsFile "/home/frank/Workspace11/dainoSite/settings3.yaml"
fnmd3 = makeAbsFile "/home/frank/Workspace11/dainoSite/ReadMe/index.md"
-- test with testhtml template 
test_toHtmlTest = do  --  with data from dainoSite
    res1 <- runErr $ do 
        metaplus5 <- setup_md2metaplus settingsFn fnmd3 

        (metap1,_) <- docrep2panrep NoticeLevel0 (def::PubFlags)  metaplus5
        html1 <- panrep2htmlForTest NoticeLevel0  testTemplate metap1
        -- putIOwords ["test_toHtmlTest pr \n", unHTMLout html1]
        let hash1 = show . hash . show $  html1 :: String
        return hash1

    assertEqual (Right "Hash {asWord64 = 130111545342583451}") 
        res1

--     These are all the values for htmlTufte81.dtpl

-- lang            en
-- settingsAuthor  Author of Settings
-- date            2023-03-31
-- keywords        exampleKeyword exampleKeyword2
-- page-title  missing todo 
-- image           
-- imageCaption    
-- siteBanner      /resources/theme/templates/img/DSC04809.JPG
-- siteBannerCaption   Ruhiger Sommer im Garten in Geras
-- sitename        siteName3
-- sitebyline      siteByLine3
-- menuitems        
--                     ReadMe/index.html
--                     ReadMe
                     
--                     SSGdesign/index.html
--                     Rationale
                     
--                     Contact/index.html
--                     Contact
                     
--                     Blog/index.html
--                     Blog
                     
--                     PublicationList/index.html
--                     Publications
                    
-- title           ix-Title of Book <q>ReadMe/index.md</q>
-- subtitle         
-- author          Author of Settings
-- table of contents   
-- abstract        ix-Abstract <em>of</em> Book should have 02alltext and 03tree
-- linkpdf         
-- date   
-- content         <h1 id="ix-hl1-title">ix-hl1 Title</h1>
-- <p>ix-Introductory text for book.</p>
-- menufiles       
-- menu2subdir     

-- dainoversion    Version {versionBranch = [0,1,5,6,3], versionTags = []}
-- visibility      public
-- version         publish

-- -- not in thmlTufte

-- latLanguage     english
-- styleBiber      authoryear
-- mdFile          /home/frank/Workspace11/daino/tests/data/ReadMe/index.md
-- localhostPort   3000
-- settingsDate    2019-01-01
-- siteLayout      /home/frank/bakedTestSite/
-- blogAuthorToSuppress 
--                                         AOS
--                                         AUF
--                                         Author of Settings
                
-- indexEntryDirectories 
    
-- indexEntryFiles 
--         ----
--             ixfn        /home/frank/Workspace11/daino/tests/data/ReadMe/03tree.md
--             link        ReadMe/03tree.md
--             title       03title with ref
--             abstract    03abstract: The web pages are structured as a tree.
--             author      
--             date        2023-03-31 
--             content      
--             visibility  public 
--             version     publish 
--             sortOrder    
--         ----
--             ixfn        /home/frank/Workspace11/daino/tests/data/ReadMe/02alltxt.md
--             link        ReadMe/02alltxt.md
--             title       title02 missing
--             abstract    abstract02 missing
--             author      
--             date        2023-03-31 
--             content      
--             visibility  public 
--             version     publish 
--             sortOrder    
                            
-- masterTemplateFile  metaplusHtml.dtpl
-- texTemplateFile     latexTufte81.dtpl
-- themeDir            /home/frank/Workspace11/daino/resources/

-- Bibliography        resources/BibTexLatex.bib
                    
-- headerShitft        true
