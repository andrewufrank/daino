{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports #-}

module Lib.Template_test where

import Test.Framework

import Foundational.Filetypes4sites
import Foundational.SettingsPage
    -- ( SiteLayout(doughDir, bakedDir, themeDir),
    --   Settings(siteLayout) )
import Foundational.CmdLineFlags
import Wave.Md2doc
import Wave.Docrep2panrep
import ShakeBake.ReadSettingFile
import Lib.IndexCollect
import Uniform.Pandoc
-- import Data.Aeson
import Uniform.Json
-- import Uniform.Test.TestHarness
import Uniform.MetaPlus hiding (MetaPlus(..), Settings(..), ExtraValues(..))
import UniformBase
import Lib.Md2doc_test
-- test_exampleTrue = assertEqual 0 0

settFn = makeAbsFile "/home/frank/Workspace11/daino/settings3.yaml"
fnin = makeAbsFile "/home/frank/Workspace11/dainoSite/ReadMe/index.md"
htmlTest = makeAbsFile "/home/frank/Workspace11/daino/tests/data/metaplusHtml.dtpl"
-- "/home/frank/Workspace11/u4blog/uniform-pandoc/resources/metaplusHtml.dtpl"



test_templatehtml = do 
    res1 <- runErr $ do 
        let debug = NoticeLevel0
        sett3 <- readSettings debug settFn 
        metaplus1 <- readMarkdownFile2docrep debug sett3 fnin
        when (inform debug) $ 
            putIOwords ["ttesting_templatehtml metaplus1 \n"
            , showPretty metaplus1]

        -- docrep2panrep does suppress author 
        (metaplus2, needs) <- docrep2panrep debug testFlags  metaplus1
        -- from panrep2htm
        htmlTempl  <- compileTemplateFile2 htmlTest

        htm1 <- meta2xx writeHtml5String2 (metap metaplus2)
        let metaplus5 = metaplus2{metaHtml = htm1}

        when (inform debug) $ 
            putIOwords ["testing_templatehtml metaplus5 \n"
                    -- , showPretty . toJSON $ metaplus5
                    , showPretty  metaplus5
                    ]

        let hpl1 = renderTemplate htmlTempl (toJSON metaplus5)  -- :: Doc Text
        when (inform debug) $
            putIOwords ["testing_templatehtml hpl1 \n", showT hpl1]
        let ht1 = render (Just 50) hpl1  -- line length, can be Nothing
        when (inform debug) $
            putIOwords ["testing_templatehtml ht1 \n", ht1]

        return ht1

    assertEqual (Right temp_res1) res1

temp_res1 =  "\n    \n\n    These are all the values for htmlTufte81.dtpl\n\nlang            en\nsettingsAuthor  Author of Settings\ndate            2023-03-31\nkeywords        \nimage           \nimageCaption    \nsiteBanner      /resources/theme/templates/img/DSC04809.JPG\nsiteBannerCaption   Ruhiger Sommer im Garten in Geras\nsitename        \nsitebyline      siteByLine3\nmenuitems        \n                    ReadMe/index.html\n                    ReadMe\n                     \n                    SSGdesign/index.html\n                    Rationale\n                     \n                    Contact/index.html\n                    Contact\n                     \n                    Blog/index.html\n                    Blog\n                     \n                    PublicationList/index.html\n                    Publications\n                    \ntitle           ix-Title of Book\nsubtitle         \nauthor          \ntable of contents   \nabstract        ix-Abstract <em>of</em> Book\nlinkpdf         \ndate            2023-03-31\ncontent         <h1 id=\"ix-hl1-title\">ix-hl1 Title</h1>\n<p>ix-Introductory text for book.</p>\nmenufiles       \nmenu2subdir     \n\ndainoversion    Version {versionBranch = [0,1,5,6,3], versionTags = []}\nvisibility      public\nversion         publish\n\n-- not in thmlTufte\n\nlatLanguage     english\nstyleBiber      authoryear\nmdFile          /home/frank/Workspace11/dainoSite/ReadMe/index.md\nlocalhostPort   3000\nsettingsDate    2019-01-01\nsiteLayout      /home/frank/bakedTestSite/\nblogAuthorToSuppress \n                                        AOS\n                                        AUF\n                                        Author of Settings\n                \nmasterTemplateFile  htmlTufte81.dtpl\ntexTemplateFile     latexTufte81.dtpl\nthemeDir            /home/frank/Workspace11/dainoTheme/\n\nBibliography        resources/BibTexLatex.bib\n                    \nheaderShitft        true\n\n-- other settings similar"


--     These are all the values for htmlTufte81.dtpl

-- lang            en
-- settingsAuthor  Author of Settings
-- date            2023-03-31
-- keywords        
-- image           
-- imageCaption    
-- siteBanner      /resources/theme/templates/img/DSC04809.JPG
-- siteBannerCaption   Ruhiger Sommer im Garten in Geras
-- sitename        
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
                    
-- title           ix-Title of Book
-- subtitle         
-- author          
-- table of contents   
-- abstract        ix-Abstract <em>of</em> Book
-- linkpdf         
-- date            2023-03-31
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
-- mdFile          /home/frank/Workspace11/dainoSite/ReadMe/index.md
-- localhostPort   3000
-- settingsDate    2019-01-01
-- siteLayout      /home/frank/bakedTestSite/
-- blogAuthorToSuppress 
--                                         AOS
--                                         AUF
--                                         Author of Settings
                
-- masterTemplateFile  htmlTufte81.dtpl
-- texTemplateFile     latexTufte81.dtpl
-- themeDir            /home/frank/Workspace11/dainoTheme/

-- Bibliography        resources/BibTexLatex.bib
                    
-- headerShitft        true