 
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

-- | tests foundation.hs and readSettingFile
module Lib.ReadSettingFile_test where

import           Test.Framework hiding (Result)
import           Uniform.Test.TestHarness hiding (Result)
import UniformBase 
import Foundational.LayoutFlags
import Foundational.Filetypes4sites

import Uniform.Yaml

-- import           Lib.Templating  
import ShakeBake.ReadSettingFile
-- import Data.Aeson
import Uniform.Json  

programName = "ssg"

-- settingsFile3 = sourceDirTestDocs </> settingsFileName

-- (layout7, port7) <- readSettings True settingsFile 

test_settings = 
    testVar0FileIO programName  
        (sourceDirTestSite </> settingsFileName) 
        "settingsFile" 
        (fmap fst . readSettings NoticeLevel0 )  

test_checkSettings_def = do 
    res <- runErr $ do 
            (lay, por) <- readSettings NoticeLevel0 
                (sourceDirTestSite </> settingsFileName)
            return lay 
    assertEqual (Right layoutDefaults) res 

readYaml2rec :: (FromJSON a, Show a) => Path Abs File -> ErrIO a 
-- | read a yaml file into a record Value 
-- error when syntax fault
readYaml2rec fn = do 
    putIOwords [" yaml file name", showT fn ]
    -- settingsTxt <- read8 settingsfilename yamlFileType
    s0 :: Value <- readYaml2value fn 
    putIOwords ["yaml read", showPretty s0 ]

    s1  <-  fromJSONerrio  s0  -- :: Result Settings 

    putIOwords ["json parsed", showT s1 ]

    return s1
test_readSettings2 = do 
    res <- runErr $ do 
        let settingsfilename = (sourceDirTestSite) </> settingsFileName
        putIOwords [" settings file name", showT settingsfilename ]
        -- settingsTxt <- read8 settingsfilename yamlFileType
        s1 <- readYaml2rec settingsfilename 

         

        putIOwords ["json parsed", showT s1 ]

        return s1
    assertEqual (Right settings1) res
 

-- test_readSettings1 = do 
--     res <- runErr $ do 
--         let settingsfilename = (sourceDirTestSite) </> settingsFileName
--         putIOwords [" settings file name", showT settingsfilename ]
--         -- settingsTxt <- read8 settingsfilename yamlFileType
--         s0 :: Value <- readYaml2value settingsfilename 
--         putIOwords ["yaml read", showPretty s0 ]

--         s1 :: Settings <-  fromJSONerrio  s0  -- :: Result Settings 

--         putIOwords ["json parsed", showT s1 ]

--         return s1
--     assertEqual (Right settings1) res

-- as long as path and prettyprint do not parse 
-- produce error 

settings1 = zero :: Settings 
    -- Right (Settings {storage = SiteLayout {themeDir = Path Abs Dir /home/frank/Workspace11/ssg/docs/theme/, doughDir = Path Abs Dir /home/frank/Workspace11/ssg/docs/site/dough/, bakedDir = Path Abs Dir /home/frank/Workspace11/ssg/docs/site/baked/, masterTemplateFile = Path Rel File master5.dtpl}, localhostPort = 3000, settingsAuthor = "Author of Settings", settingsDate = "2019-01-01", settings = Settings2 {sitename = "siteNameExample", byline = "siteByLineExample", banner = "/templates/img/symmetricGeras2.jpg"}, menu = [MenuItem {navlink = "/Blog/index.html", navtext = "Blog"},MenuItem {navlink = "/PublicationList/index.html", navtext = "Publications"},MenuItem {navlink = "/SSGdesign/index.html", navtext = "SSG Documentation"}]})

instance ShowTestHarness (Path Abs File)
instance ShowTestHarness SiteLayout

test_EmptyTestTrue = assertEqual 0 0


