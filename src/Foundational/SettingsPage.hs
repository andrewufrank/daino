----------------------------------------------------------------------
--
-- Module      : layout and flags  takes data from the settinsgN.yaml file (metaPage deals with the md file YAML header)
----------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- | the defintion for a layout and a flags type
  which carry info from the command line and the siteHeader file
 the defaults for flags are set up for testing  are overridden
 the defaults for layout must correspond to what is set in the test siteHeader file.
 layout defaults are used in testing

 content dirs are those, which have *.md files
-}
module Foundational.SettingsPage
    (module Foundational.SettingsPage
    , def 
    , toJSON, fromJSON
    ) where

import UniformBase
import Data.Default.Class ( Default(def) ) -- to define a default class for siteLayout 
import Uniform.Json ( FromJSON, ToJSON (toJSON), fromJSON )
import Uniform.MetaPlus hiding (MetaPlus(..), Settings(..), ExtraValues(..))
import qualified Data.Map as M

import Path (parent)
progName, progTitle :: Text
progName = "daino"  
progTitle = "constructing a static site generator" :: Text

settingsFileName :: Path Rel File
-- ^ the yaml file in which the siteHeader are fixec
settingsFileName = makeRelFile "settings3" -- the yaml file

-- | the siteHeader file with all fields 
data Settings = Settings
    { siteLayout :: SiteLayout 
    , localhostPort :: Int 
    , settingsAuthor :: Text 
    , settingsDate :: Text -- should be UTC 
    , siteHeader :: SiteHeader 
    , menuitems :: MenuItems
    -- , today :: Text
    } deriving (Show, Read, Ord, Eq, Generic, Zeros)

data DainoMetaPlus = DainoMetaPlus 
                { metap :: Meta    -- ^ the pandoc meta 
                , sett :: Settings -- ^ the data from the settingsfile
                , extra :: DainoValues -- ^ other values to go into template
                , metaMarkdown :: M.Map Text Text 
                , metaHtml ::  M.Map Text Text
                , metaLatex ::  M.Map Text Text
                }
    deriving (Eq, Ord, Show, Read, Generic) -- Zeros, ToJSON, FromJSON)
instance ToJSON DainoMetaPlus
instance FromJSON DainoMetaPlus
instance Zeros DainoMetaPlus where 
        zero = DainoMetaPlus zero zero zero zero zero zero

instance ToJSON Settings
instance FromJSON Settings

-- the extraValues will eventually go into settings
data DainoValues = DainoValues 
                        { mdFile:: Text
                        , dainoVersion :: Text 
                        , authorReduced :: Text
                        -- , extraBakedDir :: Text
                        }
    deriving (Eq, Ord, Show, Read, Generic)
    
instance ToJSON DainoValues 
instance FromJSON DainoValues 

{-
example1 :: MetaPlus 
example1 = DainoMetaPlus {
    metap = 
        Meta {unMeta = fromList [(
            "Bibliography",MetaString "resources/BibTexLatex.bib"),(
            "abstract",MetaInlines [Str "ix-Abstract",Space,Emph [Str "of"],Space,Str "Book"]),(
            "bibliography",MetaInlines [Str "resources/BibTexLatex.bib"]),("body",MetaBlocks [Header 1 ("ix-hl1-title",[],[]) [Str "ix-hl1",Space,Str "Title"],Para [Str "ix-Introductory",Space,Str "text",Space,Str "for",Space,Str "book."]]),("book",MetaInlines [Str "booklet"]),(
            "data",MetaString "2000-01-01 00:00:00 UTC"),("
            date",MetaInlines [Str "2023-03-31"]),(
            "headerShift",MetaString "1"),(
            "keywords",MetaInlines [Str "exampleKeyword",Space,Str "exampleKeyword2"]),(
            "lang",MetaString "en"),(
            "latLanguage",MetaString "english"),("styleBiber",MetaString "authoryear"),(
            "title",MetaInlines [Str "ix-Title",Space,Str "of",Space,Str "Book"]),(
            "version",MetaInlines [Str "publish"]),(
            "visibility",MetaString "public")]}, 
    
    sett = Settings {
        siteLayout = SiteLayout {
            themeDir = Path Abs Dir /home/frank/Workspace11/dainoTheme/, 
            doughDir = Path Abs Dir /home/frank/Workspace11/dainoSite/, 
            bakedDir = Path Abs Dir /home/frank/bakedTestSite/, 
            masterTemplateFile = Path Rel File htmlTufte81.dtpl, 
            texTemplateFile = Path Rel File latexTufte81.dtpl, 
            doNotBake = "DNB", 
            blogAuthorToSuppress = ["AOS","AUF","Author of Settings"], 
            defaultAuthor = "AOS", 
            replaceErlaubtFile = Path Abs File /home/frank/Workspace11/
            replaceUmlaut/nichtUmlaute.txt}, localhostPort = 3000, 
            settingsAuthor = "Author of Settings", settingsDate = "2019-01-01", 
            siteHeader = SiteHeader {sitename = "siteName3", 
            byline = "siteByLine3", 
            banner = "/resources/theme/templates/img/DSC04809.JPG", 
            bannerCaption = "Ruhiger Sommer im Garten in Geras"}, 
            menuitems = MenuItems {menuNav = [
                MenuItem {navlink = "ReadMe/index.html", navtext = "ReadMe"},
                MenuItem {navlink = "SSGdesign/index.html", navtext = "Rationale"},
                MenuItem {navlink = "Contact/index.html", navtext = "Contact"},
                MenuItem {navlink = "Blog/index.html", navtext = "Blog"},
                MenuItem {navlink = "PublicationList/index.html", navtext = "Publications"}]}}, 
    extra = DainoValues {
        mdFile = "/home/frank/Workspace11/dainoSite/ReadMe/index.md", 
        dainoVersion = "Version {versionBranch = [0,1,5,6,3], versionTags = []}"}, 
    metaMarkdown = fromList [], 
    metaHtml = fromList [("Bibliography","resources/BibTexLatex.bib"),("abstract","ix-Abstract <em>of</em> Book"),("bibliography","resources/BibTexLatex.bib"),("body","<h1 id=\"ix-hl1-title\">ix-hl1 Title</h1>\n<p>ix-Introductory text for book.</p>"),("book","booklet"),("data","2000-01-01 00:00:00 UTC"),("date","2023-03-31"),("headerShift","1"),("keywords","exampleKeyword exampleKeyword2"),("lang","en"),("latLanguage","english"),("styleBiber","authoryear"),("title","ix-Title of Book"),("version","publish"),("visibility","public")], metaLatex = fromList []}
-}

instance Zeros DainoValues where zero = DainoValues zero  zero zero

-- data SiteHeader = SiteHeader 
--     { sitename :: FilePath 
--     , byline :: Text 
--     , banner :: FilePath 
--     , bannerCaption :: Text 
--     } deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- instance ToJSON SiteHeader
-- instance FromJSON SiteHeader

-- newtype MenuItems = MenuItems {menuNav:: [MenuItem]
--                             -- , menuB:: Text
--                             } deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- instance ToJSON MenuItems 
-- instance FromJSON MenuItems 

-- data MenuItem = MenuItem  
--     { navlink :: FilePath 
--     , navtext :: Text
--     -- , navpdf :: Text  -- for the link to the pdf 
--     -- not a good idead to put here
--     } deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- instance ToJSON MenuItem
-- instance FromJSON MenuItem

data SiteLayout = SiteLayout
    { -- | the place of the  theme files (includes templates)
      themeDir :: Path Abs Dir
    , -- | where the content is originally (includes resources)
      doughDir :: Path Abs Dir
    , -- | the webroot, the dir with all the produced files
      bakedDir :: Path Abs Dir
    , masterTemplateFile :: Path Rel File  -- for html
    , texTemplateFile :: Path Rel File   -- for latex 
    , doNotBake :: Text 
    -- todo probably not used
    , blogAuthorToSuppress :: [Text]
    , defaultAuthor :: Text
    , replaceErlaubtFile :: Path Abs File
    -- the list of permitted (not to replace)
    
    -- , defaultBibliography:: Text
    -- cannot be defaulted, value must be read by pandoc 
    }
    deriving (Show, Read, Ord, Eq, Generic, Zeros)
instance ToJSON SiteLayout
instance FromJSON SiteLayout
 

sourceDirTestDocs :: Path Abs Dir
sourceDirTestDocs = makeAbsDir "/home/frank/daino/docs/"

sourceDirTestSite :: Path Abs Dir
sourceDirTestSite = sourceDirTestDocs </> (makeRelDir "site")
-- ^ the dir with the source for the test site

layoutDefaults :: Path Abs Dir -> Path Abs Dir ->  SiteLayout
-- used for finding the test cases
-- must correspond to the settings3.yaml in source code repository
-- fix this later for use in testing todo 
layoutDefaults dough4test homeDir1 =
    zero -- SiteLayout
        { doughDir = dough4test
        , bakedDir = homeDir1 </> makeRelDir "bakedTestSite" :: Path Abs Dir
        ,  themeDir = (parent (parent dough4test)) </> makeRelDir "theme"
 
        ,  masterTemplateFile = makeRelFile "master7tufte.dtpl"
        , texTemplateFile = makeRelFile "resources/theme/templates/latex7.dtpl"
        ,  doNotBake = "DNB"
        -- included in filenames (and directories) to exclude from bake process
        , blogAuthorToSuppress = []
        , defaultAuthor = "AOS"
        , replaceErlaubtFile = makeAbsFile "/home/frank/Workspace11/replaceUmlaut/nichtUmlaute.txt"
        -- , defaultBibliography = "resources/BibTexLatex.bib"
        }

-- instance Default SiteLayout where 
--         def = layoutDefaults

-- notDNB :: SiteLayout -> FilePath -> Bool 
-- notDNB siteLayout = not . isInfixOf' (t2s $ doNotPublish siteLayout)

resourcesName =  "resources"
templatesName = "templates"
themeName = "theme"

templatesDir :: SiteLayout -> Path Abs Dir
templatesDir layout = themeDir layout `addFileName` (makeRelDir templatesName)

blankAuthorName :: [Text] -> Text -> Text 
-- suppress/oppress author name, if the author name is the same as one in the first arg (AUF, Andrew U..) then set it to empty else copy 
-- goal is to avoid to have each page say the obvious "author XX"
blankAuthorName names current = 
    if current `elem` names 
        then zero 
        else current 

