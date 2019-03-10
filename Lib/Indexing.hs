
------------------------------------------------------------------------------
--
-- Module      :   create an index for a directory
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
--{-# LANGUAGE DuplicateRecordFields #-}

module Lib.Indexing
     where

import Uniform.Strings hiding ((<.>), (</>))
import Uniform.FileIO hiding ((<.>), (</>))
import qualified Uniform.FileIO as FileIO
--import Uniform.FileStrings
--import Uniform.TypedFile

import Data.Aeson as A
import Data.Yaml  as Y
--import Lib.FileMgt
import Development.Shake.FilePath
import Lib.FileMgt (DocValue (..)) --  MarkdownText (..), markdownFileType)
import Lib.YamlBlocks

--insertIndex :: Path Abs File -> ErrIO ()
---- insert the index into the index md

makeIndex :: Bool -> DocValue -> Path Abs File -> Path Abs Dir -> ErrIO MenuEntry
-- | make the index text, will be moved into the page template later
-- return zero if not index page
makeIndex debug docval pageFn dough2= do
        let doindex = fromMaybe False $ getMaybeStringAtKey docval "indexPage"
        when debug $ putIOwords ["makeIndex", "doindex", showT doindex]

        ix :: MenuEntry <- if doindex
            then do
                    let currentDir2 = makeAbsDir $ getParentDir pageFn
                    ix2 <- makeIndexForDir debug currentDir2 pageFn dough2
                    when debug $ putIOwords ["makeIndex", "index", showT ix2]
                    return ix2

          else return zero
        return ix -- (toJSON ix :: Value)

findDirs  :: [FilePath] -> ErrIO [Path Abs Dir]
-- ^ find directories in a list of files
findDirs fns = do
    mdirs <- mapM isDir fns
    return . catMaybes $ mdirs

isDir :: FilePath -> ErrIO (Maybe (Path Abs Dir))
isDir fn = do
    st <- getFileStatus'  fn
    return (if isDirectory st then Just (makeAbsDir fn) else Nothing)

makeIndexForDir :: Bool -> Path Abs Dir -> Path Abs File -> Path Abs Dir-> ErrIO MenuEntry
-- make the index for the directory
-- place result in index.html in this directory
-- the name of the index file is passed to exclude it
-- makes index only for md files in dough
-- and for subdirs, where the index must be called index.md

makeIndexForDir debug pageFn indexFn dough2 = do
    let parentDir = makeAbsDir . getParentDir . toFilePath $ pageFn :: Path Abs Dir
    let relDirPath = fromJustNote "makeIndexForDir prefix dwerwd"
                                $  stripPrefix dough2 parentDir :: Path Rel Dir
    -- this is the addition for the links
    putIOwords ["makeIndexForDir", "for ", showT pageFn, "\n relative root", showT relDirPath ]

    fs <- getDirContentNonHidden (toFilePath pageFn)
    let fs2 = filter (/= (toFilePath indexFn)) fs -- exclude index
    let fs3 = filter (FileIO.hasExtension ( "md")) fs2

    when debug $ putIOwords ["makeIndexForDir", "for ", showT pageFn, "\n", showT fs3 ]
    is :: [IndexEntry] <- mapM (\f -> getOneIndexEntry dough2 (makeAbsFile f)) fs3

    -- find directories
    dirs <- findDirs fs
    putIOwords ["makeIndexForDir", "dirs ", showT pageFn, "\n", showT dirs]

    let dis  = map  oneDirIndexEntry dirs :: [IndexEntry]
    -- needed filename.html title abstract author data
    putIOwords ["makeIndexForDir", "index for dirs  ", showT pageFn, "\n", showT dis]

    let menu1 = MenuEntry {menu2 = dis ++ is}
    when debug $ putIOwords ["makeIndexForDir", "for ", showT pageFn, "\n", showT menu1 ]
    let yaml1 = bb2t .   Y.encode  $ menu1
    when debug $ putIOwords ["makeIndexForDir", "yaml ", yaml1  ]

    return menu1

oneDirIndexEntry :: Path Abs Dir -> IndexEntry
-- make an entry for a subdir
oneDirIndexEntry dn = zero {text = showT dn
                , link = s2t $ nakedName </> "index.html"
                , title = printable <> "(subdirectory)" }
     where
        nakedName = getNakedDir . toFilePath $ dn :: FilePath
        printable = s2t $ nakedName

getOneIndexEntry :: Path Abs Dir -> Path Abs File ->  ErrIO (IndexEntry)
-- fill one entry from one mdfile file
getOneIndexEntry dough2 mdfile  = do
    (_, meta2) <- readMd2meta mdfile
--        mdtext :: MarkdownText <- read8 mdfile markdownFileType
--        pandoc <- readMarkdown2 mdtext
--        let meta2 = flattenMeta (getMeta pandoc)

    let abstract1 = getMaybeStringAtKey meta2 "abstract" :: Maybe Text
    let title1 = getMaybeStringAtKey meta2 "title" :: Maybe Text
    let author1 = getMaybeStringAtKey meta2 "author" :: Maybe Text
    let date1 = getMaybeStringAtKey meta2 "date" :: Maybe Text
    let publish1 = getMaybeStringAtKey meta2 "publish" :: Maybe Text

    let ix2 = A.fromJSON meta2 :: Result IndexEntry

    putIOwords ["getONeIndexEntry", "decoded", showT ix2]

    let parentDir = makeAbsDir . getParentDir . toFilePath $ mdfile :: Path Abs Dir
    let relDirPath = fromJustNote "makeIndexForDir prefix dwerwd"
                                $  stripPrefix dough2 parentDir :: Path Rel Dir
    let paths = reverse $ splitPath (toFilePath mdfile)
    let fn = head paths
    let dir = toFilePath relDirPath -- head . tail $ paths
    let fnn = takeBaseName fn
    let ln = s2t $ "/" <> dir </> fnn  <.> "html"

    putIOwords ["getONeIndexEntry", "dir", showT mdfile, "link", ln]

    let ix = IndexEntry {text =  s2t fnn
                    , link = ln
                    , abstract =  maybe "" id abstract1
                    , title = maybe ln id title1
                    , author = maybe "" id author1
                    , date = maybe "" id date1
                    , publish =  shownice $ maybe PSpublish text2publish publish1
                            -- default is publish
                    }
    return ix

text2publish :: Text -> PublicationState
-- convert a text to a publicationstate
text2publish  tt =   case tt of
                                "True" -> PSpublish
                                "Draft" -> PSdraft
                                "Old" -> PSold
                                _ -> PSzero


data MenuEntry = MenuEntry {menu2 :: [IndexEntry]} deriving (Generic, Eq, Ord, Show)
instance Zeros MenuEntry where zero = MenuEntry zero
instance FromJSON MenuEntry
instance ToJSON MenuEntry

data IndexEntry = IndexEntry {text :: Maybe Text  -- ^ naked filename -- not shown
                              , link :: Text -- ^ the url relative to dough dir
                              , title :: Text -- ^ the title as shown
                              , abstract :: Text
                              , author :: Text
                              , date :: Text -- ^ data in the JJJJ-MM-DD format
                              , publish :: Text

                              } deriving (Generic, Eq, Ord, Show)
instance Zeros IndexEntry where zero = IndexEntry zero zero zero zero zero zero zero
--instance FromJSON IndexEntry
instance ToJSON IndexEntry
instance FromJSON IndexEntry where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields  = True }

data PublicationState = PSpublish | PSdraft | PSold | PSzero
        deriving (Generic,  Show, Read, Ord, Eq)
-- ^ is this file ready to publish
instance Zeros PublicationState where zero = PSzero
instance NiceStrings PublicationState where shownice = drop' 2 . showT

instance ToJSON PublicationState
instance FromJSON PublicationState


--makeIndexEntry :: Path Abs File -> ErrIO IndexEntry
--makeIndexEntry fp = do
--    let paths = reverse $ splitPath (toFilePath fp)
--    let fn = head paths
--    let dir = head . tail $ paths
--    let fnn = takeBaseName fn
--
--
--
--    return $ IndexEntry {text = s2t fnn, link = s2t $ "/" <> dir </> fnn <.> "html"}


--getDirContentNonHiddenFiles :: FileOps fp => fp -> ErrIO [fp]
--getDirContentNonHiddenFiles fp = do
--    fps  <- getDirContentNonHidden fp
--    filterM doesFileExist' fps

