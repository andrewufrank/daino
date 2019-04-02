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
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric     #-}

module Lib.Indexing
    ( module Lib.Indexing
    , getAtKey
    )
where

import           Uniform.Shake
import           GHC.Exts                       ( sortWith )
import           Uniform.Json
import           Uniform.Json                   ( FromJSON(..) )
import           Uniform.Pandoc                 (  getAtKey
        -- DocValue(..)
                        -- , unDocValue
                                                 
                                                )
import           Uniform.Time                   ( year2000 )
import           Lib.CmdLineArgs                ( PubFlags(..) )
import           Lib.CheckInput                 ( MetaRec(..)
                                                , PublicationState(..)
                                                , SortArgs (..)
                                                -- , readMeta2rec
                                                -- , checkOneMdFile
                                                )

makeIndex
    :: Bool
    -> PubFlags
    -> MetaRec
    -> Path Abs File
    -> Path Abs Dir
    -> ErrIO MenuEntry
-- | make the index text, will be moved into the page template later
-- return zero if not index page
makeIndex debug flags metaRec pageFn dough2 = do
    let doindex   = fromMaybe False $  indexPage metaRec
    -- let indexSort1 = indexSort metaRec :: SortArgs
    when debug $ putIOwords ["makeIndex", "doindex", showT doindex]

    ix :: MenuEntry <- if doindex
        then do
            -- let currentDir2 = makeAbsDir $ getParentDir pageFn
            ix2 <- makeIndexForDir debug
                                   flags metaRec
                                   dough2
                                   pageFn

            when debug $ putIOwords ["makeIndex", "index", showT ix2]
            return ix2
        else return zero
    return ix -- (toJSON ix :: Value)


makeIndexForDir
    :: Bool
    -> PubFlags
    -> MetaRec
    -> Path Abs Dir
    -> Path Abs File
    -> ErrIO MenuEntry
-- make the index for the directory
-- place result in index.html in this directory
-- the name of the index file is passed to exclude it
-- and from it the directory above (i.e. the one to index) is derived
-- makes index only for md files in dough
-- and for subdirs, where the index must be called index.md

makeIndexForDir debug flags metaRec dough2 indexFn  = do
    -- values title date
    let pageFn = makeAbsDir $ getParentDir indexFn :: Path Abs Dir
    let parentDir =
            makeAbsDir . getParentDir . toFilePath $ pageFn :: Path Abs Dir
    let relDirPath = parentDir
    --         fromJustNote "makeIndexForDir 1 prefix dwerwd"
    --             $ stripPrefix dough2 parentDir :: Path Rel Dir
    -- this is the addition for the links
    when debug $ putIOwords
        [ "makeIndexForDir 2"
        , "for " , showT pageFn, "index", showT indexFn
        , "\n relative root"  , showT relDirPath
        , "\n sort", showT (indexSort metaRec) , "flags", showT flags
        ]

    fs <- getDirContentNonHidden (toFilePath pageFn)
    let fs2 = filter (/= toFilePath indexFn) fs -- exclude index
    let fs3 = filter (hasExtension "md") fs2
    let fs4 = map makeAbsFile fs3
    -- fs4 are full path
    when debug $ putIOwords
        ["makeIndexForDir 3", "for ", showT pageFn, "\n", showT fs3]


    -- fileIxs :: [IndexEntry] <- mapM (\f -> getOneIndexEntry dough2 $ makeAbsFile f) fs3
    fileIxs1 :: [Maybe IndexEntry] <-
            mapM (  getOneIndexEntry debug flags  metaRec dough2  ) fs4
    let fileIxs = catMaybes fileIxs1

    let fileIxsSorted = case   (indexSort metaRec) of
            SAtitle       -> sortWith title2 fileIxs
            SAdate        -> sortWith date2 fileIxs
            SAreverseDate -> reverse $ sortWith date2 fileIxs
            SAzero ->         errorT ["makeIndexForDir fileIxsSorted", showT SAzero]

    unless (null fileIxs) $ do
        -- putIOwords
        --     [ "makeIndexForDir"
        --     , "index for dirs not sorted "
        --     , showT $ map title2 fileIxs
        --     ]
        when debug $ putIOwords
            [ "makeIndexForDir 4"
            , "index for dirs sorted "
            , showT $ map title2 fileIxsSorted
            ]

    -- directories
    dirs <- findDirs fs
    when debug $ putIOwords
        ["makeIndexForDir 5", "dirs ", showT pageFn, "\ndirIxs", showT dirs]
    let dirIxs = map oneDirIndexEntry dirs :: [IndexEntry]
    -- format the subdir entries
    -- needed filename.html title abstract author data
    when debug $ putIOwords
        [ "makeIndexForDir 6"
        , "index for dirs  "
        , showT pageFn
        , "\n"
        , showT dirIxs
        ]
    let dirIxsSorted = sortWith title2 dirIxs
    let dirIxsSorted2 = if not (null dirIxsSorted)
            then dirIxsSorted ++ [zero { title2 = "------" }]
            else []
    let menu1 = MenuEntry { menu2 = dirIxsSorted2 ++ fileIxsSorted }
    when debug $ putIOwords
        ["makeIndexForDir 7", "for ", showT pageFn, "\n", showT menu1]
    let yaml1 = encodeT menu1 -- bb2t . encode $ menu1
    when debug $ putIOwords ["makeIndexForDir 8", "yaml ", yaml1]

    return menu1

oneDirIndexEntry :: Path Abs Dir -> IndexEntry
-- format an entry for a subdir
oneDirIndexEntry dn = zero { text2  = showT dn
                           , link2  = s2t $ nakedName </> ("html" :: FilePath)
                           , title2 = printable <> " (subdirectory)"
                           }
  where
    nakedName = getNakedDir $ dn :: FilePath
                -- getNakedDir . toFilePath $ dn :: FilePath
    printable = s2t nakedName

makeRelLink :: Path Abs Dir -> Path Abs File -> ErrIO Text 
-- convert a filepath to a relative link 
makeRelLink dough2 mdfile = do 
    rel2root <- stripProperPrefix' dough2 mdfile
    let relHtml = setExtension (makeExtensionT "html") . removeExtension $ rel2root
    -- let fnn   = takeBaseName . toFilePath $ rel2root
    return $  s2t $ "/" <> toFilePath relHtml


getOneIndexEntry
    :: Bool -> PubFlags -> MetaRec-> Path Abs Dir -> Path Abs File -> ErrIO (Maybe IndexEntry)
-- fill one entry from one mdfile file
getOneIndexEntry debug flags metaRec dough2 mdfile = do
    -- (_, metaRec, report) <- checkOneMdFile dough2 dough2 mdfile
    if checkPubStateWithFlags flags (publicationState metaRec)
        then do
            when debug $ putIOwords ["getOneIndexEntry 1", showT flags
                        , showT mdfile]

            -- let parentDir =  makeAbsDir . getParentDir . toFilePath $ mdfile
            --                 :: Path Abs Dir
            -- let relDirPath =
            --         fromJustNote "makeIndexForDir prefix dwerwd"
            --             $ stripPrefix dough2 parentDir :: Path Rel Dir
            -- let paths = reverse $ splitPath (toFilePath mdfile)
            -- let fn    = head paths
            -- let dir   = toFilePath parentDir -- relDirPath -- head . tail $ paths
            -- rel2root <- stripProperPrefix' dough2 mdfile
            -- let relHtml = setExtension (makeExtensionT "html") . removeExtension $ rel2root
            -- let fnn   = takeBaseName . toFilePath $ rel2root
            -- let linkName = s2t $ "/" <> toFilePath relHtml
            linkName <- makeRelLink dough2 mdfile
            -- when debug $ putIOwords ["getOneIndexEntry 2"
            --     , "parentDir", showT parentDir
            --     , "rel2root", showT rel2root
            --     , "relHtml", showT relHtml
            --     , "fnn", showT fnn
            --     , "linkName", showT linkName
            --     ]
-- getOneIndexEntry 2
-- parentDir Path Abs Dir /home/frank/Workspace8/ssg/docs/site/dough/Blog/
-- rel2root Path Rel File Blog/postTufteStyled.md
-- relHtml Path Rel File Blog/postTufteStyled.html fnn "postTufteStyled"
-- linkName "/Blog/postTufteStyled.html"

-- should be "/Blog/postTufteStyled.html""


            let ix = IndexEntry
                    { text2     = s2t . takeBaseName . t2s $ linkName 
                    , link2     = linkName
                    , abstract2 = fromMaybe "" $ abstract metaRec
                    , title2    = fromMaybe linkName $ title metaRec
                    , author2   = fromMaybe "" $ author metaRec
                    , date2     = maybe (showT year2000) showT $ date metaRec
                    , publish2  = shownice $ publicationState metaRec
                                    -- default is publish
                    }
            when debug $ putIOwords
                [ "getOneIndexEntry 3" , "dir", showT mdfile
                , "link", linkName , "title2", showT $ title2 ix
                , "title originally", fromMaybe linkName $ title metaRec
                ]
            return . Just $ ix
        else return Nothing

checkPubStateWithFlags :: PubFlags -> Maybe PublicationState -> Bool
-- check wether the pubstate corresponds to the flag
checkPubStateWithFlags flags (Just PSpublish) = publishFlag flags
checkPubStateWithFlags flags (Just PSdraft  ) = draftFlag flags
checkPubStateWithFlags flags (Just PSold    ) = oldFlag flags
checkPubStateWithFlags _     (Just PSzero   ) = False
checkPubStateWithFlags flags     Nothing          = publishFlag flags

newtype MenuEntry = MenuEntry {menu2 :: [IndexEntry]} deriving (Generic, Eq, Ord, Show)

instance NiceStrings MenuEntry

instance Zeros MenuEntry where
    zero = MenuEntry zero
instance FromJSON MenuEntry
instance ToJSON MenuEntry

data IndexEntry = IndexEntry {text2 ::  Text  -- ^ naked filename -- not shown
                              , link2 :: Text -- ^ the url relative to dough dir
                              , title2 :: Text -- ^ the title as shown
                              , abstract2 :: Text
                              , author2 :: Text
                              , date2 :: Text -- UTCTime -- read the time early one to find errors
                              , publish2 :: Text

                              } deriving (Generic, Eq, Ord, Show, Read)

instance Zeros IndexEntry where
    zero = IndexEntry zero zero zero zero zero zero zero
--instance FromJSON IndexEntry
instance ToJSON IndexEntry
instance FromJSON IndexEntry where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }


