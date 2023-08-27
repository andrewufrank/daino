----------------------------------------------------------------------
--
-- Module Shake2 :
----------------------------------------------------------------------
{- construct the two indexEntries 
    -}
----------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans
            -fno-warn-missing-signatures
            -fno-warn-missing-methods
            -fno-warn-duplicate-exports  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use ++" #-}


module ShakeBake.Shake2indexes where

import UniformBase 
import Foundational.CmdLineFlags
import Uniform.Shake
import Development.Shake.FilePath (makeRelative, replaceDirectory)

import Uniform.Pandoc
import Foundational.SettingsPage
import Foundational.Filetypes4sites
import Lib.IndexCollect
import Wave.Md2doc
 
-- import Development.Shake (getDirectoryFilesIO)
import qualified Data.Map as M
-- import GHC.Base (indexDoubleArrayAsDoubleX2#)



indexNeeds debug doughP bakedP outP = do 
    let debug = NoticeLevel2
    let thisDir =   getParentDir outP  
    let thisDirP = makeAbsDir $ getParentDir outP :: Path Abs Dir
    let bakedDirP = replaceDirectoryP bakedP doughP thisDirP :: Path Abs Dir 
    putInform debug ["rule **/*.panrep i1- thisDir2", showT thisDirP]
    putInform debug ["rule **/*.panrep i1- bakedDirP", showT bakedDirP]

    
--    unless ("/home/frank/bakedTestSite" == thisDir1) $ do 
    -- the case of the webroot is dealt with already  
    -- during initialization

    -- putInform debug ["rule **/*.panrep - thisDir", showT thisDir]

    fs2 :: [Path Abs File] <- getDirectoryFilesFullP bakedDirP ["*.md"]
    putInform debug ["\nrule **/*.panrep i1 getDirectoryFiles done fs"
                , showT fs2]
    -- let fs2compl = map (addFileName bakedDirP) fs2 :: [Path Abs File]

    dr2 :: [Path Abs Dir]  <- getDirectoryDirsFullP bakedDirP 
    putInform debug ["\nrule **/*.panrep i1 getDirectoryDir done dr2"
                , showT dr2]
    -- let dr2compl = map (addDir bakedDirP) dr2   :: [Path Abs Dir]
    -- putInform debug ["\nrule **/*.panrep i1 getDirectoryDir done dr2compl"
            -- , showT dr2compl]

    fs3 :: [Path Abs File] <- fmap concat $ mapM (\f -> getDirectoryFilesFullP f ["index.md"]) dr2
    putInform debug ["\nrule **/*.panrep i1 getDirectoryDir done fs3"
            , showT fs3]
    -- let fs3compl = zipWith (addDir ) dr2compl fs3 :: [Path Abs File]
    -- filter (not . (isInfixOf' 
            -- (t2s $ doNotBake (siteLayout sett4))) . getNakedDir) dr2
            -- problem with get nacked dir 
            -- not relevant, the webroot is not serched
    -- lint is triggered. perhaps should use the
    -- non traced getDirectoryFilesIO?
    putIOwords ["rule **/*.panrep i2 fs2", showT fs2]
    -- let fs2a = map (addFileName thisDirP) fs2
    putIOwords ["rule **/*.panrep i2 fs3", showT fs3]

    -- putIOwords ["rule **/*.panrep i3 dr2", showT dr2]
    -- putIOwords ["rule **/*.panrep i4 dr3", showT dr3]

    
    -- needs for the docrep but 
    let needsmd = -- map (replaceDirectoryP doughP bakedP ) 
                    (fs2 ++  fs3):: [Path Abs File]
        -- -- map (replaceDirectoryP  doughP bakedP) .
        --         map (addFileName thisDirP)
        --         . map (replaceExtension' "docrep") 
        --         $  (fs2  ) 
                -- todo not used dirs 
    putIOwords ["rule **/*.panrep i5 needsmd", showT needsmd]
    return needsmd
 
-- for indexpage 
-- constructIndexPages outP = do 

        -- (p3, needsFound) <- if isIndexPage mdFile5
        --     then do
        --         putIOwords ["\n ix2-2-----------------------docrep2panrep before collectIndex"]
        --         -- extra7 <- collectIndex NoticeLevel0 flags sett4 mdFileDir extra6

        --                   -- collectIndex debug pubf sett4 fn dv1 = do
        --         putInform debug ["collectIndex 1", "start", showPretty mdFileDir]

        --         let -- layout = siteLayout sett4 
        --             -- doughP = doughDir layout -- the regular dough
        --             -- bakedP = bakedDir layout

        --         -- (dirs, files) :: ([Path Abs Dir], [Path Abs File]) <- getDirContent2dirs_files NoticeLevel0 flags sett4 doughP  mdFileDir

        --         -- getDirContent2dirs_files debug flags sett4 doughP   indexDir = do
        --         putInform debug ["getDirContent2dirs_files for", showPretty mdFileDir]
        --         -- let pageFn = makeAbsDir $ getParentDir indexpageFn :: Path Abs Dir
        --         -- -- get the dir in which the index file is embedded
        --         -- putInform debug ["getDirContent2dirs_files pageFn", showPretty pageFn]

        --         dirs1 :: [Path Abs Dir] <- getDirectoryDirs' mdFileDir
        --         let dirs2 =  filter (not . isInfixOf' (doNotBake (siteLayout sett4)). s2t . getNakedDir) dirs1
        --         let dirs3 = filter ( not . (isPrefixOf' resourcesName) .  getNakedDir) dirs2
        --         let dirs4 = filter ( not . (isPrefixOf'  templatesName) . getNakedDir) dirs3
        --         let dirs5 = filter ( not . (isPrefixOf' "." ) .   getNakedDir) dirs4
        --         -- TODO may need extension (change to list of excluded)
        --         -- build from constants in foundation

        --         putInform debug ["\ngetDirContent2dirs_files dirs4", showPretty dirs5]
        --         let ixdirs = dirs5

        --         files1 :: [Path Abs File] <- getDirContentFiles mdFileDir

        --         putInform debug ["getDirContent2dirs_files files1", showPretty files1]
            
        --         let files2 =
        --                 filter (("index" /=) . getNakedFileName) -- should not exclude all index pages but has only this one in this dir?
        --                     . filter (hasExtension extMD)  
        --                     . filter (not . isInfixOf' (doNotBake (siteLayout sett4)) . s2t. toFilePath)
        --                     $ files1
        --         putInform debug ["getDirContent2dirs files2", showPretty files2]
        --         let ixfiles = files2

        --         putInform debug ["getDirContent2dirs xfiles", showPretty ixfiles, "\n ixdirs", showPretty ixdirs]

        --         let (dirs, files) =  ( ixdirs,  ixfiles)



        --         putInform NoticeLevel2 ["collectIndex 2 dough!", "\n dirs"
        --                         , showT dirs, "\n files", showT files]

        --         -- check for publishable: test docrep not zero 
        --         -- put needs 
        --         -- change to search in bakedP 

        --         let files11 = map (replaceDirectoryP doughP bakedP) files
        --         let dirs21 = dirs -- catMaybes $ map check2publishDirs dirs 
        --         -- files2m <- mapM check2publishFiles files1
        --         files2m <- mapM (\fn3 -> do 
        --                                     f <- read8 fn3 docrepFileType
        --                                     return $ if f /= zero then Just fn3 else Nothing) files11 

        --         let files22 =  catMaybes files2m  

        --         -- let    mdfs = mdFiles flags :: [Path Abs File]
        --         -- let dirs2 = catMaybes (map (check2publishDirs doughP mdfs) dirs)
        --         -- let files2 = catMaybes (map (check2publishFiles doughP mdfs) files)
        --             -- map ((\fp -> addFileName fp (makeRelFile "index"))  . 
        --                     --   removeExtension . makeRelativeP doughP)   dirs 
        --                             -- :: [Path Rel File]
        --         let dv2 = extra6{dirEntries = map (initializeIx2dir (mdFiles flags) doughP) dirs21
        --                         , fileEntries = map (initializeIx2file (mdFiles flags) doughP) files22}

        --         putInform debug ["collectIndex 3"
        --             , "\ndv2 dirs ixfn", showT (map ixfn . dirEntries $ dv2)
        --             , "\ndv2 dirs link", showT (map link . dirEntries $ dv2)
        --             , "\ndv2 files ixfn", showT (map ixfn . fileEntries $ dv2)
        --             , "\ndv2 files link", showT (map link . fileEntries $ dv2)
        --             ]
        --         let extra7 = dv2


        --         -- when (inform debug) $
        --         putIOwords ["\n ix2-2-----------------------docrep2panrep after collectIndex"]
        --         --     , showPretty extra7 
        --         --     ]
        --         -- attention the dir/index is one level deeper than the files
        --         let
        --             ns  =  map (<.> ("docrep" :: FilePath ) ) ns2
        --             ns2 = map ((toFilePath bakedP) </>) . map (makeRelative (toFilePath doughP)) $ ns0
        --                         :: [FilePath]
        --             ds =  map ixfn (dirEntries  extra7) :: [FilePath]
        --             fs =   map ixfn (fileEntries extra7) :: [FilePath]
        --             ns0 = ds ++ fs 
        --             -- ixs =  map addIndex (dirEntries  extra7) ++ (fileEntries extra7)
        --             needs :: [FilePath] =   ns
        --         -- putIOwords ["\tds", showT ds]
        --         -- putIOwords ["\tfs", showT fs]

        --         -- putIOwords ["\tns", showT ns]
        --         -- putIOwords ["\tns2", showT ns2]
        --         -- putIOwords ["\tns0", showT ns0]
        --         -- when (inform debug) $
        --         --     putIOwords ["\n extra7------------------------docrep2panrep end if"
        --         --     , showPretty extra7
        --         --     , "needs ns with index.docrep", showT needs ]  

        --         return (metaplus6{extra=extra7}, needs )
        --     else
        --         return (metaplus6 , []) 
