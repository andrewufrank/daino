----------------------------------------------------------------------
--
-- Module Shake2 :
----------------------------------------------------------------------
{- the conversion starts with the root files to produce, 
    i.e. only index.md 
    This triggers the rule html -> panrep 
    and panrep2html produces the needs for *.pdf, templates, jpg and bib

    for now the css, dtpl, jpg etc. are still included
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


module ShakeBake.Shake2panrep where

import UniformBase 
import Foundational.CmdLineFlags
import Uniform.Shake
import Development.Shake.FilePath (makeRelative)

import Uniform.Pandoc
import Foundational.SettingsPage
import Foundational.Filetypes4sites
import Lib.IndexCollect
import Wave.Md2doc
 
-- import Development.Shake (getDirectoryFilesIO)
import qualified Data.Map as M

shake2panrep debug flags sett4 bakedP = 
    (toFilePath bakedP <> "**/*.panrep") %> \out -> do  -- insert pdfFIles1
         
    let layout = siteLayout sett4 
        doughP = doughDir layout -- the regular dough
        -- bakedP = bakedDir layout

    putInform debug ["rule **/*.panrep", showT out]

    let outP = makeAbsFile out :: Path Abs File
    
    let bakedFrom = replaceExtension'  "docrep" outP

    putInform debug ["rule **/*.panrep - bakedFrom", showT bakedFrom]
    needP [bakedFrom]

    putInform debug ["rule **/*.panrep continued 1", showT out]

    let thisDir1 =  getParentDir outP ::FilePath
    
    putInform debug ["rule **/*.panrep - thisDir1", showT thisDir1]

    --  here the unless insert 
                
    putInform debug ["rule **/*.panrep continued 2", showT out]

    needs2 <- runErr2action $ do
            --  bakeOneDocrep2panrep debug flags bakedFrom sett4 outP 
    --           bakeOneDocrep2panrep debug flags inputFn sett3 resfn2 = do
        when (informAll debug) $    putIOwords
            [ "-----------------"
            , "bakeOneDocrep2panrep 1 inputFn"
            , showT bakedFrom
            , showT outP
            ]
        dr1 <- read8 bakedFrom docrepFileType

        -- (p3, needsFound) <- docrep2panrep debug flags  dr1
                -- completes index and should process reps 
                -- what to do with needs?
                     -- docrep2panrep debug flags dr1 = do
        -- let debug = NoticeLevel0   -- avoid_output_fromHere_down
        when (informAll debug) $
            putIOwords
                ["-------------------------docrep2panrep 1"
                --  , "metaplus: \n", showPretty dr1
                    -- , "\np1: ", showT p1
                    ]
  
        let -- sett4 = sett dr1
            -- layout = siteLayout sett4
            meta5 = metap  dr1 -- ~ panyam 
            extra6 = metaSetBook sett4 dr1 

        htm1 <- meta2xx writeHtml5String2 meta5
        tex1  :: M.Map Text Text <- meta2xx   writeTexSnip2 meta5

        let dr2 = dr1{metaHtml = htm1
                        ,metaLatex = tex1
                        , extra = extra6 }
    
        -- needs to read the docrep files

        write8 outP panrepFileType dr2 -- content is html style
        putInform NoticeLevel1 
                ["\n------bakeOneDocrep2panrep done produced resf2n", showT outP
                    -- , "\n needsFound", showT needsFound
                ]
        return [] --  needsFound

    putInform debug ["rule **/*.panrep continued 3 end", showT out]

-- for unless
   -- unless ("/home/frank/bakedTestSite" == thisDir1) $ do 
    --     -- the case of the webroot is dealt with already  
    --     -- during initialization
    --     let thisDir2 = makeAbsDir $ getParentDir outP :: Path Abs Dir
    --         thisDir = replaceDirectoryP bakedP doughP  thisDir2
    --     putInform debug ["rule **/*.panrep - thisDir2", showT thisDir2]
    --     putInform debug ["rule **/*.panrep - thisDir", showT thisDir]

    --     fs2 :: [Path Rel File] <- getDirectoryFilesP thisDir ["*.md"]
    --     dr2 :: [Path Rel File]  <- getDirectoryFilesP thisDir ["index.md"]
    --     let dr3 = dr2 
    --     -- filter (not . (isInfixOf' 
    --             -- (t2s $ doNotBake (siteLayout sett4))) . getNakedDir) dr2
    --             -- problem with get nacked dir 
    --             -- not relevant, the webroot is not serched
    --     -- lint is triggered. perhaps should use the
    --     -- non traced getDirectoryFilesIO?
    --     putIOwords ["rule **/*.panrep fs2", showT fs2]
    --     putIOwords ["rule **/*.panrep dr2", showT dr2]
    --     putIOwords ["rule **/*.panrep dr3", showT dr3]

    --     -- needs for the docrep but 
    --     let needsmd = -- map (replaceDirectoryP bakedP doughP) .
    --                 map (addFileName thisDir)
    --                 . map (replaceExtension' "docrep") 
    --                 $  (fs2 ++ dr3)
    --     putIOwords ["rule **/*.panrep needsmd", showT needsmd]
    --     needP needsmd

-- for indexpage 

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