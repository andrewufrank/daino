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
-- import Foundational.CmdLineFlags
import Uniform.Shake
-- import Development.Shake.FilePath (makeRelative, replaceDirectory)

import Uniform.Pandoc
import Text.Pandoc.Definition as Pandoc
import Foundational.SettingsPage
import Foundational.Filetypes4sites
-- import Lib.IndexCollect
-- import Wave.Md2doc
-- import Wave.Panrep2html
 
-- import Development.Shake (getDirectoryFilesIO)
import qualified Data.Map as M
import qualified Data.List as D
-- import GHC.Base (indexDoubleArrayAsDoubleX2#)



indexNeeds debug sett4 doughP bakedP outP = do 
    let debug = NoticeLevel2
    putInform debug ["rule **/*.panrep i1- outP", showT outP]
    let bakedoutP = replaceDirectoryP bakedP doughP outP 
    putInform debug ["rule **/*.panrep i1- bakedoutP", showT bakedoutP]
    -- let thisDir =   getParentDir outP  
    let bakedDirP = makeAbsDir $ getParentDir bakedoutP :: Path Abs Dir
    -- let bakedDirP = replaceDirectoryP bakedP doughP thisDirP :: Path Abs Dir 
    putInform debug ["rule **/*.panrep i1- bakedDirP", showT bakedDirP]
    -- putInform debug ["rule **/*.panrep i1- bakedDirP", showT bakedDirP]

    
--    unless ("/home/frank/bakedTestSite" == thisDir1) $ do 
    -- the case of the webroot is dealt with already  
    -- during initialization

    -- putInform debug ["rule **/*.panrep - thisDir", showT thisDir]

    fs2nake :: [Path Rel File] <- getDirectoryFilesP bakedDirP ["*.md"]
    -- all md files, without dir
    putInform debug ["\nrule **/*.panrep i2 getDirectoryFiles done fs2nake"
                , showT fs2nake]
    -- remove the index.md file ! gives recursion in rules
    let fs2a =  D.delete (makeRelFile "index.md") fs2nake   
    putInform debug ["\nrule **/*.panrep i3 getDirectoryFiles done fs2a"
                , showT fs2a]
    let fs2 = map (addFileName bakedDirP)  fs2a
    putInform debug ["\nrule **/*.panrep i3 getDirectoryFiles done fs2"
                , showT fs2]
    let fs2filtered = dnbFilter sett4 fs2
    putInform debug ["\nrule **/*.panrep i3 getDirectoryFiles done fs2filtered"
                , showT fs2filtered]

                -- return fs2filtered for md files in dir 

    dr2 :: [Path Abs Dir]  <- getDirectoryDirsFullP bakedDirP 
    putInform debug ["\nrule **/*.panrep i4 getDirectoryDir done dr2"
                , showT dr2]
    let dr2filtered = dnbFilter sett4 dr2
    putInform debug ["\nrule **/*.panrep i4 getDirectoryDir   dr2filtered"
                , showT dr2filtered]

                    -- get the index file for each dir 
                    -- to check publish
    fs3 :: [Path Abs File] <- fmap concat $ mapM (\f -> getDirectoryFilesFullP f ["index.md"]) dr2
    putInform debug ["\nrule **/*.panrep i5 getDirectoryDir done fs3"
            , showT fs3]

    -- lint is triggered. perhaps should use the
    -- non traced getDirectoryFilesIO?
    -- putIOwords ["rule **/*.panrep i6 fs2", showT fs2]
    -- -- let fs2a = map (addFileName thisDirP) fs2
    putIOwords ["rule **/*.panrep i7 fs3", showT fs3]
    let fs3filtered =  (dnbFilter sett4) fs3 
    putIOwords ["rule **/*.panrep i7 fs3filtered", showT fs3filtered]


    let needsmd =  -- md files , dirs as dir/index.md
                    (fs2filtered , fs3filtered):: ([Path Abs File], [Path Abs File])
        -- -- map (replaceDirectoryP  doughP bakedP) .
        --         map (addFileName thisDirP)
        --         . map (replaceExtension' "docrep") 
        --         $  (fs2  ) 
    putIOwords ["rule **/*.panrep i8 needsmd", showT needsmd]
    return needsmd

-- replaceDirectoryP2 pref newpref old = if pref == newpref then newpref </> rem1 
--         where rem1 = stripProperPrefixP pref old

dnbFilter ::  Settings -> [Path Abs a] -> [Path Abs a]
dnbFilter sett4 dirs1 =  (filter (not . (isInfixOf' dnbString). s2t 
                . toFilePath )) dirs1
    where
            dnbString = doNotBake (siteLayout sett4) :: Text
-- for indexpage 

constructFileEnry debug sett4 mdfn  = do 
    putIOwords ["constructFileEntry 1 for mdfn", showT mdfn ]

    let layout = siteLayout sett4 
        doughP = doughDir layout -- the regular dough
        bakedP = bakedDir layout

    let docrepFn = replaceDirectoryP doughP bakedP 
                    . replaceExtension' "docrep" $ mdfn 
    putIOwords ["constructFileEntry 2 docrepFn", showT docrepFn ]

    needP [docrepFn]
    
    dr <- runErr2action $   read8 docrepFn docrepFileType
    let incld = dr /= zero 
    
    putIOwords ["constructFileEntry 3 continues docrepFn publish"
            , showT incld,  showT docrepFn ]
    if incld 
      then do 
        let ixfn1 =   removeExtension .  stripProperPrefixP bakedP $ docrepFn :: Path Rel File
            -- link1 = getParentDir ixfn 
            m = unMeta .  metap $ dr  -- must be redone for correct html/latex encoding? 
            ie0 = zero { ixfn = toFilePath ixfn1
                        -- , link = link1 
                        , textualMd= zero -- fillTextual m
                        -- the text in 
                        -- ,  abstract = getTextFromMeta5 ""  "abstract" m
                        -- , title = getTextFromMeta5 "TITLE MISSING" "title" m
                        -- , author = getTextFromMeta5 "" "author" m  
                        -- copied form panrep2html
                        }
        return . Just $ ie0 
      else return Nothing 

-- fillTextual :: Meta -> TextualIx MetaValue
-- -- | to fill the textual data from meta
-- --  this gives the pandoc internal rep 
-- fillTextual m = zero 
-- { abstract = maybe (MetaString "ABSTRACT MISSING") id $ Pandoc.lookupMeta "abstract" m
--                 --    , title = getTextFromMeta5 "TITLE MISSING" "title" m
--                 --    , author = getTextFromMeta5 "" "author" m   
--                    , content = zero }

-- convertTextualIx :: Block -> Text  
-- is block2xx 

fillTextual4MP :: DainoMetaPlus -> DainoMetaPlus 
-- | copy the values into textual 
fillTextual4MP mp = mp{extra = x2}
    where 
        m1 =  metap mp
        pan1 = zero
            { abstract = fromMaybe (MetaString "XX") $ Pandoc.lookupMeta "abstract" m1
            , title = fromMaybe (MetaString "Missing TITLE") $ Pandoc.lookupMeta "title" m1 
            , author = fromMaybe (MetaString "Missing AUTHOR") $ Pandoc.lookupMeta "author" m1 
            -- could add content here?
            }
        h1 = metaHtml  mp ::M.Map Text Text
        htm1 = zero 
            { abstract = fromMaybe ( "XX") $ M.lookup "abstract" h1
            , title = fromMaybe ( "Missing TITLE") $ M.lookup "title" h1
            , author = fromMaybe ( "Missing AUTHOR") $ M.lookup "author" h1 
            -- could add content here?
            }
        t1 = metaLatex  mp ::M.Map Text Text
        tex1 = zero 
            { abstract = fromMaybe ( "XX") $ M.lookup "abstract" t1
            , title = fromMaybe ( "Missing TITLE") $ M.lookup "title" t1
            , author = fromMaybe ( "Missing AUTHOR") $ M.lookup "author" t1 
            -- could add content here?
            }

        x1 = extra mp 
    --     pan1 = fillTextual m1 
    --     md1 = 

    -- -- md1 <- block2xx writeToMarkdown  [pan1] -- (metap metapl1)
    -- -- htm1 <- meta2xx writeHtml5String2 pan1
    -- -- lat1 <- meta2xx writeTexSnip2 pan1
        x2 = x1 { textual0pan = pan1 
                , textual0html = htm1 
                -- , textual0md = md1
                , textual0tex = tex1
                } :: DainoValues 

    -- return mp -- {extra = x2}
     

 