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

import Foundational.SettingsPage
import Foundational.Filetypes4sites
import UniformBase 
import Uniform.Shake
import Uniform.Pandoc

import qualified Data.Map as M
import qualified Data.List as D



indexNeeds ::  NoticeLevel -> Settings -> Path Abs Dir -> Path Abs Dir -> Path Abs File -> Action ([Path Abs File], [Path Abs File])
indexNeeds debug sett4 doughP bakedP outP = do 
    -- let debug = NoticeLevel2
    putInformOne debug ["rule **/*.panrep i1- outP", showT outP]
    let bakedoutP = replaceDirectoryP bakedP doughP outP 
    putInformOne debug ["rule **/*.panrep i1- bakedoutP", showT bakedoutP]
    -- let thisDir =   getParentDir outP  
    let bakedDirP = makeAbsDir $ getParentDir bakedoutP :: Path Abs Dir
    -- let bakedDirP = replaceDirectoryP bakedP doughP thisDirP :: Path Abs Dir 
    putInformOne debug ["rule **/*.panrep i1- bakedDirP", showT bakedDirP]
    -- putInformOne debug ["rule **/*.panrep i1- bakedDirP", showT bakedDirP]

    
--    unless ("/home/frank/bakedTestSite" == thisDir1) $ do 
    -- the case of the webroot is dealt with already  
    -- during initialization

    -- putInformOne debug ["rule **/*.panrep - thisDir", showT thisDir]

    fs2nake :: [Path Rel File] <- getDirectoryFilesP bakedDirP ["*.md"]
    -- all md files, without dir
    putInformOne debug ["\nrule **/*.panrep i2 getDirectoryFiles done fs2nake"
                , showT fs2nake]
    -- remove the index.md file ! gives recursion in rules
    let fs2a =  D.delete (makeRelFile "index.md") fs2nake   
    putInformOne debug ["\nrule **/*.panrep i3 getDirectoryFiles done fs2a"
                , showT fs2a]
    let fs2 = map (addFileName bakedDirP)  fs2a
    putInformOne debug ["\nrule **/*.panrep i3 getDirectoryFiles done fs2"
                , showT fs2]
    let fs2filtered = dnbFilter sett4 fs2
    putInformOne debug ["\nrule **/*.panrep i3 getDirectoryFiles done fs2filtered"
                , showT fs2filtered]

                -- return fs2filtered for md files in dir 

    dr2 :: [Path Abs Dir]  <- getDirectoryDirsFullP bakedDirP 
    putInformOne debug ["\nrule **/*.panrep i4 getDirectoryDir done dr2"
                , showT dr2]
    let dr2filtered = dnbFilter sett4 dr2
    putInformOne debug ["\nrule **/*.panrep i4 getDirectoryDir   dr2filtered"
                , showT dr2filtered]

                    -- get the index file for each dir 
                    -- to check publish
    fs3 :: [Path Abs File] <- fmap concat $ mapM (\f -> getDirectoryFilesFullP f ["index.md"]) dr2
    putInformOne debug ["\nrule **/*.panrep i5 getDirectoryDir done fs3"
            , showT fs3]

    -- lint is triggered. perhaps should use the
    -- non traced getDirectoryFilesIO?
    -- putIOwords ["rule **/*.panrep i6 fs2", showT fs2]
    -- -- let fs2a = map (addFileName thisDirP) fs2
    putInformOne debug ["rule **/*.panrep i7 fs3", showT fs3]
    let fs3filtered =  (dnbFilter sett4) fs3 
    putInformOne debug ["rule **/*.panrep i7 fs3filtered", showT fs3filtered]


    let needsmd =  -- md files , dirs as dir/index.md
                    (fs2filtered , fs3filtered):: ([Path Abs File], [Path Abs File])
        -- -- map (replaceDirectoryP  doughP bakedP) .
        --         map (addFileName thisDirP)
        --         . map (replaceExtension' "docrep") 
        --         $  (fs2  ) 
    putInformOne debug ["rule **/*.panrep i8 needsmd", showT needsmd]
    return needsmd

-- replaceDirectoryP2 pref newpref old = if pref == newpref then newpref </> rem1 
--         where rem1 = stripProperPrefixP pref old

dnbFilter ::  Settings -> [Path Abs a] -> [Path Abs a]
dnbFilter sett4 dirs1 =  (filter (not . (isInfixOf' dnbString). s2t 
                . toFilePath )) dirs1
    where
            dnbString = doNotBake (siteLayout sett4) :: Text
-- for indexpage 

constructFileEnry :: NoticeLevel -> Settings -> Path Abs File -> Action (Maybe IndexEntry2)
constructFileEnry debug sett4 mdfn  = do 
    putInformOne debug ["constructFileEntry 1 for mdfn", showT mdfn ]

    let layout = siteLayout sett4 
        doughP = doughDir layout -- the regular dough
        bakedP = bakedDir layout

    let docrepFn = replaceDirectoryP doughP bakedP 
                    . replaceExtension' "docrep" $ mdfn 
    putInformOne debug ["constructFileEntry 2 docrepFn", showT docrepFn ]

    needP [docrepFn]
    
    dr <- runErr2action $   read8 docrepFn docrepFileType
    let incld = dr /= zero 
    
    putInformOne debug ["constructFileEntry 3 continues docrepFn publish"
            , showT incld,  showT docrepFn ]
    if incld 
      then do 
        let ixfn1 =   removeExtension .  stripProperPrefixP bakedP $ docrepFn :: Path Rel File
            m =   metap $ dr 
            x = extra dr
            pdfFn = replaceExtension2 ".pdf" docrepFn 
 
            ie0 = zero { ixfn = toFilePath ixfn1
                    , date = getTextFromMeta5 "2000-01-01" "date" m
                    , sortOrder = getTextFromMeta5 "filename" "sortOrder" m
                    , version = getTextFromMeta5 "draft" "version" m
                    , visibility = getTextFromMeta5 "private" "visibility" m
                    , pdf1 = s2t $ toFilePath pdfFn
                    , textualHtml = textual0html x
                    , textualTex = textual0tex x 
                    }
        return . Just $ ie0 
      else return Nothing 



fillTextual4MP :: DainoMetaPlus -> DainoMetaPlus 
-- | copy the values into textual during the reading of md files
--      into the docrep
-- the defaults are set before with metaDefaults
fillTextual4MP mp = mp{extra = x2}
    where 
        -- m1 =  metap mp
        -- pan1 = (zero :: TextualIx MetaValue)
        --     { abstract = fromMaybe (MetaString "XX") $ Pandoc.lookupMeta "abstract" m1
        --     , title = fromMaybe (MetaString "Missing TITLE") $ Pandoc.lookupMeta "title" m1 
        --     , author = fromMaybe (MetaString "Missing AUTHOR") $ Pandoc.lookupMeta "author" m1 
        --     -- could add content here?
        --     }
        h1 = metaHtml  mp ::M.Map Text Text
        htm1 = (zero :: TextualIx Text)  
            { abstract = getTextFromMap ( "XX")   "abstract" h1
            , title = getTextFromMap ( "Missing TITLE")   "title" h1
            , author = getTextFromMap ( "Missing AUTHOR")  "author" h1 
            -- -- for html  content not used
            }
        t1 = metaLatex  mp ::M.Map Text Text
        tex1 = (zero :: TextualIx Text) 
            { abstract = getTextFromMap ( "Missing ABSTRACT")  "abstract" t1
            , title = removeChar '\n' $ 
                    getTextFromMap ( "Missing TITLE")   "title" t1
            , author = getTextFromMap ( "Missing AUTHOR") "author" t1 
            -- , content = fromMaybe ("Missing CONTENT") $ M.lookup "bodyBase" t1
            , content = getTextFromMap ("Missing CONTENT") "bodyBase" t1
            -- could add content here?
            -- for html not used
            }

        x1 = extra mp 
        x2 = x1 { 
            -- textual0pan = pan1 
                 textual0html = htm1 
                , textual0tex = tex1
                } :: DainoValues 

        -- latexTitle =   fromMaybe ( "Missing TITLE") $ M.lookup "title" t1
        -- latexTitle2 =  removeChar' '\n' latexTitle

getTextFromMap :: Text -> Text -> M.Map Text Text ->  Text
--get the Metavalue (with  default)
getTextFromMap def1 key m =
       fromMaybe def1
        $ M.lookup key m
 