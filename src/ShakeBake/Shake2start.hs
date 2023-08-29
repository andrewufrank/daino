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


module ShakeBake.Shake2start where

import           Uniform.Shake
import Foundational.SettingsPage
import Foundational.CmdLineFlags
import ShakeBake.Shake2



shakeAll :: NoticeLevel -> Settings -> PubFlags -> FilePath -> ErrIO ()
-- ^ calls shake in the IO monade. this is in the ErrIO
shakeAll debug sett3 flags causedby = do
    let layout = siteLayout sett3 
        doughP = doughDir layout :: Path Abs Dir -- the regular dough
        bakedP = bakedDir layout :: Path Abs Dir
        themeP = themeDir layout :: Path Abs Dir
    putIOwords
        [ "\n\n===================================== shakeAll start"
        , "\n flags", showPretty flags
        , "\ncaused by", s2t causedby, "."
        , "\ndebug:", showT debug
        , "\ndough", showT doughP 
        , "\nbaked", showT bakedP 
        , "\ntheme", showT themeP 
        , "\ndebug", showT debug 
        , "\n======================================="
        ]

    let 
        fs4 = ["index.html"
                -- , "Minimal/index.html"
                -- , "Event/index.html"
                ]  -- start with HTML! 
        fs4htmlTemplate =   
            ["resources/theme/templates/static/tufte.css"
            , "resources/theme/templates/static/tufte-extra.css"
            , "resources/theme/templates/static/pandoc.css"
            , "resources/theme/templates/static/tufte-additions.css"
            , "resources/img/squared16.jpg" -- add rest 
            , "resources/theme/templates/img/DSC04809.JPG"
            -- banner . siteHeader $ sett3 
            ,"resources/BibTexLatex.bib"
            ]
        flags2 = flags{mdFiles = 
            concat [ map (addFileName bakedP ) $ map makeRelFile fs4htmlTemplate
                , map (addFileName bakedP) $ map makeRelFile fs4
                                    ]
        }
    putInform NoticeLevel1 ["start with fs4", showT fs4, "\n"]
    putInform debug ["mdFiles flags", showT $ mdFiles flags2]        
    callIO $ shakeMD NoticeLevel2 sett3  flags2   
