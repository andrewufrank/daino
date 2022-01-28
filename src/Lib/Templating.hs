----------------------------------------------------------------------
--
-- Module      :   applying a template (using pandoc)
--
----------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Lib.Templating where -- (openMain, htf_thisModuelsTests)

import Uniform.Http -- ( HTMLout(HTMLout) )

import Uniform.Json ( Value, ErrIO )
import Uniform.PandocHTMLwriter ( applyTemplate4 )
import UniformBase

putValinMaster :: NoticeLevel -> [Value] -> Path Abs File -> ErrIO HTMLout
{- ^ get the master html template and put the val into it
 takes the master filename from val
 not clear what intended
 for now: use the master TODO
-}
putValinMaster debug vals masterfn = do
    when (inform debug) $ putIOwords ["putValinMaster", "masterfn", showT masterfn]

    template2 :: Text <- readFile2 (toFilePath masterfn)

    -- templatapplyTemplate3 debug masterfn vals -- inTemplate.html
    html2 <- applyTemplate4 (inform debug) template2 vals  
    return . HTMLout $ html2

{- list of variables potentially used by Master5.dtpl:
css - name of stylesheet
date 
keywords  (list)
page-title 
page-title-isPostfix 
include-before 
settings
    sitename 
    byline 
    banner 
    bannerCaption
menu 
    link 
    text 
title 
subtitle 
author 
menu2 
    link2 
    title2 
    abstract2
    author2 
    date2 
    -- publish2 
table-of-contents 
beforeContent
abstract 
contentHtml 
afterContent 
ssgversion
today
Filenames 
filename3  -- the current filename producing the page
include_after  
ssgversion
-}