-----------------------------------------------------------------------------
--
-- Module      :   the main for the sgg - no UI yet
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main     where      -- must have Main (main) or Main where

import Uniform.Convenience.StartApp
import Uniform.Filenames

import Lib.Bake

programName = "SSG" :: Text
progTitle = "constructing a static site generator" :: Text

--siteDoughPath  makeRelDir "site/dough" :: Path Rel Dir
--siteBakedPath makeRelDir "site/baked" :: Path Rel Dir

post1 = makeRelFile "postwk.md" :: Path Rel File

main :: IO ()
main = startProg programName progTitle
            ( do
                 putIOwords ["do bake for ", showT post1]
                 bake -- siteDoughPath siteBakedPath
                        post1


            )

