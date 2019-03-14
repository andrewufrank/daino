
------------------------------------------------------------------------------
--
-- Module      :  check all inputs
--

-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lib.CheckInput
     where

import Uniform.Strings hiding ((</>))
import Uniform.Filenames
-- import Uniform.FileStrings
import Lib.Foundation 
import Lib.YamlBlocks (Value, readMd2meta, getMaybeStringAtKey)
import Lib.Indexing 

import Text.Pandoc 


checkAllInputs :: SiteLayout -> [Path Rel File] -> ErrIO () 
-- ^ check the input files for syntax errors 
checkAllInputs layout mdfiles = do 
     putIOwords ["checkAllInput start", showT layout]
     val <- mapM (checkOneMdFile (doughDir layout)) mdfiles 
     
     let res = showT val 
     putIOwords ["checkAllInput end", showT res]
     return () 

checkOneMdFile :: Path Abs Dir -> Path Rel File -> ErrIO Value 
-- check one input file, return ?
checkOneMdFile dough2 mdfn = do 
     putIOwords ["checkOneMdFile start", showT mdfn]
     (_, meta2) :: (Pandoc,Value) <- readMd2meta (dough2 </> mdfn)
     ixEntry <- getOneIndexEntry dough2 (dough2 </> mdfn)
     -- what needs to be checked ? 

     let doindex1 =  getMaybeStringAtKey meta2 "indexPage" :: Maybe Text
     let doindex = maybe False ("True"==) doindex1
     -- why does it work in indexing and here no instance AtKey  Bool
     -- if doindex then 


     putIOwords ["checkOneMdFile end", showT meta2]
     return meta2
    

