
------------------------------------------------------------------------------
--
-- Module      :  check all inputs
--

-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}

module Lib.CheckInput where

import Lib.Foundation
import Lib.Indexing
-- import Uniform.FileStrings
-- import Lib.YamlBlocks                 ( Value
                                                -- , readMd2meta
                                                -- , getAtKey
                                                -- )
-- import Text.Pandoc
import Uniform.Filenames
import Uniform.Json
import Lib.Pandoc (Pandoc)
import Uniform.Pandoc (readMd2meta)
import Uniform.Json (getAtKey)
import Uniform.Strings         hiding ( (</>) )


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
  (_, meta2) :: (Pandoc, Value) <- readMd2meta (dough2 </> mdfn)
  ixEntry                       <- getOneIndexEntry dough2 (dough2 </> mdfn)
  -- what needs to be checked ? 

  -- let doindex1 =  maybe False ("True"==) $ getAtKey meta2 "indexPage"  :: Bool
  let doindex2 = fromMaybe b
        where b = getAtKey meta2 "indexPage" :: Maybe Bool


  putIOwords ["checkOneMdFile end", showT meta2]
  return meta2


