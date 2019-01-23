
------------------------------------------------------------------------------
--
-- Module      :   the  process to convert
--              files in any input format to html
--              orginals are found in dire doughDir and go to bakeDir
--

-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lib.Shake
     where

import Uniform.Strings
import Uniform.Filenames
import Uniform.FileStrings () -- for instances

--import Uniform.Piped
--import           Uniform.FileIO as FN hiding ( (</>), (<.>))  -- (<>),
--
--import Lib.Pandoc (markdownToHTML4x)   -- with a simplified Action ~ ErrIO
--
--import Lib.Templating
--import Lib.FileMgt
--import Lib.Foundation

--import qualified Pipes as Pipe
--import qualified Pipes.Prelude as Pipe
--import Pipes ((>->)) -- , (~>)
--import qualified Path  as Path


shake ::    ErrIO ()
shake   = callIO $ do
    putIOwords ["\nshake start"]
    msg <- return "ok" -- bakeAllInSiteMD (bakeOneFile2 False)  doughPath  reportFilePath
    putIOwords ["\nshake done msg " , msg, "\n"]

    return ()



