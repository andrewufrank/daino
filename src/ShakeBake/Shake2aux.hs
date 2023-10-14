----------------------------------------------------------------------
--
-- Module Shake2aux:
----------------------------------------------------------------------
{- tools to move to proper place
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


module ShakeBake.Shake2aux where

import           Uniform.Shake
-- import Foundational.SettingsPage
-- import Foundational.CmdLineFlags
-- import ShakeBake.Bake

shakeArgs2 :: NoticeLevel -> Path b t -> Rules () -> IO ()
{- | set the options for shake
 called in shakeMD
-}
shakeArgs2 debug bakedP = do
    -- putIOwords ["shakeArgs2", "bakedP", s2t . toFilePath $ bakedP]
    res <-
        shake
            shakeOptions
                { shakeFiles = toFilePath bakedP  -- where should the shake files to into baked?
                , shakeVerbosity = verbose debug 
                -- , shakeVerbosity = Verbose --  Loud Info -- 
                        -- verbose gives a single line for each file processed
                        --          plus info for copying
                        -- info gives nothing in normal process 
                , shakeLint = Nothing -- Just LintBasic
                }
    return res

verbose debug = if inform debug then Verbose else Info

needPwithoutput debug t1 t2 files = do 
        putInformOne debug ["\nneeds set", t1, t2, showT files]
        needP files 


io2bool :: MonadIO m => ErrIO b -> m b
io2bool op = do
    -- todo move
    x <- liftIO $ runErr op
    let res = case x of
            Left msg -> errorT [msg]
            Right b -> b
    return res

{- | the generic copy for all the files
 which can just be copied
 (exceptions md, which are a special case of needed)
-}
copyFileToBaked ::
    ( Filenames3 fp (Path Rel File)
    , FileResultT fp (Path Rel File) ~ Path Abs File
    ) =>
    NoticeLevel ->
    fp ->
    Path Abs Dir ->
    FilePath ->
    Action ()
copyFileToBaked debug doughP bakedP out = do
    let outP = makeAbsFile out :: Path Abs File
    when (inform debug) $ liftIO $ putIOwords ["copyFileToBaked outP", showT outP, "\n"]
    let fromfile = doughP </> makeRelativeP bakedP outP
    -- putInformOne debug
    --             ["\ncopyFileToBaked fromfile ", showT fromfile, "added NEED automatically"]
    copyFileChangedP fromfile outP