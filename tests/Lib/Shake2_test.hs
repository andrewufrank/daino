-----------------------------------------------------------------------------
--
-- Module      :   testing the new shake 
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports#-}

module Lib.Shake2_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import Uniform.Test.TestHarness


import Lib.Foundation (progName, SiteLayout (..), templatesDirName)
import Lib.Shake2
import Lib.Foundation_test (testLayout)
import Lib.Foundation (templatesImgDirName, bannerImageFileName)
import Lib.CmdLineArgs (allFlags)

    -- tests a conversion, is always ok
    --  "Lib.Shake_test - files written to /home/frank/.SSG/bakedTest"
-- test_shakeMD = 
--     do 
--         res <- shakeMD testLayout allFlags
--                 (doughDir testLayout)
--                 ((themeDir $ testLayout) `addFileName` ( templatesDirName)
--                             :: Path Abs Dir)
--                 (makeAbsDir "/home/frank/.SSG/bakedTest0")
--                 (templatesImgDirName `addFileName` bannerImageFileName)
--         assertEqual () res  

test_shakeAll = 
    do  
        res <- runErr $ shakeAll True testLayout allFlags "TEST"
        assertEqual (Right ()) res

-- shakeOp :: Text -> ErrIO Text 
--         -- SiteLayout -> PubFlags -> FilePath -> ErrIO ()
-- shakeOp _ = do 
--     putIOwords ["shake with layout", showT testLayout]
--     shakeAll testLayout allFlags "" 
--     -- let bakedP = bakeDir testLayout
--     -- ex
--     -- res <- 
-- test_shakeAll = testVar0FileIO "shake" "shake" "shakeTest" shakeOp  
    -- testVar0FileIO progName  a resfile op
-- doughP = makeAbsDir "/home/frank/Workspace8/ssg/docs/site/dough"

-- test_directoryToBake_null = do 
--     res <-  getDirectoryToBake "zzzzzzzzzzzxxxxxxxxx"
--                 doughP ["*.txt"]
--     assertEqual [] res  
        --    propably impossible to construct
        -- no undo of action given 