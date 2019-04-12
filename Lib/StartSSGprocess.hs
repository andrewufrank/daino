------------------------------------------------------------------------------
--
-- Module      :   the  process to convert
--              files in any input format to html
--              orginals are found in dire doughDir and go to bakeDir
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- {-# LANGUAGE TypeSynonymInstances  #-}
module Lib.StartSSGprocess (ssgProcess) 
  --  (openMain, htf_thisModuelsTests)
                where

-- import Lib.CheckInput (checkOneMdFile)  
import Lib.CmdLineArgs                ( PubFlags(..) )

-- import Lib.CmdLineArgs (PubFlags(..))
import Lib.Foundation (SiteLayout(..)
          , settingsFileName, testLastUploadFileName)
-- import Lib.Foundation (SiteLayout(..), templatesDir)
-- import Lib.Pandoc                     ( docValToAllVal
--                                                 , markdownToPandocBiblio
--                                                 , pandocToContentHtml
--                                                 )
import Lib.ReadSettingFile (readSettings)
import Lib.Shake2 (shakeAll) 
-- import Lib.Templating                 ( putValinMaster )
import Lib.Watch (mainWatch) 

import Uniform.Error -- for instances
import Uniform.FileIO
-- import Uniform.Filenames-- with a simplified Action ~ ErrIO

import Uniform.FileStrings            ( )
import Uniform.Ftp
-- import Uniform.Pandoc                 ( Pandoc
--                                                 , htmloutFileType
--                                                 , write8
--                                                 , HTMLout (..)
--                                                 )
import Uniform.Time
import Uniform.WebServer (runScotty)

ssgProcess :: PubFlags -> ErrIO ()
ssgProcess flags = do  
      
      (layout2, port2) <- readSettings (settingsFile flags)

      -- read the time of the last upload
      uploadFileExist <- doesFileExist' testLastUploadFileName 
      lastUpload <- if uploadFileExist 
                      then do
                        lastUpload1 <- readFile2 testLastUploadFileName   
                        let lastUpload = read lastUpload1 :: UTCTime 
                        return lastUpload
                      else 
                        return year2000 

      let testWithLastTime  = testNewerModTime  lastUpload
                      -- compare with year2000 if all should be uploaded
      if watchFlag flags  -- implies server
        then  
          mainWatch layout2 flags port2
        else do  
          shakeAll layout2 flags ""
          -- the last is the filename that caused the shake call
          --  let landing = makeRelFile "landingPage.html"
          when (serverFlag flags) $  
            runScotty port2 (bakedDir layout2) (landingPage layout2)
          when (uploadFlag flags) $ do
                (a,s) <- runStateT
                    (ftpUploadDirsRecurse testWithLastTime (bakedDir layout2) 
                        (if testFlag flags then makeAbsDir "/ssg.gerastree.at/"
                                  else makeAbsDir "/frank.gerastree.at/")
                    )
                        ftp0
                currentTime <- getCurrentTimeUTC 
                writeFile2 testLastUploadFileName (show currentTime)
        
                putIOwords ["uploadTest completed", showT currentTime]             
          -- return ()
        
      putIOwords ["ssgBake done"]
      return () 