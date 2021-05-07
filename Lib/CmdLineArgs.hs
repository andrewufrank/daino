----------------------------------------------------------------------
--
-- Module      :   an example for a command line argument setup
--                  is a Main and starts with convenience
-- for information see https://github.com/pcapriotti/optparse-applicative
-- change the getAttr function to return Text
----------------------------------------------------------------------
  {-# LANGUAGE
  MultiParamTypeClasses
  -- , TypeSynonymInstances
--    , FunctionalDependencies
      , FlexibleInstances
      , FlexibleContexts
      , ScopedTypeVariables
    --    , UndecidableInstances
      , OverloadedStrings
      , TypeFamilies

  #-}

module Lib.CmdLineArgs where
import UniformBase
    ( Path,
      Abs,
      Dir,
      File,
      Rel,
      Text,
      callIO,
      currentDir,
      t2s,
      putIOwords,
      showT,
      Zeros(zero),
      ErrIO,
      Filenames3((</>)) )
import Options.Applicative.Builder
    ( fullDesc, header, help, info, long, progDesc, short, switch )
import Options.Applicative
    ( Parser,
      fullDesc,
      header,
      help,
      info,
      long,
      progDesc,
      short,
      switch,
      execParser,
      helper )

import Lib.Foundation ( testSettingsFileName )


-- | the command line arguments raw
--  number of args must correspond in order and number with the
--  command arguments described in the parser
data LitArgs = LitArgs
  { publishSwitch  -- ^ p
  , oldSwitch -- ^ o
  , draftSwitch -- ^ d
  , testSwitch  -- ^ t
  , serverSwitch -- ^ s 
  , watchSwitch -- ^ w 
  , uploadSwitch -- ^ u  -- not yet used  
         :: Bool
   } deriving (Show)

cmdArgs :: Parser LitArgs
-- | strings which have no default result in enforced arguments
-- order and type of arguments must correspod to LitArgs
cmdArgs  =
  LitArgs
    <$> switch
          (long "publish" <> short 'p' <> help
            "include material ready to publish"
          )
    <*> switch (long "old" <> short 'o' <> help "include old material")
    <*> switch (long "draft" <> short 'd' <> help "include draft material")
    <*> switch
          (long "test" <> short 't' <> help
            "use test data in layout (site/dough), start server on 3000"
          )
    <*> switch
          (long "server" <> short 's' <> help "start a server on port 3001")
    <*> switch
        (long "watch" <> short 'w' <> help
          "start the watch of files for restarting bake"
        )
    <*> switch
        (long "upload" <> short 'u' <> help
          "upload to external server"
        )

-- | the switches for material to include
data PubFlags = PubFlags
        {publishFlag
        , oldFlag
        , draftFlag
        , testFlag
        , watchFlag
        , serverFlag:: Bool
        , uploadFlag :: Bool 
        , settingsFile :: Path Abs File
        } deriving (Show,  Eq)  -- no read for path 

instance Zeros PubFlags where 
    zero = PubFlags zero zero zero zero zero zero zero zero

allFlags :: PubFlags
allFlags = zero {publishFlag = True  -- not including draft
  , oldFlag = True 
  , draftFlag = False
  , settingsFile = testSettingsFileName}



parseArgs2input :: Path Rel File -> Text -> Text -> ErrIO PubFlags
-- getting cmd line arguments, produces the input in the usable form
--  with a default value for the file name
-- the two text arguments are used in the cmd arg parse
-- is specific to the parser (and thus to the cmd line arguments

parseArgs2input settingsFN t1 t2 = do
  args1 <- getArgsParsed t1 t2
  putIOwords ["parseArgs2input: args found", showT args1]
  workingdir1 :: Path Abs Dir <- currentDir


  let flags1 = PubFlags { publishFlag  = publishSwitch args1
                         , oldFlag      = oldSwitch args1
                         , draftFlag    = draftSwitch args1
                         , testFlag     = testSwitch args1
                         , watchFlag    = watchSwitch args1
                         , serverFlag   = serverSwitch args1
                         , settingsFile = workingdir1 </> settingsFN
                         -- perhaps wrong, could be site/dough?
                        ,  uploadFlag = uploadSwitch args1
                         }

  let flags2 = if testFlag flags1
        then flags1 { settingsFile = testSettingsFileName }
                    --  ,  PortNumber = sourceDirTest
        else flags1

  putIOwords ["parseArgs2input:  inputs ", showT flags2]
  return flags2


getArgsParsed :: Text -> Text -> ErrIO LitArgs
getArgsParsed  t1 t2 = do
        args <- callIO $ execParser opts
        return args
    where
        opts  = info (helper <*> cmdArgs)
                  (fullDesc <> (progDesc . t2s $ t1) <> (header . t2s $ t2))
