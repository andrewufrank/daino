-----------------------------------------------------------------------------
--
-- Module      :   an example for a command line argument setup
--                  is a Main and starts with convenience
-- for information see https://github.com/pcapriotti/optparse-applicative
-- change the getAttr function to return Text
-----------------------------------------------------------------------------
  {-# LANGUAGE
  MultiParamTypeClasses
  , TypeSynonymInstances
--    , FunctionalDependencies
  , FlexibleInstances
  , FlexibleContexts
  , ScopedTypeVariables
--    , UndecidableInstances
  , OverloadedStrings
  , TypeFamilies

  #-}

module Lib.CmdLineArgs where





import           Uniform.Strings
import           Uniform.FileIO
-- import           Uniform.Error
-- import           Uniform.Convenience.StartApp
import           Data.Semigroup                 ( (<>) )
import           Options.Applicative.Builder
import           Options.Applicative

import           Lib.Foundation

programName, progTitle :: Text
programName = "CmdLineArgsExample.hs"
progTitle = "example for command line argument processing" :: Text


-- | the command line arguments raw
--  number of args must correspond in order and number with the
--  command arguments described in the parser
data LitArgs = LitArgs
  { publishSwitch  -- ^ p
  , oldSwitch -- ^ o
  , draftSwitch -- ^ d
  , testSwitch  :: Bool   -- ^ t
  , settingsFileString  ::  String  -- ^ s 
   } deriving (Show)

cmdArgs :: Path Abs File -> Parser LitArgs
-- | strings which have no default result in enforced arguments
-- order and type of arguments must correspod to LitArgs
cmdArgs defaultSetting =
  LitArgs
    <$> switch
          (long "publish" <> short 'p' <> help
            "include material ready to publish"
          )
    <*> switch (long "old" <> short 'o' <> help "include old material")
    <*> switch (long "draft" <> short 'd' <> help "include draft material")
    <*> switch
          (long "test" <> short 't' <> help
            "use test data in layout (site/dough)"
          )
    <*> strOption
          (  long "settingsFile (optional)"
          <> short 'g'
          <> metavar "File1"
          <> value (toFilePath defaultSetting)
          <> help
               (unwords
                 [ "not yet used"
                 , "settingsFile (default workingdir/"
                 , toFilePath defaultSetting
                 , ")"
                 ]
               )
          )

-- | the switches for material to include
data Inputs = Inputs
        {publishFlag
        , oldFlag
        , draftFlag
        , testFlag  :: Bool
        , settingsFile :: Path Abs File
        } deriving (Show, Read, Eq)


parseArgs2input :: Path Abs File -> Text -> Text -> ErrIO Inputs
-- getting cmd line arguments, produces the input in the usable form
--  with a default value for the file name
-- the two text arguments are used in the cmd arg parse
-- is specific to the parser (and thus to the cmd line arguments

parseArgs2input settings t1 t2 = do
  args1 <- getArgsParsed settings t1 t2
  putIOwords ["parseArgs2input: args found", showT args1]



  let inputs1 = Inputs { publishFlag  = publishSwitch args1
                       , oldFlag      = oldSwitch args1
                       , draftFlag    = draftSwitch args1
                       , testFlag     = testSwitch args1
                       , settingsFile = settings
                       }

  let inputs2 = if testFlag inputs1
        then inputs1 { settingsFile = testSettingsFileName }
        else inputs1

  putIOwords ["parseArgs2input:  inputs ", showT inputs2]
  return inputs2


getArgsParsed :: Path Abs File -> Text -> Text -> ErrIO LitArgs
getArgsParsed fn t1 t2 = do
  args <- callIO $ execParser (opts fn)
  return args
 where
  opts fn = info (helper <*> cmdArgs fn)
                 (fullDesc <> (progDesc . t2s $ t1) <> (header . t2s $ t2))
