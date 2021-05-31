----------------------------------------------------------------------
--
-- Module      :   
-- | a command line argument setup
--                  is a Main and starts with convenience
-- for information see https://github.com/pcapriotti/optparse-applicative
-- change the getAttr function to return Text
----------------------------------------------------------------------
  {-# LANGUAGE
  MultiParamTypeClasses
      , FlexibleInstances
      , FlexibleContexts
      , ScopedTypeVariables
      , OverloadedStrings
      , TypeFamilies

  #-}

module ShakeBake.CmdLineArgs where
import UniformBase
import Uniform.CmdLineArgs   -- from u2to 
  
import Foundational.LayoutFlags 
import Foundational.MetaPage
-- import Options.Applicative.Builder()

-- | the command line arguments raw
--  number of args must correspond in order and number with the
--  command arguments described in the parser
data LitArgs = LitArgs
  { publishSwitch  -- x^ p
  , oldSwitch -- x^ o
  , draftSwitch -- x^ d
  , testSwitch  -- x^ t
  , serverSwitch -- x^ s 
  , watchSwitch -- x^ w 
  , uploadSwitch -- x^ u  -- not yet used  
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
            "use test data in layout (site/dough), start server on port set"
          )
    <*> switch
          (long "server" <> short 's' 
                -- <> value True   -- not working
                <> help "start a server on port set in settings file")
    <*> switch
        (long "watch" <> short 'w' <> help
          "start the watch of files for restarting bake"
        )
    <*> switch
        (long "upload" <> short 'u' <> help
          "upload to external server"
        )





parseArgs2input :: Path Abs Dir -> Text -> Text -> ErrIO PubFlags
-- getting cmd line arguments, produces the input in the usable form
--  with a default value for the file name
-- the two text arguments are used in the cmd arg parse
-- is specific to the parser (and thus to the cmd line arguments

parseArgs2input testdataDir t1 t2 = do
  args1 <- getArgsParsed t1 t2
  when False $ putIOwords ["parseArgs2input: args found", showPretty args1]
  workingdir1 :: Path Abs Dir <- currentDir


  let flags1 = PubFlags { publishFlag  = publishSwitch args1
                         , oldFlag      = oldSwitch args1
                         , draftFlag    = draftSwitch args1
                         , testFlag     = testSwitch args1
                         , watchFlag    = watchSwitch args1
                         , serverFlag   = serverSwitch args1
                         , settingsFile = workingdir1 </> settingsFileName 
                         -- perhaps wrong, could be site/dough?
                        ,  uploadFlag = uploadSwitch args1
                         }

  let flags2 = if testFlag flags1
        then flags1 { settingsFile = testdataDir </> settingsFileName }
                    --  ,  PortNumber = sourceDirTest
        else flags1

  when False $ putIOwords ["parseArgs2input:  inputs ", showPretty flags2]
  return flags2


getArgsParsed :: Text -> Text -> ErrIO LitArgs
getArgsParsed  t1 t2 = do
        args <- callIO $ execParser opts
        return args
    where
        opts  = info (helper <*> cmdArgs)
                  (fullDesc <> (progDesc . t2s $ t1) <> (header . t2s $ t2))
