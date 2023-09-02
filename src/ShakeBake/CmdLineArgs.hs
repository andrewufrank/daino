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
  
import Foundational.CmdLineFlags 
-- import Foundational.SettingsPage 
-- import Options.Applicative.Builder()

-- | the command line arguments raw
--  number of args must correspond in order and number with the
--  command arguments described in the parser
data LitArgs = LitArgs
  {
   draftSwitch -- x^ d
  , privateSwitch -- x^ v
--   , publishSwitch  -- x^ p
--   , oldSwitch -- x^ o
--   , finishedSwitch -- x^ f 
  , testSwitch  -- x^ t
  , testNewSwitch -- x^ T -- test after delete to create all new
  , restartSwitch -- x^ R   -- delete all site and reconstruct (compare T)
  , quickSwitch -- x^ q
  , serverSwitch -- x^ s 
  , watchSwitch -- x^ w 
  , verboseSwitch --x^ v  -- set debug level to 2 
--   , uploadSwitch -- x^ u  -- not yet used  
         :: Bool
  , locationDirArg -- x^ l 
        :: String
   } deriving (Show)

cmdArgs :: Parser LitArgs
-- | strings which have no default result in enforced arguments
-- order and type of arguments must correspod to LitArgs
cmdArgs  =  
  LitArgs
    <$> switch (long "draft" <> short 'd' 
        <> help "include draft material, else only `publish`")
    <*> switch
          (long "private" <> short 'p' <> help
            "include the private data, else only public"
          )
    -- <$> switch
    --       (long "publish" <> short 'p' <> help
    --         "include material ready to publish"
    --       )
    -- <*> switch (long "old" <> short 'o' <> help "include old material")
    <*> switch
          (long "test" <> short 't' <> help
            "use test data (site/dough), continue test, start server on port set"
          )
    <*> switch
          (long "testComplete" <> short 'T' <> help
            "use test data (site/dough), complete test, start server on port set"
          )
    <*> switch
          (long "restart site generation" <> short 'R' <> help
            "delete the current site"
          )
    <*> switch
          (long "quick" <> short 'q' 
            <> help "produce only html, but not the (slower) pdf's"
          )
    <*> switch
          (long "server" <> short 's' 
                -- <> value True   -- not working
                <> help "start a server on port set in siteHeader file")
    <*> switch
        (long "watch" <> short 'w' <> help
          "start the watch of files for restarting bake"
        )
    <*> switch
        (long "verbose" <> short 'v' <> help
          "add a bit more debug output"
        )
    <*>  strOption
        (long "location" <> short 'l' <> value "." <> help 
            "the directory in which to find the settings file")
    -- <*> switch
    --     (long "upload" <> short 'u' <> help
    --       "upload to external server"
    --     )





parseArgs2input ::   Text -> Text -> ErrIO PubFlags
-- getting cmd line arguments, produces the input in the usable form
--  with a default value for the file name
-- the two text arguments are used in the cmd arg parse
-- is specific to the parser (and thus to the cmd line arguments

parseArgs2input   t1 t2 = do
  args1 <- getArgsParsed t1 t2
  when False $ putIOwords ["parseArgs2input: args found", showPretty args1]
  workingdir1 :: Path Abs Dir <- currentDir


  let flags1 = PubFlags { draftFlag    = draftSwitch args1
                        -- , publishFlag  = publishSwitch args1
                        --  , oldFlag      = oldSwitch args1
                         , privateFlag  = privateSwitch args1
                         , testFlag     = testSwitch args1
                         , testNewFlag = testNewSwitch args1
                         , quickFlag    = quickSwitch args1
                         , restartFlag    = restartSwitch args1
                         , serverFlag   = serverSwitch args1
                         , watchFlag    = watchSwitch args1
                         , verboseFlag = verboseSwitch args1
                         , locationDir = locationDirArg args1
                         -- perhaps wrong, could be site/dough?
                        -- ,  uploadFlag = uploadSwitch args1
                        , mdFiles = zero 
                         }

--   let flags2 = if testFlag flags1
--         then flags1 { settingsFile = testdataDir </> settingsFileName }
--                     --  ,  PortNumber = sourceDirTest
--         else flags1

  when False $ putIOwords ["parseArgs2input:  inputs ", showPretty flags1]
  return flags1



getArgsParsed :: Text -> Text -> ErrIO LitArgs
getArgsParsed  t1 t2 = do
        args <- callIO $ execParser opts
        return args
    where
        opts  = info (helper <*> cmdArgs)
                  (fullDesc <> (progDesc . t2s $ t1) <> (header . t2s $ t2))
