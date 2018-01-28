{-# LANGUAGE DeriveDataTypeable #-}

module Pushme.Options where

import Data.Data (Data)
import Data.Monoid
import Data.Typeable (Typeable)
import Options.Applicative hiding (Success, (&))

version :: String
version = "2.0.0.1"

copyright :: String
copyright = "2013-4"

pushmeSummary :: String
pushmeSummary =
    "pushme " ++ version ++ ", (C) " ++ copyright ++ " John Wiegley"

data Options = Options
    { jobs      :: Int
    , dryRun    :: Bool
    , noSync    :: Bool
    , copyAll   :: Bool
    , dump      :: Bool
    , ssh       :: String
    , rsyncOpt  :: String
    , checksum  :: Bool
    , fromName  :: String
    , configDir :: String
    , filesets  :: String
    , classes   :: String
    , siUnits   :: Bool
    , verbose   :: Bool
    , quiet     :: Bool
    , cliArgs   :: [String]
    }
    deriving (Data, Typeable, Show, Eq)

pushmeOpts :: Parser Options
pushmeOpts = Options
    <$> option auto
        (   short 'j'
         <> long "jobs"
         <> value 1
         <> help "Run INT concurrent finds at once (default: 1)")
    <*> switch
        (   short 'n'
         <> long "dry-run"
         <> help "Don't take any actions")
    <*> switch
        (   short 'N'
         <> long "no-sync"
         <> help "Don't even attempt a dry-run sync")
    <*> switch
        (   long "copy-all"
         <> help "For git-annex directories, copy all files")
    <*> switch
        (   long "dump"
         <> help "Show all the stores that would be synced")
    <*> strOption
        (   long "ssh"
         <> value ""
         <> help "Use a specific ssh command")
    <*> strOption
        (   long "rsync"
         <> value ""
         <> help "Use a specific rsync command")
    <*> switch
        (   long "checksum"
         <> help "Pass --checksum flag to rsync")
    <*> strOption
        (   long "from"
         <> value ""
         <> help "Provide the name of the current host")
    <*> strOption
        (   long "config"
         <> value "~/.pushme"
         <> help "Directory containing configuration files (def: ~/.pushme)")
    <*> strOption
        (   short 'f'
         <> long "filesets"
         <> value ""
         <> help "Synchronize the given fileset(s) (comma-sep)")
    <*> strOption
        (   short 'c'
         <> long "classes"
         <> value ""
         <> help "Filesets classes to synchronize (comma-sep)")
    <*> switch
        (   long "si"
         <> help "Use 1000 instead of 1024 to divide")
    <*> switch
        (   short 'v'
         <> long "verbose"
         <> help "Report progress verbosely")
    <*> switch
        (   short 'q'
         <> long "quiet"
         <> help "Be a little quieter")
   <*> many (argument (eitherReader Right) (metavar "ARGS"))
    -- <*> many (argument Just (metavar "ARGS"))

optionsDefinition = info
    (helper <*> pushmeOpts)
    (fullDesc <> progDesc "" <> header pushmeSummary)

getOptions = execParser optionsDefinition
