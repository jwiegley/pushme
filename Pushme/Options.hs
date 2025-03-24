{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Pushme.Options where

import Control.Logging
import Data.Aeson hiding (Options)
import Data.Data (Data)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Options.Applicative hiding (Success)

version :: String
version = "2.0.0.1"

copyright :: String
copyright = "2013-4"

pushmeSummary :: String
pushmeSummary =
  "pushme " ++ version ++ ", (C) " ++ copyright ++ " John Wiegley"

data Options = Options
  { jobs :: Maybe Int,
    dryRun :: Bool,
    ssh :: Maybe String,
    rsyncOpt :: Maybe String,
    includeFrom :: Maybe FilePath,
    checksum :: Bool,
    fromName :: String,
    filesets :: Maybe String,
    classes :: Maybe String,
    siUnits :: Bool,
    verbose :: Bool,
    cliArgs :: [String]
  }
  deriving (Data, Typeable, Show, Eq)

instance FromJSON Options where
  parseJSON (Object v) =
    Options
      <$> v .:? "jobs"
      <*> (fromMaybe False <$> v .:? "dryRun")
      <*> v .:? "ssh"
      <*> v .:? "rsyncOpt"
      <*> v .:? "includeFrom"
      <*> (fromMaybe False <$> v .:? "checksum")
      <*> (fromMaybe "" <$> v .:? "fromName")
      <*> v .:? "filesets"
      <*> v .:? "classes"
      <*> (fromMaybe False <$> v .:? "siUnits")
      <*> (fromMaybe False <$> v .:? "verbose")
      <*> (fromMaybe [] <$> v .:? "cliArgs")
  parseJSON _ = errorL "Error parsing Options"

instance Semigroup Options where
  Options a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1
    <> Options a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 =
      Options
        (a1 <|> a2)
        (b1 || b2)
        (c1 <|> c2)
        (d1 <|> d2)
        (e1 <|> e2)
        (f1 || f2)
        (g1 <|> g2)
        (h1 <|> h2)
        (i1 <|> i2)
        (j1 || j2)
        (k1 || k2)
        (l1 <|> l2)

pushmeOpts :: Parser Options
pushmeOpts =
  Options
    <$> optional
      ( option
          auto
          ( short 'j'
              <> long "jobs"
              <> help "Run INT concurrent finds at once"
          )
      )
    <*> switch
      ( short 'n'
          <> long "dry-run"
          <> help "Don't take any actions"
      )
    <*> optional
      ( strOption
          ( long "ssh"
              <> help "Use a specific ssh command"
          )
      )
    <*> optional
      ( strOption
          ( long "rsync"
              <> help "Use a specific rsync command"
          )
      )
    <*> optional
      ( strOption
          ( long "include-from"
              <> help "Use a specific rsync command"
          )
      )
    <*> switch
      ( long "checksum"
          <> help "Pass --checksum flag to rsync"
      )
    <*> strOption
      ( long "from"
          <> help "Name of the current (sending) host"
      )
    <*> optional
      ( strOption
          ( short 'f'
              <> long "filesets"
              <> help "Synchronize the given fileset(s) (comma-sep)"
          )
      )
    <*> optional
      ( strOption
          ( short 'c'
              <> long "classes"
              <> help "Filesets classes to synchronize (comma-sep)"
          )
      )
    <*> switch
      ( long "si"
          <> help "Use 1000 instead of 1024 to divide"
      )
    <*> switch
      ( short 'v'
          <> long "verbose"
          <> help "Report progress verbosely"
      )
    <*> many (argument (eitherReader Right) (metavar "ARGS"))

optionsDefinition :: ParserInfo Options
optionsDefinition =
  info
    (helper <*> pushmeOpts)
    (fullDesc <> progDesc "" <> header pushmeSummary)

getOptions :: IO Options
getOptions = execParser optionsDefinition
