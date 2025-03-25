{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Pushme.Options where

import Control.Logging
import Data.Aeson hiding (Options)
import Data.Data (Data)
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
  { dryRun :: Bool,
    ssh :: Maybe String,
    includeFrom :: Maybe FilePath,
    checksum :: Bool,
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
      <$> v .:? "dryRun" .!= False
      <*> v .:? "ssh"
      <*> v .:? "includeFrom"
      <*> v .:? "checksum" .!= False
      <*> v .:? "filesets"
      <*> v .:? "classes"
      <*> v .:? "siUnits" .!= False
      <*> v .:? "verbose" .!= False
      <*> pure []
  parseJSON _ = errorL "Error parsing Options"

instance Semigroup Options where
  Options b1 c1 e1 f1 h1 i1 j1 k1 l1
    <> Options b2 c2 e2 f2 h2 i2 j2 k2 l2 =
      Options
        (b1 || b2)
        (c1 <|> c2)
        (e1 <|> e2)
        (f1 || f2)
        (h1 <|> h2)
        (i1 <|> i2)
        (j1 || j2)
        (k1 || k2)
        (l1 <|> l2)

pushmeOpts :: Parser Options
pushmeOpts =
  Options
    <$> switch
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
          ( long "include-from"
              <> help "Use a specific rsync command"
          )
      )
    <*> switch
      ( long "checksum"
          <> help "Pass --checksum flag to rsync"
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
