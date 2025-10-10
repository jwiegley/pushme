{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Pushme.Options where

import Control.Lens hiding (argument)
import Control.Logging
import Data.Aeson hiding (Options)
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative hiding (Success)

version :: String
version = "3.0.0"

copyright :: String
copyright = "2013-2025"

pushmeSummary :: String
pushmeSummary =
  "pushme " ++ version ++ ", (C) " ++ copyright ++ " John Wiegley"

data RsyncOptions = RsyncOptions
  { _rsyncFilters :: Maybe Text,
    _rsyncNoBasicOptions :: Bool,
    _rsyncNoDelete :: Bool,
    _rsyncPreserveAttrs :: Bool,
    _rsyncProtectTopLevel :: Bool,
    _rsyncOptions :: Maybe [Text],
    _rsyncReceiveFrom :: Maybe [Text],
    _rsyncActive :: Bool
  }
  deriving (Show, Eq)

instance FromJSON RsyncOptions where
  parseJSON (Object v) =
    RsyncOptions
      <$> v .:? "Filters"
      <*> v .:? "NoBasicOptions" .!= False
      <*> v .:? "NoDelete" .!= False
      <*> v .:? "PreserveAttrs" .!= False
      <*> v .:? "ProtectTopLevel" .!= False
      <*> v .:? "Options"
      <*> v .:? "ReceiveFrom"
      <*> v .:? "Active" .!= True
  parseJSON _ = errorL "Error parsing Rsync"

instance Semigroup RsyncOptions where
  RsyncOptions a1 b1 c1 d1 e1 f1 g1 h1
    <> RsyncOptions a2 b2 c2 d2 e2 f2 g2 h2 =
      RsyncOptions
        (a2 <|> a1)
        (b2 || b1)
        (c2 || c1)
        (d2 || d1)
        (e2 || e1)
        (f2 <|> f1)
        (g2 <|> g1)
        (h2 && h1)

makeLenses ''RsyncOptions

data Options = Options
  { _optsConfigDir :: FilePath,
    _optsDryRun :: Bool,
    _optsFilesets :: Maybe [Text],
    _optsClasses :: Maybe [Text],
    _optsSiUnits :: Bool,
    _optsVerbose :: Bool,
    _optsNoColor :: Bool,
    _optsRsyncOpts :: Maybe RsyncOptions,
    _optsCliArgs :: [String]
  }
  deriving (Show, Eq)

instance FromJSON Options where
  parseJSON (Object v) =
    Options
      <$> v .:? "Config" .!= "~/.config/pushme"
      <*> v .:? "DryRun" .!= False
      <*> v .:? "Filesets"
      <*> v .:? "Classes"
      <*> v .:? "SIUnits" .!= False
      <*> v .:? "Verbose" .!= False
      <*> v .:? "NoColor" .!= False
      <*> v .:? "GlobalOptions"
      <*> pure []
  parseJSON _ = errorL "Error parsing Options"

instance Semigroup Options where
  Options _a1 b1 c1 d1 e1 f1 g1 h1 i1
    <> Options a2 b2 c2 d2 e2 f2 g2 h2 i2 =
      Options
        a2
        (b2 || b1)
        (c2 <|> c1)
        (d2 <|> d1)
        (e2 || e1)
        (f2 || f1)
        (g2 || g1)
        (h2 <> h1)
        (i2 <|> i1)

makeLenses ''Options

separated :: Char -> ReadM [Text]
separated c = T.split (== c) <$> str

pushmeOpts :: Parser Options
pushmeOpts =
  Options
    <$> strOption
      ( long "config"
          <> value "~/.config/pushme"
          <> help "Config directory (default: ~/.config/pushme)"
      )
    <*> switch
      ( short 'n'
          <> long "dry-run"
          <> help "Do not take any actions, just report"
      )
    <*> optional
      ( option
          (separated ',')
          ( short 'f'
              <> long "filesets"
              <> help "File sets to synchronize (comma-separated)"
          )
      )
    <*> optional
      ( option
          (separated ',')
          ( short 'c'
              <> long "classes"
              <> help "Classes to synchronize (comma-separated)"
          )
      )
    <*> switch
      ( short 's'
          <> long "si-units"
          <> help "Use 1000 instead of 1024 as a divisor"
      )
    <*> switch
      ( short 'v'
          <> long "verbose"
          <> help "Report progress verbosely"
      )
    <*> switch
      ( long "no-color"
          <> help "Do not use ANSI colors in report output"
      )
    <*> optional
      ( RsyncOptions
          <$> optional
            ( strOption
                ( long "rsync-filters"
                    <> help "rsync filters to pass using --include-from"
                )
            )
          <*> switch
            ( long "rsync-no-basic-options"
                <> help "Do not pass -a (and possibly other basic options)"
            )
          <*> switch
            ( long "rsync-no-delete"
                <> help "Do not pass --delete"
            )
          <*> switch
            ( long "rsync-preserve-attrs"
                <> help "Preserve all attributes (i.e., pass -AXUNHE)"
            )
          <*> switch
            ( long "rsync-protect-top-level"
                <> help "Protect top-level items from deletion"
            )
          <*> optional
            ( option
                (separated ' ')
                ( long "rsync-options"
                    <> help "Space-separated list of options to pass to rsync"
                )
            )
          <*> pure Nothing
          <*> pure True
      )
    <*> many (argument (eitherReader Right) (metavar "ARGS"))

optionsDefinition :: ParserInfo Options
optionsDefinition =
  info
    (helper <*> pushmeOpts)
    (fullDesc <> progDesc "" <> header pushmeSummary)

getOptions :: IO Options
getOptions = execParser optionsDefinition
