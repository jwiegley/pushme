{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Pushme.Options where

import Control.Lens hiding (argument)
import Control.Logging
import Control.Monad (forM_, when)
import Data.Aeson hiding (Options)
import Data.Aeson.Types (modifyFailure)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Options.Applicative hiding (Success)

version :: String
version = "3.0.0"

copyright :: String
copyright = "2012-2026"

pushmeSummary :: String
pushmeSummary =
  "pushme " ++ version ++ ", (C) " ++ copyright ++ " John Wiegley"

data RsyncOptions = RsyncOptions
  { _rsyncFilters :: Maybe Text
  , _rsyncExtraFilters :: Maybe Text
  , _rsyncNoBasicOptions :: Bool
  , _rsyncNoDelete :: Bool
  , _rsyncPreserveACLs :: Bool -- -A
  , _rsyncPreserveXattrs :: Bool -- -X
  , _rsyncPreserveAtimes :: Bool -- -U
  , _rsyncPreserveCrtimes :: Bool -- -N
  , _rsyncPreserveHardLinks :: Bool -- -H
  , _rsyncPreserveExecutability :: Bool -- -E
  , _rsyncProtectTopLevel :: Bool
  , _rsyncOptions :: Maybe [Text]
  , _rsyncReceiveFrom :: Maybe [Text]
  , _rsyncActive :: Bool
  }
  deriving (Show, Eq)

instance FromJSON RsyncOptions where
  parseJSON (Object v) = do
    preserveAll <- v .:? "PreserveAttrs" .!= False
    RsyncOptions
      <$> v .:? "Filters"
      <*> v .:? "ExtraFilters"
      <*> v .:? "NoBasicOptions" .!= False
      <*> v .:? "NoDelete" .!= False
      <*> v .:? "PreserveACLs" .!= preserveAll
      <*> v .:? "PreserveXattrs" .!= preserveAll
      <*> v .:? "PreserveAtimes" .!= preserveAll
      <*> v .:? "PreserveCrtimes" .!= preserveAll
      <*> v .:? "PreserveHardLinks" .!= preserveAll
      <*> v .:? "PreserveExecutability" .!= preserveAll
      <*> v .:? "ProtectTopLevel" .!= False
      <*> v .:? "Options"
      <*> v .:? "ReceiveFrom"
      <*> v .:? "Active" .!= True
  parseJSON _ = errorL "Error parsing Rsync"

instance Semigroup RsyncOptions where
  RsyncOptions a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1
    <> RsyncOptions a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 =
      RsyncOptions
        (a2 <|> a1) -- Filters
        (combineFilters b1 b2) -- ExtraFilters
        (c2 || c1) -- NoBasicOptions
        (d2 || d1) -- NoDelete
        (e2 || e1) -- PreserveACLs
        (f2 || f1) -- PreserveXattrs
        (g2 || g1) -- PreserveAtimes
        (h2 || h1) -- PreserveCrtimes
        (i2 || i1) -- PreserveHardLinks
        (j2 || j1) -- PreserveExecutability
        (k2 || k1) -- ProtectTopLevel
        (l2 <|> l1) -- Options
        (m2 <|> m1) -- ReceiveFrom
        (n2 && n1) -- Active

combineFilters :: Maybe Text -> Maybe Text -> Maybe Text
combineFilters Nothing Nothing = Nothing
combineFilters (Just a) Nothing = Just a
combineFilters Nothing (Just b) = Just b
combineFilters (Just a) (Just b) = Just (a <> "\n" <> b)

makeLenses ''RsyncOptions

-- | Alias definition for host names with optional configuration overrides
data Alias = Alias
  { _aliasName :: Text
  , _aliasHost :: Text
  , _aliasMaxJobs :: Maybe Int
  , _aliasVariables :: Map Text Text
  , _aliasOptions :: Maybe [Text]
  }
  deriving (Show, Eq)

instance FromJSON Alias where
  parseJSON (Object v) = modifyFailure addContext $ do
    -- Parse Variables if present
    variables <- v .:? "Variables" .!= M.empty
    -- Check for legacy Prefix field and merge it
    legacyPrefix <- v .:? "Prefix"
    let finalVariables = case legacyPrefix of
          Just prefix -> M.insert "prefix" (pack prefix) variables
          Nothing -> variables
    -- Explicitly parse Host to provide better error message
    mHost <- v .:? "Host"
    host <- case mHost of
      Nothing -> fail "Missing required field 'Host' in alias definition"
      Just h -> pure h
    Alias "" host -- Name will be filled in from the Map key
      <$> v .:? "MaxJobs"
      <*> pure finalVariables
      <*> v .:? "Options"
   where
    addContext msg = "Error parsing Alias: " ++ msg
  parseJSON _ = errorL "Error parsing Alias"

makeLenses ''Alias

-- Note: Host type is defined in Main.hs as:
-- data Host = Host { _hostName :: Text, _hostMaxJobs :: Int }

-- | Host reference combining logical name (for fileset matching) with actual host details
data HostRef = HostRef
  { _hostRefLogicalName :: Text
  , _hostRefActualHost :: (Text, Int) -- (hostName, maxJobs) - avoiding circular dependency
  , _hostRefVariables :: Map Text Text
  , _hostRefOptions :: Maybe [Text] -- Per-host rsync options from alias
  }
  deriving (Show, Eq, Ord)

makeLenses ''HostRef

data Options = Options
  { _optsConfigDir :: FilePath
  , _optsDryRun :: Bool
  , _optsFilesets :: Maybe [Text]
  , _optsClasses :: Maybe [Text]
  , _optsSiUnits :: Bool
  , _optsVerbose :: Bool
  , _optsNoColor :: Bool
  , _optsReverse :: Bool
  , _optsRsyncOpts :: Maybe RsyncOptions
  , _optsAliases :: Map Text Alias
  , _optsCliArgs :: [String]
  }
  deriving (Show, Eq)

instance FromJSON Options where
  parseJSON (Object v) = do
    aliasMap <- v .:? "Aliases" .!= M.empty
    -- Fill in the _aliasName field from the Map keys
    let aliasMapWithNames = M.mapWithKey (\k a -> a{_aliasName = k}) aliasMap
    -- Validate that all aliases have a non-empty Host field
    forM_ (M.toList aliasMapWithNames) $ \(name, alias) ->
      when (T.null (alias ^. aliasHost)) $
        fail $
          "Alias '" ++ unpack name ++ "' is missing required field 'Host'"
    Options
      <$> v .:? "Config" .!= "~/.config/pushme"
      <*> v .:? "DryRun" .!= False
      <*> v .:? "Filesets"
      <*> v .:? "Classes"
      <*> v .:? "SIUnits" .!= False
      <*> v .:? "Verbose" .!= False
      <*> v .:? "NoColor" .!= False
      <*> v .:? "Reverse" .!= False
      <*> v .:? "GlobalOptions"
      <*> pure aliasMapWithNames
      <*> pure []
  parseJSON _ = errorL "Error parsing Options"

instance Semigroup Options where
  Options _a1 b1 c1 d1 e1 f1 g1 g1r h1 i1 j1
    <> Options a2 b2 c2 d2 e2 f2 g2 g2r h2 i2 j2 =
      Options
        a2
        (b2 || b1)
        (c2 <|> c1)
        (d2 <|> d1)
        (e2 || e1)
        (f2 || f1)
        (g2 || g1)
        (g2r || g1r)
        (h2 <> h1)
        (i2 <> i1) -- Right-biased merge: right Map wins on key conflicts
        (j2 <|> j1)

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
    <*> switch
      ( short 'R'
          <> long "reverse"
          <> help "Pull from remote hosts instead of pushing to them"
      )
    <*> optional
      ( (\filters noBasic noDelete preserveAll protectTop opts ->
           RsyncOptions filters Nothing noBasic noDelete
             preserveAll preserveAll preserveAll preserveAll preserveAll preserveAll
             protectTop opts Nothing True)
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
                <> help "Preserve all attributes (-AXUNHE)"
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
      )
    <*> pure M.empty -- Aliases come from config file, not CLI
    <*> many (argument (eitherReader Right) (metavar "ARGS"))

optionsDefinition :: ParserInfo Options
optionsDefinition =
  info
    (helper <*> pushmeOpts)
    (fullDesc <> progDesc "" <> header pushmeSummary)

getOptions :: IO Options
getOptions = execParser optionsDefinition
