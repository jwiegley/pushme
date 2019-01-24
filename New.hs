{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Control.Lens hiding (Context, (.=))
-- import           Control.Monad
import           Control.Monad.Except
import           Data.Aeson hiding (Options)
import           Data.Aeson.TH
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import           Data.Maybe (isJust)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Yaml as Yaml
import           GHC.Generics
import qualified Text.Show.Pretty as PS

type HostName = String
type UserName = String
type SvcName = String

data Host services = Host
  { _hostAliases  :: Maybe [HostName]
  , _hostServices :: [services]
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

makeLenses ''Host

$(deriveJSON defaultOptions{fieldLabelModifier = drop 5, omitNothingFields = True} ''Host)

type Glob = String

data Conditions = Conditions
  { _condReceiveFrom :: Maybe [HostName]
  , _condSendTo      :: Maybe [HostName]
  , _condFilters     :: Maybe [Glob]
  }
  deriving (Eq, Show, Generic)

makeLenses ''Conditions

$(deriveJSON defaultOptions{fieldLabelModifier = drop 5, omitNothingFields = True} ''Conditions)

data Store media = Store
  { _storeConditions :: Maybe Conditions
  , _storeMedia      :: media
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

makeLenses ''Store

instance ToJSON media => ToJSON (Store media) where
  toJSON (Store conds media) =
    let Object v = toJSON media
        Object u = object [ "Conditions" .= conds | isJust conds ] in
    Object (H.union u v)

instance FromJSON media => FromJSON (Store media) where
  parseJSON (Object v) =
    Store
      <$> v .:? "Conditions"
      <*> parseJSON (Object (H.delete "Conditions" v))
  parseJSON _ = error "Error parsing Media"

type FsName = String

data Fileset media = Fileset
  { _fsPriority   :: Int
  , _fsClass      :: Maybe [String]
  , _fsConditions :: Maybe Conditions
  , _fsStores     :: Map HostName (Store media)
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

makeLenses ''Fileset

$(deriveJSON defaultOptions{fieldLabelModifier = drop 3, omitNothingFields = True} ''Fileset)

data Config services media = Config
  { _configHosts    :: Map HostName (Host services)
  , _configFilesets :: Map FsName (Fileset media)
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

makeLenses ''Config

$(deriveJSON defaultOptions{fieldLabelModifier = drop 7} ''Config)

{------------------------------------------------------------------------}

data NetService = NetService
  { _netHostName :: HostName
  , _netUser     :: Maybe UserName
  , _netPort     :: Maybe Int
  }
  deriving (Eq, Show, Generic)

makeLenses ''NetService

$(deriveJSON defaultOptions{fieldLabelModifier = drop 4, omitNothingFields = True} ''NetService)

data Services
  = Ssh NetService
  | UnknownService
  deriving (Eq, Show, Generic)

makePrisms ''Services

$(deriveJSON defaultOptions{allNullaryToStringTag = True, sumEncoding = ObjectWithSingleField} ''Services)

data ZfsMedia = ZfsMedia
  { _zfsRoot :: String
  , _zfsPath :: String
  }
  deriving (Eq, Show, Generic)

makeLenses ''ZfsMedia

$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''ZfsMedia)

data Media
    = Path FilePath
    | Zfs ZfsMedia
  deriving (Eq, Show, Generic)

makePrisms ''Media

$(deriveJSON defaultOptions{allNullaryToStringTag = True, sumEncoding = ObjectWithSingleField} ''Media)

data SyncError
  = NotReceivable
  | NotSendable

data Sync services media = Sync
  { _syncFileset      :: Fileset media
  , _syncFromHostName :: HostName
  , _syncFromHost     :: Host services
  , _syncToHostName   :: HostName
  , _syncToHost       :: Host services
  }
  deriving (Eq, Show, Generic)

data ReadySync services media = ReadySync
  { _knownSync      :: Sync services media
  , _knownFromStore :: Store media
  , _knownToStore   :: Store media
  }
  deriving (Eq, Show, Generic)

findHost :: Maybe HostName -> IO (HostName, Host s)
findHost = undefined

determineStores :: Sync s m -> ExceptT SyncError IO (ReadySync s m)
determineStores _s = undefined -- do
 --  unless (covered ln lh (rs^?fsConditions.traverse.condReceiveFrom)) $
 --    throwError NotReceivable

 --  unless (covered rn rh (ls^?fsConditions.traverse.condSendTo)) $
 --    throwError NotSendable

 --  return (ls^?!fsStores.ix ln, rs^?!fsStores.ix rn)
 -- where
 --  covered n h s = True `elem` do
 --    name <- n : fromMaybe [] (h^.hostAliases)
 --    pure $ case s of
 --      Just (Just xs) -> name `elem` xs
 --      _ -> True

data Stats = Stats
  { _stFiles  :: Int
  , _stData   :: Int
  , _stErrors :: Int
  }
  deriving (Eq, Show, Generic)

makeLenses ''Stats

rsync :: Host Services -> Store Media -> Host Services -> Store Media
      -> IO Stats
rsync _lh _ls _rh _rs = undefined

readDataFile :: FromJSON a => FilePath -> IO a
readDataFile p = do
    eres <- Yaml.decodeFileEither p
    case eres of
        Left err -> error $ "Failed to read file '" ++ p ++ "': " ++ show err
        Right x  -> return x

main :: IO ()
main = do
  putStrLn $ Text.unpack $ Text.decodeUtf8 $ Yaml.encode config

  PS.pPrint =<< (readDataFile "new.yml" :: IO (Config Services Media))
 where
  config :: Config Services Media
  config =
    Config
      (M.singleton "vulcan"
         (Host Nothing
            [Ssh (NetService "76.234.69.149"
                    (Just "johnw") (Just 2201))]))
      (M.singleton "Documents"
         (Fileset 12 Nothing Nothing
            (M.singleton "vulcan"
               (Store Nothing
                  (Zfs (ZfsMedia "tank" "/Users/johnw/Documents"))))))
