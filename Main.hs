{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Concurrent.ParallelIO
import           Control.Lens
import           Control.Monad hiding (sequence)
import           Data.Char
import           Data.Foldable
import           Data.Function
import           Data.Function.Pointless
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import           Data.Stringable as S hiding (fromText)
import qualified Data.Text as ST
import           Data.Text.Format
import           Data.Text.Lazy as T
import           Filesystem hiding (readFile)
import           Filesystem.Path.CurrentOS hiding (fromText, (</>))
import           GHC.Conc
import           Prelude hiding (FilePath, sequence, catch, putStrLn, readFile,
                                 print)
import           Shelly
import           System.Console.CmdArgs
import           System.Environment (getArgs, withArgs)
import           System.IO (stderr)
import           System.IO.Storage
import           System.Log.Formatter
import           System.Log.Handler (setFormatter)
import           System.Log.Handler.Simple (streamHandler)
import           System.Log.Logger
import           Text.Regex.Posix

default (Integer, Text)

version :: String
version = "0.1.0"

copyright :: String
copyright = "2012"

pushmeSummary :: String
pushmeSummary = "pushme v" ++ version ++ ", (C) John Wiegley " ++ copyright

data PushmeOpts = PushmeOpts { jobs      :: Int
                             , sendOnly  :: Bool
                             , dryRun    :: Bool
                             , quick     :: Bool
                             , verbose   :: Bool
                             , debug     :: Bool
                             , arguments :: [String] }
               deriving (Data, Typeable, Show, Eq)

pushmeOpts :: PushmeOpts
pushmeOpts = PushmeOpts
    { jobs      = def &= name "j" &= typ "INT"
                      &= help "Run INT concurrent finds at once (default: 2)"
    , sendOnly  = def &= name "S"
                      &= help "Do not create a new snapshot before sending"
    , dryRun    = def &= name "n"
                      &= help "Don't take any actions"
    , quick     = def &= name "q"
                      &= help "Avoid expensive operations"
    , verbose   = def &= name "v"
                      &= help "Report progress verbosely"
    , debug     = def &= name "D"
                      &= help "Report debug information"
    , arguments = def &= args &= typ "ARGS..." } &=
    summary pushmeSummary &=
    program "pushme" &=
    help "Synchronize data from one machine to another"

-- | A 'Fileset' is a data source.  It can be synced to a 'filesetStorePath'
--   within a ZFS 'Store', or synced to the same path in a non-ZFS 'Store'.

data Fileset = Fileset { _filesetName      :: Text
                       , _filesetPath      :: FilePath
                       , _filesetStorePath :: FilePath }
               deriving Show

-- | A 'Store' is a repository for holding 'Fileset' data.  Synchronization of
--   a remote store is done by updating the host's local store, and then copy
--   over the incremental snapshots.  If 'storeLocal' is @Just True@, it means
--   the store is always considered local to the machine it's running on.  If
--   it's @Just False@, it's always considered remote.  If it's @Nothing@,
--   locality is determined by compared the current hostnames to the
--   'storeHosts' value.  Note that 'storeHosts' refers to names used by SSH.

data Store = Store { _storeHost     :: [Text]
                   , _storeLocal    :: Maybe Bool
                   , _storePath     :: FilePath
                   , _storeZfsName  :: Maybe Text
                   , _storeLastRev  :: Int
                   , _storeLastSync :: Int }
             deriving Show

makeLenses ''Store

type FilesetMap = M.Map Text Fileset
type StoreMap   = M.Map Text Store

main :: IO ()
main = do
  mainArgs <- getArgs
  opts     <- withArgs (if L.null mainArgs then [] else mainArgs)
                       (cmdArgs pushmeOpts)
  procs    <- GHC.Conc.getNumProcessors
  _        <- GHC.Conc.setNumCapabilities $
              case jobs opts of 0 -> min procs 1; x -> x
  runPushme opts

runPushme :: PushmeOpts -> IO ()
runPushme opts = do
  let level = if debug opts
              then DEBUG
              else if verbose opts
                   then INFO
                   else NOTICE

  h <- streamHandler System.IO.stderr level >>= \lh ->
         return $ setFormatter lh (simpleLogFormatter "$time - [$prio] $msg")

  removeAllHandlers
  updateGlobalLogger "pushme" (setLevel level)
  updateGlobalLogger "pushme" (addHandler h)

  withStore "main" $ do
    putValue "main" "opts" opts

    when (dryRun opts) $
      warningM "pushme" "`--dryrun' specified, no changes will be made!"

    for_ (arguments opts) $ \host -> shelly $ do
      here <- thisHost
      let there = fromString host

      this <- identifyStore here
      that <- identifyStore there

      debugL $ here  <> " -> " <> fromString (show this)
      debugL $ there <> " -> " <> fromString (show that)

      case that^.storeZFS of
        Nothing ->
          if sendOnly opts || isLocal
            then warningL "Nothing to do"
            else copySystemRemotely that
        Just _ ->
          if sendOnly opts
            then sendSnapshots this that
            else do copySystemLocally this
                    this' <- createSnapshot this
                    sendSnapshots this' that

  stopGlobalPool

testme :: IO ()
testme = runPushme $ PushmeOpts { jobs      = 1
                                , sendOnly  = False
                                , dryRun    = True
                                , quick     = True
                                , verbose   = True
                                , debug     = False
                                , arguments = ["titan"] }

getOption :: Data a => (a -> b) -> Sh b
getOption option = do
  opts <- liftIO $ getValue "main" "opts"
  return $ fromJust $ option <$> opts

getHomePath :: Text -> IO FilePath
getHomePath p = getHomeDirectory >>= (return . (</> fromText p))

readStoreConfig :: FilePath -> IO (StoreMap, Store)
readStoreConfig path = undefined

readFilesetConfig :: FilePath -> IO FilesetMap
readFilesetConfig path = undefined

-- | Given a hostname (actually, a host name entry from @.ssh/config@), look
--   it up in @~/.pushme/config@ to find out what the store details are.
identifyStore :: Text -> Sh HostStore
identifyStore host = do
  store <- liftIO $ identifyStore' host
  host' <- thisHost
  let hstore = if host == host' || host == "backup" -- jww (2012-09-17): !
               then LocalStore store
               else RemoteStore host store
  lastRev <- if store^.storeZFS
             then latestSnapshot hstore
             else return (-1)
  let store' = storeRev .~ lastRev $ getStore hstore

  return $ if host == host'
           then localStore  .~ store' $ hstore
           else remoteStore .~ store' $ hstore

  where
    identifyStore' h = do
      hostsFile <- getHomePath ".pushme/config"
      found <- L.filter ((S.toText h ==) . L.head)
               . L.map ST.words . ST.lines
               <$> readTextFile hostsFile
      if L.null found
        then error $ toString $ "Unknown host: " <> h
        else do
          return Store
                 { _storePath = S.toFilePath $ getElem 1 found
                 , _storeName = S.toLazyText $ getElem 2 found
                 , _storeRev  = -1
                 , _storeZFS  = read . toString $ getElem 3 found }

    getElem n x = x ^. element 0 . element n

thisHost :: Sh Text
thisHost = silently $ do
  hostname <- cmd "hostname"
  return $ canonicalizeHost $ toString hostname

canonicalizeHost :: String -> Text
canonicalizeHost n
  | n =~ T.unpack "[Vv]ulcan" = "vulcan"
  | n =~ T.unpack "[Hh]ermes" = "hermes"
  | otherwise =
    error $ toString $ "Cannot determine canonical hostname: " <> n

remote :: Text -> [Text] -> Sh Text
remote host xs = do
  run "ssh" $ (host:xs)

-- | Determine the latest known snapshot in a local or remote store by
--   inspecting the .zfs/snapshot directory at the top of the store.
latestSnapshot :: HostStore -> Sh Int
latestSnapshot hstore = silently $ do
  let store = getStore hstore
      snapshots = fromFilePath $
                  (store^.storePath) </> fromText ".zfs/snapshot"
  listing <- T.lines <$>
    case hstore of
      LocalStore _ ->
        run "ls" ["-1", snapshots]
      RemoteStore host _ ->
        remote host ["ls", "-1", snapshots]

  let numericEntries = L.filter (T.all isDigit) listing
  if L.length numericEntries == 0
    then return (-1)
    else return $ read . toString . L.last $ numericEntries

doRun :: (FilePath -> [Text] -> Sh a)
      -> (Text -> Sh ())
      -> Sh a -> Bool -> Bool -> FilePath -> [Text]
      -> Sh a
doRun f g retval doLog heedDry n xs = do
  when doLog $
    g $ format "{} {}" [toTextIgnore n, T.unwords xs]

  dry <- getOption dryRun
  if dry && heedDry
    then retval
    else f n xs

drun :: Bool -> FilePath -> [Text] -> Sh Text
drun = doRun run debugL (return "") True

drun_ :: Bool -> FilePath -> [Text] -> Sh ()
drun_ x = void .: doRun run_ debugL (return ()) True x

vrun :: FilePath -> [Text] -> Sh Text
vrun = doRun run infoL (return "") True True

vrun_ :: FilePath -> [Text] -> Sh ()
vrun_ = void .: doRun run_ infoL (return ()) True True

srun :: FilePath -> [Text] -> Sh Text
srun = doRun run infoL (return "") False True

srun_ :: FilePath -> [Text] -> Sh ()
srun_ = void .: doRun run_ infoL (return ()) False True

createSnapshot :: HostStore -> Sh HostStore
createSnapshot store@(LocalStore this) =
  let nextRev = succ $ this^.storeRev
      thisSnapshot = (this^.storeName) <> "@" <> intToText nextRev
  in do noticeL $ format "Creating snapshot {}" [thisSnapshot]
        vrun_ "sudo" ["zfs", "snapshot", "-r", thisSnapshot]
        return $ localStore.storeRev .~ nextRev $ store

createSnapshot store@(RemoteStore {}) =
  error $ toString ("Will not create snapshot on remote store: " <> show store)

sendSnapshots :: HostStore -> HostStore -> Sh ()
sendSnapshots (LocalStore this) (LocalStore that) =
  doSendSnapshots this that (that^.storeName) $
    return ["sudo", "zfs", "recv", "-d", "-F", that^.storeName]

sendSnapshots (LocalStore this) (RemoteStore host that) = do
  doSendSnapshots this that (format "{}:{}" [host, that^.storeName]) $ do
    ssh <- which "ssh"
    return [ toTextIgnore (fromJust ssh)
           , host, "zfs", "recv", "-d", "-F", that^.storeName ]

sendSnapshots this that =
  terror $ format "Cannot send from {} to {}" [show this, show that]

doSendSnapshots :: Store -> Store -> Text -> Sh [Text] -> Sh ()
doSendSnapshots this that dest recvCmd =
  unless (this^.storeRev == that^.storeRev) $ escaping False $ do
    verb <- getOption verbose
    let thatSnapshot = (this^.storeName) <> "@" <> intToText (that^.storeRev)
        thisSnapshot = (this^.storeName) <> "@" <> intToText (this^.storeRev)
        sendCmd =
           if that^.storeRev == -1
          then do noticeL $ format "Sending {} → {}" [thisSnapshot, dest]
                  return ["sudo", "zfs", "send", thisSnapshot]
          else do noticeL $ format "Sending {} to {} → {}"
                                   [thatSnapshot, thisSnapshot, dest]
                  return $ ["sudo", "zfs", "send", "-R"]
                        <> (if verb then ["-v"] else [])
                        <> ["-I", thatSnapshot, thisSnapshot]

    -- jww (2012-09-18): There is a currently a bug preventing this from
    -- working.
    -- sendCmd -|- recvCmd

    sendArgs <- sendCmd
    recvArgs <- recvCmd
    vrun_ (fromText (L.head sendArgs)) $
      L.tail sendArgs
          <> ["|"]
          <> (if verb then ["pv", "|"] else [])
          <> recvArgs

intToText :: Int -> Text
intToText = fromString . show

debugL :: Text -> Sh ()
debugL = liftIO . debugM "pushme" . toString

infoL :: Text -> Sh ()
infoL = liftIO . infoM "pushme" . toString

noticeL :: Text -> Sh ()
noticeL = liftIO . noticeM "pushme" . toString

warningL :: Text -> Sh ()
warningL = liftIO . warningM "pushme" . toString

errorL :: Text -> Sh ()
errorL = error . toString
--errorL = liftIO . errorM "pushme" . toString

criticalL :: Text -> Sh ()
criticalL = liftIO . criticalM "pushme" . toString

------------------------------------------------------------------------------

coreSystemMappings :: Sh [(Text, FilePath, Text)]
coreSystemMappings = do
  homePath      <- liftIO $ getHomeDirectory
  messagesPath  <- liftIO $ getHomePath "Messages/"
  documentsPath <- liftIO $ getHomePath "Documents/"
  projectsPath  <- liftIO $ getHomePath "Projects/"

  let baseMappings =
           [("Documents", documentsPath, "Documents/")]
        <> [("Projects", projectsPath, "Projects/")]

  q <- getOption quick
  return $
    if q
    then baseMappings
    else baseMappings
         <> [("home", homePath <> fromText "/", "System/Home/")]
         <> [("Messages", messagesPath, "Messages/")]
         <> [ ("local", "/usr/local/", "System/Local/")
            , ("Library", "/Library/LaunchDaemons/",
               "System/Library/LaunchDaemons/")
            , ("opt", "/opt/", "System/Opt/")
            , ("Applications", "/Applications/",
               "System/Applications/") ]

copySystemLocally :: HostStore -> Sh ()
copySystemLocally this = do
  mappings <- coreSystemMappings
  liftIO $ parallel_ $
    L.map (\(x, y, z) ->
            systemVolCopy x y (genPath this z)) mappings

  where genPath st x =
          toTextIgnore $ (st^.localStore.storePath) </> fromText x

copySystemRemotely :: HostStore -> Sh ()
copySystemRemotely that = do
  mappings <- coreSystemMappings
  liftIO $ parallel_ $
    L.map (\(x, y, _) ->
            systemVolCopy x y (genPath that y)) mappings

  where genPath (RemoteStore host _) x =
          format "{}:{}" [host, toTextIgnore x]
        genPath _ _ = undefined

systemVolCopy :: Text -> FilePath -> Text -> IO ()
systemVolCopy label src dest = do
  optsFile <- liftIO $ getHomePath (".pushme/filters/" <> label)
  exists   <- isFile optsFile
  let rsyncOptions = if exists
                     then ["--include-from=" <> toTextIgnore optsFile]
                     else []
  shelly $
    volcopy True rsyncOptions (toTextIgnore src) dest

volcopy :: Bool -> [Text] -> Text -> Text -> Sh ()
volcopy useSudo options src dest = errExit False $ escaping False $ do
  infoL $ format "volcopy {} → {}" [src, dest]

  dry  <- getOption dryRun
  verb <- getOption verbose

  let toRemote = ":" `isInfixOf` dest
      options' =
        [ "-aHAXEy"
        , "--fileflags"
        , "--delete-after"
        , "--ignore-errors"
        , "--force-delete"

        , "--exclude='/.Caches/'"
        , "--exclude='/.Spotlight-V100/'"
        , "--exclude='/.TemporaryItems/'"
        , "--exclude='/.Trash/'"
        , "--exclude='/.Trashes/'"
        , "--exclude='/.fseventsd/'"
        , "--exclude='/Temporary Items/'"
        , "--exclude='/Network Trash Folder/'"

        , "--filter='-p .DS_Store'"
        , "--filter='-p .localized'"
        , "--filter='-p .AppleDouble/'"
        , "--filter='-p .AppleDB/'"
        , "--filter='-p .AppleDesktop/'"
        , "--filter='-p .com.apple.timemachine.supported'" ]

        <> (if dry then ["-n"] else [])
        <> (if verb then ["-P"] else ["-v"])
        <> options
        <> [src, dest]

  rsync <- which "rsync"
  doCopy (drun_ False) (fromJust rsync) toRemote useSudo options'

  where
    doCopy f rsync False True os  = f "sudo" (toTextIgnore rsync:os)
    doCopy f rsync False False os = f rsync os
    doCopy f rsync True True os   =
      f "sudo" [ "su", "-", "root", "-c"
               , "\"" <>
                 T.unwords (toTextIgnore rsync:remoteRsync rsync:os)
                 <> "\"" ]
    doCopy f rsync True False os  =
      f rsync (remoteRsync rsync:os)

    remoteRsync :: FilePath -> Text
    remoteRsync x = format "--rsync-path='sudo {}'" [toTextIgnore x]

-- Main.hs (pushme) ends here
