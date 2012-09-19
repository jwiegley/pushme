{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Concurrent.ParallelIO
import           Control.Lens
import           Control.Monad hiding (sequence)
import           Data.Char
import           Data.Function
import           Data.Function.Pointless
import qualified Data.List as L
import           Data.Maybe
import           Data.Monoid
import           Data.Stringable as S hiding (fromText)
import           Data.Text.Format
import           Data.Text.Lazy as T
import           Data.Text.Lazy.IO as TIO
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime
import           Filesystem hiding (readFile)
import           Filesystem.Path.CurrentOS hiding (fromText, (</>))
import           GHC.Conc
import           Prelude hiding (FilePath, sequence, catch, putStrLn,
                                 readFile, print)
import           Shelly
import           System.Console.CmdArgs
import           System.Environment (getArgs, withArgs)
import           System.IO (stderr)
import           System.IO.Storage
import           System.Locale
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
                             , filesets  :: String
                             , classes   :: String
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
    , filesets  = def &= name "f"
                      &= help "Synchronize the given fileset(s) (comma-sep)"
    , classes   = def &= name "c"
                      &= help "Filesets classes to synchronize (comma-sep)"
    , verbose   = def &= name "v"
                      &= help "Report progress verbosely"
    , debug     = def &= name "D"
                      &= help "Report debug information"
    , arguments = def &= args &= typ "ARGS..." } &=
    summary pushmeSummary &=
    program "pushme" &=
    help "Synchronize data from one machine to another"

currentLocalTime :: IO LocalTime
currentLocalTime = do
  tz <- getCurrentTimeZone
  tm <- getCurrentTime
  return $ utcToLocalTime tz tm

writeList :: (a -> Text) -> FilePath -> [a] -> IO ()
writeList f p =
  (TIO.writeFile . toString . toTextIgnore $ p) . listToText
  where listToText = T.unlines . L.map f

readList :: (Text -> a) -> FilePath -> IO [a]
readList f p = (TIO.readFile . toString . toTextIgnore $ p)
                  >>= return . textToList
  where textToList = L.map f . T.lines

-- | A 'Fileset' is a data source.  It can be synced to a 'filesetStorePath'
--   within a ZFS 'Store', or synced to the same path in a non-ZFS 'Store'.

data Fileset = Fileset { _filesetName      :: Text
                       , _filesetPriority  :: Int
                       , _filesetClass     :: Text
                       , _filesetPath      :: FilePath
                       , _filesetStorePath :: Maybe FilePath }
               deriving Show

makeLenses ''Fileset

filesetToText :: Fileset -> Text
filesetToText x =
  format "{}:{}:{}:{}:{}"
         [ x^.filesetName
         , (T.pack . show) (x^.filesetPriority)
         , x^.filesetClass
         , toTextIgnore (x^.filesetPath)
         , (T.pack . show) (toTextIgnore <$> (x^.filesetStorePath)) ]

textToFileset :: Text -> Fileset
textToFileset x =
  Fileset { _filesetName      = y^.element 0
          , _filesetPriority  = (read . T.unpack) (y^.element 1)
          , _filesetClass     = y^.element 2
          , _filesetPath      = fromText (y^.element 3)
          , _filesetStorePath =
               fromText <$> ((read . T.unpack) (y^.element 4) :: Maybe Text) }
  where y = splitOn ":" x

doReadFilesets :: FilePath -> IO [Fileset]
doReadFilesets = readList textToFileset

doWriteFilesets :: FilePath -> [Fileset] -> IO ()
doWriteFilesets = writeList filesetToText

getHomePath :: Text -> IO FilePath
getHomePath p = getHomeDirectory >>= (return . (</> fromText p))

readFilesetsFile :: Text -> IO [Fileset]
readFilesetsFile = getHomePath >=> doReadFilesets

writeFilesetsFile :: Text -> [Fileset] -> IO ()
writeFilesetsFile p xs = getHomePath p >>= flip doWriteFilesets xs

-- | A 'Store' is a repository for holding 'Fileset' data.  Synchronization of
--   a remote store is done by updating the host's local store, and then copy
--   over the incremental snapshots.  If 'storeIsLocal' is @Just True@, it means
--   the store is always considered local to the machine it's running on.  If
--   it's @Just False@, it's always considered remote.  If it's @Nothing@,
--   locality is determined by compared the current hostnames to the
--   'storeHosts' value.  Note that 'storeHosts' refers to names used by SSH.

data Store = Store { _storeHostRe      :: Text
                   , _storeUserName    :: Text
                   , _storeHostName    :: Text -- set on load from command-line
                   , _storeIsLocal     :: Bool -- set on load
                   , _storeAlwaysLocal :: Bool
                   , _storeIsZFS       :: Bool
                   , _storeZFSName     :: Text
                   , _storePath        :: FilePath
                   , _storeLastRev     :: Int
                   , _storeLastSync    :: LocalTime }
             deriving (Eq, Show)

makeLenses ''Store

storeToText :: Store -> Text
storeToText x =
  format "{}:{}:{}:{}:{}:{}:{}:{}"
         [ (x^.storeHostRe)
         , (x^.storeUserName)
         , (T.pack . show) (x^.storeAlwaysLocal)
         , (T.pack . show) (x^.storeIsZFS)
         , (x^.storeZFSName)
         , toTextIgnore (x^.storePath)
         , (T.pack . show) (x^.storeLastRev)
         , (T.pack . formatTime defaultTimeLocale "%Y%m%dT%H%M%S")
           (x^.storeLastSync) ]

textToStore :: Text -> Store
textToStore x =
  Store { _storeHostRe      = (y^.element 0)
        , _storeUserName    = (y^.element 1)
        , _storeHostName    = T.empty
        , _storeIsLocal     = False
        , _storeAlwaysLocal = (read . T.unpack) (y^.element 2)
        , _storeIsZFS       = (read . T.unpack) (y^.element 3)
        , _storeZFSName     = (y^.element 4)
        , _storePath        = fromText (y^.element 5)
        , _storeLastRev     = (read . T.unpack) (y^.element 6)
        , _storeLastSync    =
          (readTime defaultTimeLocale "%Y%m%dT%H%M%S" . T.unpack)
          (y^.element 7) }
  where y = splitOn ":" x

doReadStores :: FilePath -> IO [Store]
doReadStores = readList textToStore

doWriteStores :: FilePath -> [Store] -> IO ()
doWriteStores = writeList storeToText

readStoresFile :: Text -> IO [Store]
readStoresFile = getHomePath >=> doReadStores

writeStoresFile :: Text -> [Store] -> IO ()
writeStoresFile p xs = getHomePath p >>= flip doWriteStores xs

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

    stores <- readStoresFile ".pushme/stores"
    fsets  <- readFilesetsFile ".pushme/filesets"

    stores'' <- shelly $ do
      here <- cmd "hostname"
      let (this, stores') = identifyStore here True stores

      debugL $ here <> " -> " <> fromString (show this)

      L.foldl' (processHost this fsets) (return stores')
        (arguments opts)

    writeStoresFile ".pushme/stores" stores''

  stopGlobalPool

  where
    processHost this fsets storesInSh host = do
      stores <- storesInSh

      let there = fromString host
          (that, stores') =
            identifyStore there (matchText (this^.storeHostRe)
                                           (fromString host)) stores

      debugL $ there <> " -> " <> fromString (show that)

      unless (sendOnly opts) $ syncFilesets this that fsets

      echo $ this^.storeHostName
      echo $ that^.storeHostName

      let thisIsThat = (this^.storeHostRe) == (that^.storeHostRe)
      if that^.storeIsZFS
        then if sendOnly opts
             then do
               unless thisIsThat $ sendSnapshots this that
               return stores'
             else do
               this' <- createSnapshot this
               unless thisIsThat $ sendSnapshots this' that
               return $ L.map (\st ->
                                if ((==) `on` (^.storeHostName)) st this
                                then this'
                                else st)
                              stores'
        else return stores'

testme :: IO ()
testme = runPushme $ PushmeOpts { jobs      = 1
                                , sendOnly  = False
                                , dryRun    = True
                                , quick     = True
                                , filesets  = ""
                                , classes   = ""
                                , verbose   = True
                                , debug     = False
                                , arguments = ["titan"] }

getOption :: Data a => (a -> b) -> Sh b
getOption option = do
  opts <- liftIO $ getValue "main" "opts"
  return $ fromJust $ option <$> opts

getOption' :: Data a => (a -> b) -> IO b
getOption' option = do
  opts <- getValue "main" "opts"
  return $ fromJust $ option <$> opts

matchText :: Text -> Text -> Bool
matchText x y = (toString y) =~ (toString x)

identifyStore :: Text -> Bool -> [Store] -> (Store, [Store])
identifyStore hostname isLocal stores =
  let (st', stores') =
        L.foldr
          (\st (x, xs) ->
            if matchText (st^.storeHostRe) hostname
            then let y = modStore isLocal $
                         storeHostName .~ hostname $ st
                 in (Just y, (y:xs))
            else (x, (modStore False st:xs)))
          (Nothing, []) stores
  in case st' of
    Nothing -> error $ toString $
                format "Could not find hostname {} in ~/.pushme/stores"
                       [hostname]
    Just x  -> (x, stores')

  where modStore local = storeIsLocal .~ local

remote :: Text -> [Text] -> Sh Text
remote host xs = do
  run "ssh" $ (host:xs)

-- | Determine the latest known snapshot in a local or remote store by
--   inspecting the .zfs/snapshot directory at the top of the store.
getLatestSnapshot :: Store -> Sh Int
getLatestSnapshot store = silently $ do
  let snapshots = fromFilePath $
                  (store^.storePath) </> fromText ".zfs/snapshot"
  listing <- T.lines <$> if store^.storeIsLocal
                         then run "ls" ["-1", snapshots]
                         else remote (store^.storeHostName)
                                     ["ls", "-1", snapshots]

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

createSnapshot :: Store -> Sh Store
createSnapshot this
  | this^.storeIsLocal = do
    let nextRev      = succ (this^.storeLastRev)
        thisSnapshot = (this^.storeZFSName) <> "@" <> intToText nextRev
    noticeL $ format "Creating snapshot {}" [thisSnapshot]
    vrun_ "sudo" ["zfs", "snapshot", "-r", thisSnapshot]
    return $ storeLastRev .~ nextRev $ this

  | otherwise =
    error $ toString $
      "Will not create snapshot on remote store: " <> show this

sendSnapshots :: Store -> Store -> Sh ()
sendSnapshots this that
  | (this^.storeIsLocal) && (that^.storeIsLocal) =
    doSendSnapshots this that (that^.storeZFSName) $
      return ["sudo", "zfs", "recv", "-d", "-F", that^.storeZFSName]

  | (this^.storeIsLocal) && not (that^.storeIsLocal) =
    doSendSnapshots this that (format "{}@{}:{}"
                    [ that^.storeUserName
                    , that^.storeHostName
                    , that^.storeZFSName ]) $ do
      ssh <- which "ssh"
      return [ toTextIgnore (fromJust ssh)
             , that^.storeHostName, "zfs", "recv", "-d", "-F"
             , that^.storeZFSName ]

  | otherwise =
    terror $ format "Cannot send from {} to {}" [show this, show that]

doSendSnapshots :: Store -> Store -> Text -> Sh [Text] -> Sh ()
doSendSnapshots this that dest recvCmd =
  unless (this^.storeLastRev == that^.storeLastRev) $ escaping False $ do
    verb <- getOption verbose
    let thatSnapshot =
          (this^.storeZFSName) <> "@" <> intToText (that^.storeLastRev)
        thisSnapshot =
          (this^.storeZFSName) <> "@" <> intToText (this^.storeLastRev)
        sendCmd =
           if that^.storeLastRev == -1
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

syncFilesets :: Store -> Store -> [Fileset] -> Sh ()
syncFilesets this that fsets = do
  liftIO $ parallel_ $
    L.map (\fs -> do
              fss <- fromString <$> getOption' filesets
              when (T.null fss || (fs^.filesetName) `isInfixOf` fss) $ do
                cls <- fromString <$> getOption' classes
                when (T.null cls || (fs^.filesetClass) `isInfixOf` cls) $ do
                  q <- getOption' quick
                  when (not q || (fs^.filesetClass) == "quick") $
                    systemVolCopy (fs^.filesetName) (fs^.filesetPath)
                                  (genPath fs))
          (L.sortBy (compare `on` (^.filesetPriority)) fsets)

  where genPath fs =
          if (this^.storeIsZFS) && (that^.storeIsZFS)
          then (toTextIgnore (this^.storePath)) <> "/"
               <> (toTextIgnore . fromJust) (fs^.filesetStorePath)
          else if that^.storeIsLocal
               then error $ "Unexpected: " ++ show that
               else format "{}@{}:{}"
                           [ that^.storeUserName
                           , that^.storeHostName
                           , toTextIgnore $ fs^.filesetPath ]

systemVolCopy :: Text -> FilePath -> Text -> IO ()
systemVolCopy label src dest = do
  optsFile <- liftIO $ getHomePath (".pushme/filters/" <> label)
  exists   <- isFile optsFile
  let rsyncOptions = if exists
                     then ["--include-from=" <> toTextIgnore optsFile]
                     else []
  shelly $ volcopy True rsyncOptions (toTextIgnore src) dest

volcopy :: Bool -> [Text] -> Text -> Text -> Sh ()
volcopy useSudo options src dest = errExit False $ escaping False $ do
  noticeL $ format "volcopy {} → {}" [src, dest]

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
