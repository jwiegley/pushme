{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.Applicative
import           Control.Concurrent.ParallelIO
import           Control.DeepSeq
import           Control.Exception
import           Control.Lens hiding (value)
import           Control.Monad
import           Data.Aeson hiding ((.:))
import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as BC
import           Data.Foldable hiding (elem, notElem)
import           Data.Function
import           Data.Function.Pointless
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import           Data.Stringable as S hiding (fromText)
import           Data.Text.Format
import           Data.Text.Lazy as T
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.Yaml hiding ((.:))
import           Debug.Trace as D
import           Filesystem
import           Filesystem.Path.CurrentOS hiding (fromText, (</>))
import           GHC.Conc
import           Prelude hiding (FilePath, catch)
import           Shelly
import           System.Console.CmdArgs
import           System.Environment (getArgs, withArgs)
import qualified System.FilePath.Glob as Glob
import           System.IO (stderr)
import           System.IO.Storage
import           System.Locale
import           System.Log.Formatter
import           System.Log.Handler (setFormatter)
import           System.Log.Handler.Simple (streamHandler)
import           System.Log.Logger
import           Text.Printf
import           Text.Regex.Posix

default (Integer, Text)

version :: String
version = "0.1.0"

copyright :: String
copyright = "2012"

pushmeSummary :: String
pushmeSummary = "pushme v" ++ version ++ ", (C) John Wiegley " ++ copyright

data PushmeOpts = PushmeOpts { jobs      :: Int
                             , dryRun    :: Bool
                             , snapshot  :: Bool
                             , stores    :: Bool
                             , filesets  :: String
                             , classes   :: String
                             , verbose   :: Bool
                             , quiet     :: Bool
                             , debug     :: Bool
                             , arguments :: [String] }
               deriving (Data, Typeable, Show, Eq)

pushmeOpts :: PushmeOpts
pushmeOpts = PushmeOpts
    { jobs      = def &= name "j" &= typ "INT"
                      &= help "Run INT concurrent finds at once (default: 2)"
    , dryRun    = def &= name "n"
                      &= help "Don't take any actions"
    , snapshot  = def &= name "s"
                      &= help "Don't sync to an rsync-zfs store, just snapshot"
    , stores    = def &= help "Show all the stores know to pushme"
    , filesets  = def &= name "f"
                      &= help "Synchronize the given fileset(s) (comma-sep)"
    , classes   = def &= name "c"
                      &= help "Filesets classes to synchronize (comma-sep)"
    , verbose   = def &= name "v"
                      &= help "Report progress verbosely"
    , quiet     = def &= name "q"
                      &= help "Be a little quieter"
    , debug     = def &= name "D"
                      &= help "Report debug information"
    , arguments = def &= args &= typ "ARGS..." }

    &= summary pushmeSummary
    &= program "pushme"
    &= help "Synchronize data from one machine to another"

-- | A 'Fileset' is a data source.  It can be synced to a 'filesetStorePath'
--   within a ZFS 'Store', or synced to the same path in a non-ZFS 'Store'.

data Fileset = Fileset { _filesetName          :: Text
                       , _filesetPriority      :: Int
                       , _filesetClass         :: Text
                       , _filesetPath          :: FilePath
                       , _filesetReportMissing :: Bool
                       , _filesetZFSOnly       :: Bool
                       , _filesetStorePath     :: Maybe FilePath }
               deriving Show

makeLenses ''Fileset

instance FromJSON FilePath where
  parseJSON = parseJSON >=> return . fromText

instance ToJSON FilePath where
  toJSON = toJSON . toTextIgnore

$(deriveJSON (L.drop 8) ''Fileset)

-- | A 'Store' is a repository for holding 'Fileset' data.  Synchronization of
--   a remote store is done by updating the host's local store, and then copy
--   over the incremental snapshots.  If 'storeIsLocal' is @Just True@, it means
--   the store is always considered local to the machine it's running on.  If
--   it's @Just False@, it's always considered remote.  If it's @Nothing@,
--   locality is determined by compared the current hostnames to the
--   'storeHosts' value.  Note that 'storeHosts' refers to names used by SSH.

data Store = Store { _storeHostRe      :: Text
                   , _storeSelfRe      :: Text
                   , _storeUserName    :: Text
                   , _storeHostName    :: Text -- set on load from command-line
                   , _storeIsLocal     :: Bool -- set on load
                   , _storeAlwaysLocal :: Bool
                   , _storeType        :: Text
                   , _storeZFSName     :: Text
                   , _storePath        :: FilePath
                   , _storeLastRev     :: Int
                   , _storeLastSync    :: LocalTime }
             deriving (Eq, Show)

makeLenses ''Store

instance FromJSON LocalTime where
  parseJSON =
    parseJSON >=> return . readTime defaultTimeLocale "%Y%m%dT%H%M%S"

instance ToJSON LocalTime where
  toJSON = toJSON . formatTime defaultTimeLocale "%Y%m%dT%H%M%S"

$(deriveJSON (L.drop 6) ''Store)

instance NFData Store where
  rnf a = a `seq` ()

data PushmeException = PushmeException deriving (Show, Typeable)

instance Exception PushmeException

main :: IO ()
main = do
  mainArgs <- getArgs
  opts     <- withArgs (if L.null mainArgs then [] else mainArgs)
                       (cmdArgs pushmeOpts)
  procs    <- GHC.Conc.getNumProcessors
  _        <- GHC.Conc.setNumCapabilities $
              case jobs opts of 0 -> min procs 1; x -> x

  (runPushme opts) `catch` \(_ :: PushmeException) -> return ()
                   `catch` \(ex :: SomeException)  -> error (show ex)

testme :: IO ()
testme = runPushme
         PushmeOpts { jobs      = 1
                    , dryRun    = True
                    , snapshot  = False
                    , stores    = False
                    , filesets  = ""
                    , classes   = ""
                    , verbose   = True
                    , quiet     = True
                    , debug     = False
                    , arguments = ["titan"] }

runPushme :: PushmeOpts -> IO ()
runPushme opts = do
  let level
        | debug opts   = DEBUG
        | verbose opts = INFO
        | otherwise    = NOTICE
  h <- (`setFormatter` simpleLogFormatter "$time - [$prio] $msg")
       <$> streamHandler System.IO.stderr level

  removeAllHandlers
  updateGlobalLogger "pushme" (setLevel level)
  updateGlobalLogger "pushme" (addHandler h)

  withStore "main" $ do
    putValue "main" "opts" opts

    when (dryRun opts) $
      warningM "pushme" "`--dryrun' specified, no changes will be made!"

    sts   <- L.map ((storeIsLocal .~ False) . (storeHostName .~ ""))
             <$> readDataFile ".pushme/stores.yaml" :: IO [Store]
    fsets <- readDataFile ".pushme/filesets.yaml"     :: IO [Fileset]

    if stores opts
      then
        for_ sts $ \st -> shelly $ do
          echo $ fromString $
            printf (T.unpack "%-20s%5d%30s")
                   (toString (st^.storeHostRe))
                   (st^.storeLastRev)
                   (formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S"
                               (st^.storeLastSync))

      else do
        (_, sts'') <- shelly $ silently $ do
          here <- cmd "hostname"
          let (this, sts') = identifyStore here True True sts

          debugL $ here <> " -> " <> fromString (show this)

          L.foldl' (processHost fsets) (return (this, sts'))
                   (arguments opts)

        unless (dryRun opts) $
          writeDataFile ".pushme/stores.yaml" sts''

  stopGlobalPool

processHost :: [Fileset] -> Sh (Store, [Store]) -> String -> Sh (Store, [Store])
processHost fsets storesInSh host = do
  (this, sts) <- storesInSh
  let there = fromString host
      (that, sts') =
        identifyStore there False
                      (matchText (this^.storeHostRe) there) sts

  noticeL $ "Synchronizing " <> there
  debugL $ (this^.storeHostRe) <> " -> " <> fromString (show this)
  debugL $ there <> " -> " <> fromString (show that)

  snapshotOnly <- getOption snapshot

  that' <-
    case that^.storeType of
      "rsync"     -> syncFilesets that fsets
      "rsync-zfs" -> if snapshotOnly
                     then createSnapshot that
                     else syncFilesets that fsets >>= createSnapshot
      "zfs"       -> sendSnapshots this that

      _ -> error $ "Unexpected store type: " ++ show that

  debugL $ there <> " is now -> " <> fromString (show that')

  return ( if matchStores this that' then that' else this
         , replaceStore that that' sts' )

  where matchStores       = (==) `on` (^.storeHostRe)
        replaceStore      = replaceElem matchStores
        replaceElem f x y = L.map (\z -> if f x z then y else z)

identifyStore :: Text -> Bool -> Bool -> [Store] -> (Store, [Store])
identifyStore hostname isSelf isLocal sts =
  let (st', sts') =
        L.foldr
          (\st (x, xs) ->
            if matchText (if isSelf
                          then st^.storeSelfRe
                          else st^.storeHostRe) hostname
            then let y = modStore (st^.storeAlwaysLocal || isLocal) $
                         storeHostName .~ hostname $ st
                 in (Just y, y:xs)
            else (x, modStore (st^.storeAlwaysLocal) st:xs))
          (Nothing, []) sts
  in case st' of
    Nothing -> error $ toString $
                format "Could not find hostname {} in ~/.pushme/stores"
                       [hostname]
    Just x  -> (x, sts')

  where modStore = (storeIsLocal .~)

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

sendSnapshots :: Store -> Store -> Sh Store
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
    error $ toString $
    format "Cannot send from {} to {}" [show this, show that]

doSendSnapshots :: Store -> Store -> Text -> Sh [Text] -> Sh Store
doSendSnapshots this that dest recvCmd
  | this^.storeLastRev == that^.storeLastRev = return that

  | otherwise = escaping False $ do
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
                        <> ["-v" | verb]
                        <> ["-I", thatSnapshot, thisSnapshot]

    -- sendCmd -|- recvCmd
    sendArgs <- sendCmd
    recvArgs <- recvCmd
    vrun_ (fromText (L.head sendArgs)) $
      L.tail sendArgs <> ["|"] <> (if verb then ["pv", "|"] else [])
                      <> recvArgs

    now <- liftIO currentLocalTime
    return $ storeLastSync .~ now
           $ storeLastRev  .~ (this^.storeLastRev) $ that

syncFilesets :: Store -> [Fileset] -> Sh Store
syncFilesets that fsets = do
  liftIO $ parallel_ $
    L.map (\fs ->
            unless (((that^.storeType) == "rsync-zfs"
                     && isNothing (fs^.filesetStorePath))
                    || ((that^.storeType) == "rsync"
                        && (fs^.filesetZFSOnly))) $ shelly $ do
              fss <- fromString <$> getOption filesets
              when (T.null fss
                    || matchText fss (fs^.filesetName)) $ do
                cls <- fromString <$> getOption classes
                when (T.null cls
                      || matchText cls (fs^.filesetClass)) $ do
                  when (fs^.filesetReportMissing) $
                    reportMissingFiles fs
                  systemVolCopy (fs^.filesetName) (fs^.filesetPath)
                                (genPath that fs))
          (L.sortBy (compare `on` (^.filesetPriority)) fsets)

  now <- liftIO currentLocalTime
  return $ storeLastSync .~ now $ that

genPath :: Store -> Fileset -> Text
genPath that fs =
  case that^.storeType of
    "rsync-zfs" ->
      toTextIgnore ((that^.storePath) </> fromJust (fs^.filesetStorePath))

    "rsync" ->
      if that^.storeIsLocal
      then toTextIgnore ((that^.storePath) </> fromJust (fs^.filesetStorePath))
      else format "{}@{}:{}" [ that^.storeUserName
                             , that^.storeHostName
                             , escape (toTextIgnore (fs^.filesetPath)) ]

    _ -> error $ "Unexpected: genPath: " ++ show that

  where escape x = if "\"" `isInfixOf` x || " " `isInfixOf` x
                   then "'" <> T.replace "\"" "\\\"" x <> "'"
                   else x

reportMissingFiles :: Fileset -> Sh ()
reportMissingFiles fs = do
  files    <- L.map (T.drop (T.length (toTextIgnore (fs^.filesetPath)))
                     . toTextIgnore)
              <$> ls (fs^.filesetPath)
  optsFile <- liftIO $ getHomePath (".pushme/filters/" <> fs^.filesetName)
  exists   <- liftIO $ isFile optsFile
  when exists $ do
    optsText <- readfile optsFile
    let stringify    = (\x -> if x !! 0 == '/' then L.tail x else x)
                       . (\x -> if x !! (L.length x - 1) == '/'
                                then L.init x else x)
                       . T.unpack . T.drop 2
        patterns     = L.map Glob.compile
                       . L.filter (\x -> x /= "*" && x /= "*/"
                                      && x /= ".*" && x /= ".*/")
                       . L.map stringify . T.lines $ optsText
        patMatch f p = Glob.match p f
        files'       =
          L.foldl' (\acc x ->
                     if L.any (patMatch x) patterns
                     then acc
                     else (x:acc)) []
                   (L.map toString
                    . L.filter (`notElem` [ ".DS_Store", ".localized" ])
                    $ files)
    traverse_ (\x -> warningL $ format "Unknown path: \"{}\"" [x]) files'

systemVolCopy :: Text -> FilePath -> Text -> Sh ()
systemVolCopy label src dest = do
  optsFile <- liftIO $ getHomePath (".pushme/filters/" <> label)
  exists   <- liftIO $ isFile optsFile
  let rsyncOptions = ["--include-from=" <> toTextIgnore optsFile | exists]
  volcopy True rsyncOptions (toTextIgnore src) dest

volcopy :: Bool -> [Text] -> Text -> Text -> Sh ()
volcopy useSudo options src dest = errExit False $ do
  noticeL $ format "{} → {}" [src, dest]

  dry  <- getOption dryRun
  deb  <- getOption debug
  verb <- getOption verbose

  let shhh     = not deb && not verb
      toRemote = ":" `isInfixOf` dest
      options' =
        [ "-aHXEy"              -- jww (2012-09-23): maybe -A too?
        , "--fileflags"
        , "--delete-after"
        , "--ignore-errors"
        , "--force-delete"

        , "--exclude=/.Caches/"
        , "--exclude=/.Spotlight-V100/"
        , "--exclude=/.TemporaryItems/"
        , "--exclude=/.Trash/"
        , "--exclude=/.Trashes/"
        , "--exclude=/.fseventsd/"
        , "--exclude=/.zfs/"
        , "--exclude=/Temporary Items/"
        , "--exclude=/Network Trash Folder/"

        , "--filter=-p .DS_Store"
        , "--filter=-p .localized"
        , "--filter=-p .AppleDouble/"
        , "--filter=-p .AppleDB/"
        , "--filter=-p .AppleDesktop/"
        , "--filter=-p .com.apple.timemachine.supported" ]

        <> ["-n" | dry]
        <> (if shhh
            then ["--stats"]
            else if verb
                 then ["-P"]
                 else if deb
                      then ["-v", "-v"]
                      else ["-v"])
        <> options
        <> [src, dest]

  rsync <- which "rsync"
  case rsync of
    Nothing -> error "Could not find rsync!"
    Just r  ->
      if shhh
      then silently $ do
        output <- doCopy (drun False) r toRemote useSudo options'
        let stats = M.fromList $
                    L.map (liftM2 (,) L.head (L.head . T.words . L.last)
                           . T.splitOn ": ")
                    . L.filter (": " `isInfixOf`)
                    . T.lines $ output
            files = field "Number of files" stats
            sent  = field "Number of files transferred" stats
            total = field "Total file size" stats
            xfer  = field "Total transferred file size" stats
        noticeL $ format "Sent {} in {} files (out of {} in {})"
                         [ fromString (humanReadable xfer),
                           commaSep (fromIntegral sent)
                         , fromString (humanReadable total),
                           commaSep (fromIntegral files) ]
      else
        doCopy (drun_ False) r toRemote useSudo options'

  where
    commaSep :: Int -> Text
    commaSep = fst . T.foldr (\x (xs, num) ->
                               if num /= 0 && num `mod` 3 == 0
                               then (x `cons` ',' `cons` xs, num + 1)
                               else (x `cons` xs, num + 1))
                             ("", 0)
                   . intToText
    field x stats = read (toString (stats M.! x)) :: Integer

    doCopy f rsync False False os = f rsync os
    doCopy f rsync False True os  = sudo f rsync os
    doCopy f rsync True False os  = f rsync (remoteRsync rsync:os)
    doCopy f rsync True True os   = asRoot f rsync (remoteRsync rsync:os)

    remoteRsync x = format "--rsync-path=sudo {}" [toTextIgnore x]

getHomePath :: Text -> IO FilePath
getHomePath p = (</> fromText p) <$> getHomeDirectory

convertPath :: FilePath -> String
convertPath = toString

readDataFile :: FromJSON a => Text -> IO [a]
readDataFile p = do
  p' <- getHomePath p
  d  <- Data.Yaml.decode <$> BC.readFile (convertPath p')
  case d of
    Nothing -> error $ toString $ "Failed to read file " <> p
    Just d' -> return d'

writeDataFile :: ToJSON a => Text -> a -> IO ()
writeDataFile p xs =
  getHomePath p >>= flip BC.writeFile (Data.Yaml.encode xs) . convertPath

currentLocalTime :: IO LocalTime
currentLocalTime = do
  tz <- getCurrentTimeZone
  tm <- getCurrentTime
  return $ utcToLocalTime tz tm

getOption' :: Data a => (a -> b) -> IO b
getOption' option = do
  opts <- getValue "main" "opts"
  return $ fromJust $ option <$> opts

getOption :: Data a => (a -> b) -> Sh b
getOption = liftIO . getOption'

matchText :: Text -> Text -> Bool
matchText = flip ((=~) `on` toString)

intToText :: Int -> Text
intToText = fromString . show

remote :: (FilePath -> [Text] -> Sh a) -> Text -> [Text] -> Sh a
remote f host xs = f "ssh" (host:xs)

asRoot :: (FilePath -> [Text] -> Sh a) -> FilePath -> [Text] -> Sh a
asRoot f p xs =
  f "sudo" [ "su", "-", "root", "-c"
           , T.unwords (L.map (\x -> "\"" <> x <> "\"") xs') ]
  where xs' = toTextIgnore p:xs

sudo :: (FilePath -> [Text] -> Sh a) -> FilePath -> [Text] -> Sh a
sudo f p xs = f "sudo" (toTextIgnore p:xs)

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

debugL :: Text -> Sh ()
debugL = liftIO . debugM "pushme" . toString

infoL :: Text -> Sh ()
infoL = liftIO . infoM "pushme" . toString

noticeL :: Text -> Sh ()
noticeL = liftIO . noticeM "pushme" . toString

warningL :: Text -> Sh ()
warningL = liftIO . warningM "pushme" . toString

errorL :: Text -> Sh ()
errorL = liftIO . errorM "pushme" . toString

criticalL :: Text -> Sh ()
criticalL = liftIO . criticalM "pushme" . toString

humanReadable :: Integer -> String
humanReadable x
  | x < 1024   = printf "%db" x
  | x < 1024^2 = printf "%.0fK" (fromIntegral x / (1024 :: Double))
  | x < 1024^3 = printf "%.1fM" (fromIntegral x / (1024^2 :: Double))
  | x < 1024^4 = printf "%.2fG" (fromIntegral x / (1024^3 :: Double))
  | x < 1024^5 = printf "%.3fT" (fromIntegral x / (1024^4 :: Double))
  | x < 1024^6 = printf "%.3fP" (fromIntegral x / (1024^5 :: Double))
  | x < 1024^7 = printf "%.3fX" (fromIntegral x / (1024^6 :: Double))
  | otherwise  = printf "%db" x

-- Main.hs (pushme) ends here
