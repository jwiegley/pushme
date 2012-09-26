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
import           Data.Char
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

instance FromJSON LocalTime where
  parseJSON =
    parseJSON >=> return . readTime defaultTimeLocale "%Y%m%dT%H%M%S"

instance ToJSON LocalTime where
  toJSON = toJSON . formatTime defaultTimeLocale "%Y%m%dT%H%M%S"

instance FromJSON FilePath where
  parseJSON = parseJSON >=> return . fromText

instance ToJSON FilePath where
  toJSON = toJSON . toTextIgnore

version :: String
version = "0.1.0"

copyright :: String
copyright = "2012"

pushmeSummary :: String
pushmeSummary = "pushme v" ++ version ++ ", (C) John Wiegley " ++ copyright

data PushmeOpts = PushmeOpts { jobs      :: Int
                             , dryRun    :: Bool
                             , noSync    :: Bool
                             , loadRevs  :: Bool
                             , stores    :: Bool
                             , ssh       :: String
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
    , noSync    = def &= name "N"
                      &= help "Don't even attempt a dry-run sync"
    , loadRevs  = def &= name "L"
                      &= help "Load latest snapshot revs from disk"
    , stores    = def &= help "Show all the stores know to pushme"
    , ssh       = def &= help "Use a specific ssh command"
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

-- | A 'Fileset' is a logical grouping of files, with an assigned class and
--   priority.  It may also have an associated filter.

data Fileset = Fileset { _filesetName          :: Text
                       , _filesetClass         :: Text
                       , _filesetPriority      :: Int
                       , _filesetReportMissing :: Bool }
               deriving Show

makeLenses ''Fileset

$(deriveJSON (L.drop 8) ''Fileset)

-- | A 'Container' is a physical grouping of files, reflecting an instance of
--   a 'Fileset' somewhere on a storage medium.  For every 'Fileset', there
--   are many 'Container' instances.  When a fileset is synchronized, it means
--   copying it by some means between different containers.

data Container = Container { _containerFileset  :: Text
                           , _containerStore    :: Text
                           , _containerPath     :: FilePath
                           , _containerPoolPath :: Maybe FilePath
                           , _containerRecurse  :: Bool
                           , _containerLastRev  :: Maybe Int
                           , _containerLastSync :: Maybe LocalTime }
               deriving Show

makeLenses ''Container

$(deriveJSON (L.drop 10) ''Container)

instance NFData Container where
  rnf a = a `seq` ()

-- | A 'Store' is a repository that holds 'Container' instances in some form
--   or another.  Synchronization of a remote ZFS container from a local ZFS
--   container is done by copying over incremental snapshots.  Note that
--   'storeHostName' refers to name used by SSH.

data Store = Store { _storeName     :: Text
                   , _storeHostRe   :: Text
                   , _storeSelfRe   :: Text
                   , _storeUserName :: Text
                   , _storeZfsPool  :: Maybe Text
                   , _storeZfsPath  :: Maybe FilePath
                   -- 'storeTargets' is a list of (StoreName, [FilesetName]),
                   -- where the containers involved are looked up from the
                   -- Container list by the Store/Fileset name pair for both
                   -- source and target.
                   , _storeTargets  :: [(Text, [Text])] }
           deriving Show

makeLenses ''Store

$(deriveJSON (L.drop 6) ''Store)

instance NFData Store where
  rnf a = a `seq` ()

data Info = Info { _infoHostName  :: Text
                 , _infoStore     :: Store
                 , _infoContainer :: Container }
          deriving Show

makeLenses ''Info

data Binding = Binding { _bindingThis    :: Info
                       , _bindingThat    :: Info
                       , _bindingFileset :: Fileset }
             deriving Show

makeLenses ''Binding

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

  runPushme opts `catch` \(_ :: PushmeException) -> return ()
                 `catch` \(ex :: SomeException)  -> error (show ex)

testme :: IO ()
testme = runPushme
         PushmeOpts { jobs      = 1
                    , dryRun    = True
                    , noSync    = True
                    , loadRevs  = True
                    , stores    = False
                    , ssh       = ""
                    , filesets  = ""
                    , classes   = ""
                    , verbose   = True
                    , quiet     = False
                    , debug     = True
                    , arguments = ["@data", "data/titan"] }

runPushme :: PushmeOpts -> IO ()
runPushme opts = do
  let level
        | debug opts   = DEBUG
        | verbose opts = INFO
        | otherwise    = NOTICE
  h <- (`setFormatter` tfLogFormatter "%H:%M:%S" "$time - [$prio] $msg")
       <$> streamHandler System.IO.stderr level

  removeAllHandlers
  updateGlobalLogger "pushme" (setLevel level)
  updateGlobalLogger "pushme" (addHandler h)

  withStore "main" $ do
    putValue "main" "opts" opts

    when (dryRun opts || noSync opts) $
      warningM "pushme" "`--dryrun' specified, no changes will be made!"

    sts   <- readDataFile ".pushme/stores.yml"     :: IO [Store]
    fsets <- readDataFile ".pushme/filesets.yml"   :: IO [Fileset]
    cts   <- readDataFile ".pushme/containers.yml" :: IO [Container]

    pushmeCommand opts sts fsets cts

  stopGlobalPool

pushmeCommand :: PushmeOpts -> [Store] -> [Fileset] -> [Container] -> IO ()
pushmeCommand opts sts fsets cts
  | stores opts =
    for_ (L.sortBy (compare `on` (^.containerFileset)) cts) $ \ct ->
      putStrLn $
        printf "%-20s %-30.30s   %19s %5d"
               (toString (ct^.containerStore))
               (toString (toTextIgnore (ct^.containerPath)))
               (maybe "" (formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S")
                      (ct^.containerLastSync))
               (fromMaybe 0 (ct^.containerLastRev))

  | otherwise = do
    here <- shelly $ silently $ cmd "hostname"
    let this = findStore here sts

    shelly $ debugL $ here <> " -> " <> fromString (show this)

    cts' <-
      L.foldl'
        (\acc host -> do
            innerCts   <- acc
            updatedCts <- shelly $
              processHost (here,this) sts fsets innerCts (fromString host)
            -- This can get expensive fast, but the number of containers
            -- involved should never be large.  The idea is to gather the most
            -- recent data from all updated containers, until we're back to a
            -- fully up-to-date master list.  We can't do it "as we go"
            -- because of the parallel processing.
            return $
              L.map (\ct -> L.foldl' mergeContainers ct updatedCts) innerCts)
        (return cts) (arguments opts)

    unless (dryRun opts || noSync opts) $
      writeDataFile ".pushme/containers.yml" cts'

processHost :: (Text, Store) -> [Store] -> [Fileset] -> [Container] -> Text
            -> Sh [Container]
processHost defaultSelf@(here, _) sts fsets conts thereRaw
  | T.head thereRaw == '@' =
    let there = T.drop 1 thereRaw
        that  = findStore there sts
    in mconcat <$>
       (sequence
        $ L.map
            (\x -> do
                bnd <- x
                let info = bnd^.bindingThat
                createSnapshot (storeIsLocal here (info^.infoStore)) info)
        $ L.map
            (\x -> createBinding defaultSelf (there,that) fsets conts
                                (x^.containerFileset))
            (containersForStore that conts))

  | otherwise = do
    let (self@(_, this), there) =
          if "/" `isInfixOf` thereRaw
          then let [b, e] = T.splitOn "/" thereRaw
               in ((b, findStore b sts), e)
          else (defaultSelf, thereRaw)
        that = findStore there sts

    debugL $ (this^.storeHostRe) <> " -> " <> fromString (show this)
    debugL $ there <> " -> " <> fromString (show that)

    noticeL $ format "Synchronizing {} -> {}" [ this^.storeName
                                             , that^.storeName ]

    case L.lookup (that^.storeName) (this^.storeTargets) of
      Nothing -> warningL "Nothing to do" >> return []

      Just xs -> do
        fss <- fromString <$> getOption filesets
        cls <- fromString <$> getOption classes

        mappings <-
          traverse (createBinding self (there,that) fsets conts) xs

        let filteredMappings =
              L.filter
                (\x -> (T.null fss ||
                        matchText fss (x^.bindingFileset.filesetName))
                       && (T.null cls ||
                           matchText cls (x^.bindingFileset.filesetClass)))
                mappings

            sortedMappings =
              L.sortBy (compare `on` (^.bindingFileset.filesetPriority))
                       filteredMappings

        for_ sortedMappings $ \bnd ->
          when (bnd^.bindingFileset.filesetReportMissing) $
            reportMissingFiles (bnd^.bindingFileset.filesetName)
                               (bnd^.bindingThis.infoContainer)

        updatedContainers <-
          liftIO $ parallel $
            L.map (shelly . syncContainers) sortedMappings

        return $ mconcat updatedContainers

zfsPoolPath :: Info -> FilePath
zfsPoolPath info =
      fromMaybe (error $ "storeZfsPool is Nothing: " ++ show info)
                (info^.infoStore.storeZfsPool)
  </> fromMaybe (error $ "containerPoolPath is Nothing: " ++ show info)
                (info^.infoContainer.containerPoolPath)

zfsFilePath :: Info -> IO FilePath
zfsFilePath info = do
  Pathname p <-
    getPathname $
          fromMaybe (error $ "storeZfsPath is Nothing: " ++ show info)
                    (info^.infoStore.storeZfsPath)
      </> fromMaybe (error $ "containerPoolPath is Nothing: " ++ show info)
                    (info^.infoContainer.containerPoolPath)
  return p

getZfsPath :: Info -> Bool -> IO ContainerPath
getZfsPath info bothZfs
  | bothZfs    = return $ ZfsFilesystem $ toTextIgnore $ zfsPoolPath info
  | isZfs info = Pathname <$> zfsFilePath info
  | otherwise  = getPathname (info^.infoContainer.containerPath)

createSnapshot :: Bool -> Info -> Sh [Container]
createSnapshot local info
  | isZfs info = do
    let cont         = info^.infoContainer
        lastRev      = cont^.containerLastRev
        nextRev      = case lastRev of Nothing -> 1; Just x -> succ x
        poolName     = zfsPoolPath info
        thatSnapshot = toTextIgnore poolName <> "@" <> intToText nextRev
        snapCmd      = ["zfs", "snapshot"]
                    <> ["-r" | cont^.containerRecurse]
                    <> [thatSnapshot]

    noticeL $ format "Creating snapshot {}" [thatSnapshot]

    if local
      then
        vrun_ "sudo" snapCmd
      else do
        let u = info^.infoStore.storeUserName
            h = info^.infoHostName
        remote vrun_ (format "{}@{}" [u, h]) snapCmd

    return [containerLastRev .~ Just nextRev $ cont]

  | otherwise = return []

reportMissingFiles :: Text -> Container -> Sh ()
reportMissingFiles label cont = do
  (Pathname contPath) <- liftIO $ getPathname (cont^.containerPath)

  files    <- L.map (T.drop (T.length (toTextIgnore contPath))
                     . toTextIgnore)
              <$> ls contPath
  optsFile <- liftIO $ getHomePath (".pushme/filters/"
                                    <> cont^.containerFileset)
  exists   <- liftIO $ isFile optsFile

  when exists $ do
    optsText <- readfile optsFile
    let stringify    = (\x -> if L.head x == '/' then L.tail x else x)
                       . (\x -> if x !! (L.length x - 1) == '/'
                                then L.init x else x)
                       . T.unpack . T.drop 2
        patterns     = L.map Glob.compile
                       . L.filter (`notElem` ["*", "*/", ".*", ".*/"])
                       . L.map stringify . T.lines $ optsText
        patMatch f p = Glob.match p f
        files'       =
          L.foldl' (\acc x ->
                     if L.any (patMatch x) patterns
                     then acc
                     else x:acc) []
                   (L.map toString
                    . L.filter (`notElem` [ ".DS_Store", ".localized" ])
                    $ files)

    for_ files' $ \f ->
      warningL $ format "{}: unknown: \"{}\"" [label, fromString f]

createBinding :: (Text, Store) -> (Text, Store) -> [Fileset] -> [Container]
              -> Text -> Sh Binding
createBinding (here,this) (there,that) fsets conts n = do
  let bnd =
        case L.find (\x -> x^.filesetName == n) fsets of
          Nothing ->
            error $ toString  $ "Could not find fileset: " <> n
          Just fs ->
            Binding {
                _bindingThis =
                   Info { _infoHostName  = here
                        , _infoStore     = this
                        , _infoContainer = findContainer fs this conts }
              , _bindingThat =
                   Info { _infoHostName  = there
                        , _infoStore     = that
                        , _infoContainer = findContainer fs that conts }
              , _bindingFileset = fs }
  load <- getOption loadRevs
  if load
    then do
      thisContRev <- containerRev (bnd^.bindingThis)
      thatContRev <- containerRev (bnd^.bindingThat)
      let l = infoContainer.containerLastRev
      return $ bindingThis.l .~ (D.trace ("thisContRev:" ++ show thisContRev) thisContRev)
             $ bindingThat.l .~ (D.trace ("thatContRev:" ++ show thatContRev) thatContRev) $ bnd
    else return bnd

  where
    containerRev info = silently $ do
      p <- liftIO $ zfsFilePath info
      let p' = toTextIgnore $ p </> fromText ".zfs" </> fromText "snapshot"
          u  = info^.infoStore.storeUserName
          h  = info^.infoHostName
          hn = format "{}@{}" [u, h]
      listing <- L.sort <$> L.map (read . toString) <$>
                L.filter (T.all isDigit) <$> T.lines <$>
                if storeIsLocal here (info^.infoStore)
                then vrun "ls" ["-1", p']
                else remote vrun hn ["ls", "-1", p']
      if L.length listing == 0
        then return Nothing
        else return . Just . L.last $ listing

containersForStore :: Store -> [Container] -> [Container]
containersForStore store conts =
  L.filter (\x ->
             let stName = x^.containerStore
                 names  = if "," `isInfixOf` stName
                          then T.splitOn "," stName
                          else [stName]
             in (store^.storeName) `elem` names) conts

findContainer :: Fileset -> Store -> [Container] -> Container
findContainer fileset store conts =
  fromMaybe
    (error $ toString $
     format "Could not find container for Store {} + Fileset {}"
     [ store^.storeName, fileset^.filesetName ]) $
    L.find (\x -> (x^.containerFileset) == (fileset^.filesetName))
           (containersForStore store conts)

mergeContainers :: Container -> Container -> Container
x `mergeContainers` y
  |   (x^.containerFileset)  == (y^.containerFileset)
    && (x^.containerStore)    == (y^.containerStore)
    && (x^.containerPath)     == (y^.containerPath)
    && (x^.containerPoolPath) == (y^.containerPoolPath) =
    containerLastRev .~
      (if isNothing (y^.containerLastRev)
       then x^.containerLastRev
       else if   isNothing (x^.containerLastRev)
               || fromJust (x^.containerLastRev) <
                 fromJust (y^.containerLastRev)
            then y^.containerLastRev
            else x^.containerLastRev) $
    containerLastSync .~
      (if isNothing (y^.containerLastSync)
       then x^.containerLastSync
       else if   isNothing (x^.containerLastSync)
               || fromJust (x^.containerLastSync) <
                 fromJust (y^.containerLastSync)
            then y^.containerLastSync
            else x^.containerLastSync) $ x
  | otherwise = x

updateContainers :: Maybe Int -> Binding -> Sh [Container]
updateContainers Nothing bnd = do
  now <- liftIO currentLocalTime
  let update = containerLastSync .~ Just now
  return [ update (bnd^.bindingThis.infoContainer)
         , update (bnd^.bindingThat.infoContainer) ]

updateContainers rev bnd = do
  now <- liftIO currentLocalTime
  let update = containerLastSync .~ Just now
  return [ update (bnd^.bindingThis.infoContainer)
         , containerLastRev .~ rev $
           update (bnd^.bindingThat.infoContainer) ]

findStore :: Text -> [Store] -> Store
findStore n sts =
  fromMaybe
    (error $ toString $ "Could not find store matching name " <> n) $
    L.find (\x -> matchText (x^.storeHostRe) n) sts

storeIsLocal :: Text -> Store -> Bool
storeIsLocal host st = matchText (st^.storeSelfRe) host

isZfs :: Info -> Bool
isZfs info = isJust (info^.infoStore.storeZfsPool)
           && isJust (info^.infoContainer.containerPoolPath)

bothSidesZfs :: Info -> Info -> Bool
bothSidesZfs = (&&) `on` isZfs

data ContainerPath = NoPath
                   | Pathname FilePath
                   | ZfsFilesystem Text
                   | ZfsSnapshot Text Int
                   | ZfsSnapshotRange Text Int Int
                   deriving Show

data FullPath = LocalPath ContainerPath
              | RemotePath Text Text ContainerPath
              deriving Show

convertPath :: FilePath -> String
convertPath = toString

getHomePath :: Text -> IO FilePath
getHomePath p = (</> fromText p) <$> getHomeDirectory

getPathname :: FilePath -> IO ContainerPath
getPathname fp = do
  let text = toTextIgnore fp
  p <- if T.head text == '~'
       then toTextIgnore <$> getHomePath (T.drop 2 text)
       else return text
  return $ Pathname $ fromText $ if T.last p /= '/'
                                 then T.append p "/"
                                 else p

sourcePath :: Binding -> IO FullPath
sourcePath bnd
  | bothSidesZfs this that =
    let poolPath = toTextIgnore $ zfsPoolPath this
    in case (thisCont^.containerLastRev, thatCont^.containerLastRev) of
         (Just thisRev, Just thatRev) ->
           if thisRev > thatRev
           then return $ LocalPath $ ZfsSnapshotRange poolPath thatRev thisRev
           else return $ LocalPath NoPath
         (Just thisRev, Nothing) ->
           return $ LocalPath $ ZfsSnapshot poolPath thisRev
         (Nothing, _) ->
           return $ LocalPath $ ZfsFilesystem poolPath

  | isZfs this =
    LocalPath <$> (Pathname <$> zfsFilePath this)

  | otherwise =
    LocalPath <$> getPathname (bnd^.bindingThis.infoContainer.containerPath)

  where this      = bnd^.bindingThis
        that      = bnd^.bindingThat
        thisCont  = this^.infoContainer
        thatCont  = that^.infoContainer

destinationPath :: Binding -> IO FullPath
destinationPath bnd
  | storeIsLocal (this^.infoHostName) (that^.infoStore) =
    LocalPath <$> getZfsPath that (bothSidesZfs this that)

  | otherwise = do
    fullpath <- getZfsPath that (bothSidesZfs this that)
    return $
      RemotePath (that^.infoStore.storeUserName)
                 (that^.infoHostName) fullpath

  where this = bnd^.bindingThis
        that = bnd^.bindingThat

syncContainers :: Binding -> Sh [Container]
syncContainers bnd = errExit False $ do
  verb  <- getOption verbose

  noticeL $ format "Sending {}/{} → {}"
                   [ bnd^.bindingThis.infoStore.storeName
                   , bnd^.bindingFileset.filesetName
                   , bnd^.bindingThat.infoStore.storeName ]

  (sendCmd, recvCmd, updater) <- createSyncCommands bnd

  sendArgs <- sendCmd
  unless (L.null sendArgs) $ do
    recvArgs <- recvCmd
    if L.null recvArgs
      then vrun_ (fromText (L.head sendArgs)) (L.tail sendArgs)
      else escaping False $
           vrun_ (fromText (L.head sendArgs)) $
             L.tail sendArgs <> ["|"] <> (if verb
                                          then ["pv", "|"]
                                          else [])
                             <> recvArgs
  updater

createSyncCommands :: Binding -> Sh (Sh [Text], Sh [Text], Sh [Container])
createSyncCommands bnd = do
  src <- liftIO $ sourcePath bnd
  dst <- liftIO $ destinationPath bnd

  let thisCont    = bnd^.bindingThis.infoContainer
      thatCont    = bnd^.bindingThat.infoContainer
      recurseThis = thisCont^.containerRecurse
      recurseThat = thatCont^.containerRecurse

  case (src, dst) of
    (LocalPath NoPath, _) ->
      return ( infoL "No sync needed" >> return []
             , return []
             , return [] )

    (LocalPath (Pathname l), LocalPath (Pathname r)) ->
      return ( systemVolCopy (bnd^.bindingFileset.filesetName) l
                             (toTextIgnore r)
             , return []
             , updateContainers Nothing bnd )

    (LocalPath (Pathname l), RemotePath u h (Pathname r)) ->
      return ( systemVolCopy (bnd^.bindingFileset.filesetName) l $
                             format "{}@{}:{}"
                                    [u, h, escape (toTextIgnore r)]
             , return []
             , updateContainers Nothing bnd )

    (LocalPath (ZfsFilesystem l),
     LocalPath (ZfsFilesystem r)) ->
      return ( localSend l recurseThis
             , localReceive r recurseThat
             , updateContainers (thisCont^.containerLastRev) bnd )

    (LocalPath (ZfsFilesystem l),
     RemotePath u h (ZfsFilesystem r)) ->
      return ( localSend l recurseThis
             , remoteReceive u h r recurseThat
             , updateContainers (thisCont^.containerLastRev) bnd )

    (LocalPath (ZfsSnapshot l e),
     LocalPath (ZfsFilesystem r)) ->
      return ( localSendRev l recurseThis e
             , localReceive r recurseThat
             , updateContainers (Just e) bnd )

    (LocalPath (ZfsSnapshot l e),
     RemotePath u h (ZfsFilesystem r)) ->
      return ( localSendRev l recurseThis e
             , remoteReceive u h r recurseThat
             , updateContainers (Just e) bnd )

    (LocalPath (ZfsSnapshotRange l b e),
     LocalPath (ZfsFilesystem r)) ->
      return ( localSendTwoRevs l recurseThis b e
             , localReceive r recurseThat
             , updateContainers (Just e) bnd )

    (LocalPath (ZfsSnapshotRange l b e),
     RemotePath u h (ZfsFilesystem r)) ->
      return ( localSendTwoRevs l recurseThis b e
             , remoteReceive u h r recurseThat
             , updateContainers (Just e) bnd )

    (l, r) -> error $ toString $
         format "Unexpected paths {} and {}"
                [ T.pack (show l), T.pack (show r) ]

  where
    escape x = if "\"" `isInfixOf` x || " " `isInfixOf` x
               then "'" <> T.replace "\"" "\\\"" x <> "'"
               else x

    localSend pool recurse =
      return $ ["sudo", "zfs", "send"]
               <> ["-R" | recurse]
               <> [pool]

    localSendRev pool recurse r1 = do
      verb <- getOption verbose
      return $ ["sudo", "zfs", "send"]
               <> ["-R" | recurse]
               <> ["-v" | verb]
               <> [ pool <> "@" <> intToText r1 ]

    localSendTwoRevs pool recurse r1 r2 = do
      verb <- getOption verbose
      return $ ["sudo", "zfs", "send"]
               <> ["-R" | recurse]
               <> ["-v" | verb]
               <> [ "-I"
                  , pool <> "@" <> intToText r1
                  , pool <> "@" <> intToText r2 ]

    localReceive pool recurse =
      return $ ["sudo", "zfs", "recv"]
               <> ["-d" | recurse]
               <> ["-F", pool]

    remoteReceive u h pool recurse =
      remote (\x xs -> return (toTextIgnore x:xs)) (format "{}@{}" [u, h]) $
             ["zfs", "recv"] <> ["-d" | recurse] <> ["-F", pool ]

systemVolCopy :: Text -> FilePath -> Text -> Sh [Text]
systemVolCopy label src dest = do
  optsFile <- liftIO $ getHomePath (".pushme/filters/" <> label)
  exists   <- liftIO $ isFile optsFile
  let rsyncOptions = ["--include-from=" <> toTextIgnore optsFile | exists]
  volcopy True rsyncOptions (toTextIgnore src) dest

volcopy :: Bool -> [Text] -> Text -> Text -> Sh [Text]
volcopy useSudo options src dest = do
  infoL $ format "{} → {}" [src, dest]

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

  return []

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
remote f host xs = do
  sshCmd <- getOption ssh
  p <- if L.null sshCmd
      then which "ssh"
      else return . Just . fromText . T.pack $ sshCmd
  case p of
    Nothing -> error "Could not find ssh!"
    Just r  -> f r (host:xs)

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

  dry   <- getOption dryRun
  drier <- getOption noSync
  if drier || (dry && heedDry)
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
