{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.Concurrent.ParallelIO ( stopGlobalPool, parallel )
import           Control.DeepSeq ( NFData(..) )
import           Control.Exception ( SomeException, Exception, catch )
import           Control.Lens
import           Control.Monad ( void, liftM2, (>=>) )
import           Data.Aeson ( ToJSON(..), FromJSON(..) )
import           Data.Aeson.TH ( deriveJSON )
import qualified Data.ByteString.Char8 as BC ( writeFile, readFile )
import           Data.Char ( isDigit )
import           Data.Data ( Data )
import           Data.Foldable ( for_ )
import           Data.Function ( on )
import           Data.Function.Pointless ( (.:) )
import           Data.List
import qualified Data.Map as M ( Map, fromList, lookup )
import           Data.Maybe ( fromMaybe, isNothing, isJust, fromJust )
import           Data.Monoid ( Monoid(mconcat), (<>) )
import           Data.Stringable as S ( Stringable(fromString, toString) )
import           Data.Text.Format ( format )
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Time.Clock ( getCurrentTime )
import           Data.Time.Format ( readTime, formatTime )
import           Data.Time.LocalTime
import           Data.Typeable ( Typeable )
import           Data.Yaml ( encode, decode )
import           Filesystem ( isFile, getHomeDirectory )
import           GHC.Conc ( setNumCapabilities, getNumProcessors )
import           Options.Applicative
import           Prelude hiding (FilePath, catch)
import           Shelly hiding (find)
import           System.Environment ( getArgs, withArgs )
import           System.IO ( stderr )
import           System.IO.Storage ( withStore, putValue, getValue )
import           System.Locale ( defaultTimeLocale )
import           System.Log.Formatter ( tfLogFormatter )
import           System.Log.Handler ( setFormatter )
import           System.Log.Handler.Simple ( streamHandler )
import           System.Log.Logger
import           Text.Printf ( printf )
import           Text.Regex.Posix ( (=~) )

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
version = "1.2.0"

copyright :: String
copyright = "2012"

pushmeSummary :: String
pushmeSummary = "pushme v" ++ version ++ ", (C) John Wiegley " ++ copyright

data PushmeOpts = PushmeOpts
    { jobs     :: Int
    , dryRun   :: Bool
    , noSync   :: Bool
    , copyAll  :: Bool
    , loadRevs :: Bool
    , stores   :: Bool
    , ssh      :: String
    , filesets :: String
    , classes  :: String
    , verbose  :: Bool
    , quiet    :: Bool
    , debug    :: Bool
    , cliArgs  :: [String]
    } deriving (Data, Typeable, Show, Eq)

pushmeOpts :: Parser PushmeOpts
pushmeOpts = PushmeOpts
    <$> option (short 'j' <> long "jobs" <> value 2 <>
                help "Run INT concurrent finds at once (default: 2)")
    <*> switch (short 'n' <> long "dry-run" <>
                help "Don't take any actions")
    <*> switch (short 'N' <> long "no-sync" <>
                help "Don't even attempt a dry-run sync")
    <*> switch (long "copy-all" <>
                help "For git-annex directories, copy all files")
    <*> switch (short 'L' <> long "load-revs" <>
                help "Load latest snapshot revs from disk")
    <*> switch (long "stores" <>
                help "Show all the stores know to pushme")
    <*> strOption (long "ssh" <> value "" <>
                   help "Use a specific ssh command")
    <*> strOption (short 'f' <> long "filesets" <> value "" <>
                   help "Synchronize the given fileset(s) (comma-sep)")
    <*> strOption (short 'c' <> long "classes" <> value "" <>
                   help "Filesets classes to synchronize (comma-sep)")
    <*> switch (short 'v' <> long "verbose" <> help "Report progress verbosely")
    <*> switch (short 'q' <> long "quiet" <> help "Be a little quieter")
    <*> switch (short 'D' <> long "debug" <> help "Report debug information")
    <*> arguments Just (metavar "ARGS")

-- | A 'Fileset' is a logical grouping of files, with an assigned class and
--   priority.  It may also have an associated filter.

data Fileset = Fileset
    { _filesetName          :: Text
    , _filesetClass         :: Text
    , _filesetPriority      :: Int
    , _filesetReportMissing :: Bool
    } deriving (Show, Eq)

makeLenses ''Fileset

$(deriveJSON (drop 8) ''Fileset)

-- | A 'Container' is a physical grouping of files, reflecting an instance of
--   a 'Fileset' somewhere on a storage medium.  For every 'Fileset', there
--   are many 'Container' instances.  When a fileset is synchronized, it means
--   copying it by some means between different containers.

data Container = Container
    { _containerFileset  :: Text
    , _containerStore    :: Text
    , _containerPath     :: FilePath
    , _containerPoolPath :: Maybe FilePath
    , _containerRecurse  :: Bool
    , _containerIsAnnex  :: Bool
    , _containerLastRev  :: Maybe Int
    , _containerLastSync :: Maybe LocalTime
    } deriving (Show, Eq)

makeLenses ''Container

$(deriveJSON (drop 10) ''Container)

instance NFData Container where
  rnf a = a `seq` ()

-- | A 'Store' is a repository that holds 'Container' instances in some form
--   or another.  Synchronization of a remote ZFS container from a local ZFS
--   container is done by copying over incremental snapshots.  Note that
--   'storeHostName' refers to name used by SSH.

data Store = Store
    { _storeName       :: Text
    , _storeHostRe     :: Text
    , _storeSelfRe     :: Text
    , _storeUserName   :: Text
    , _storeIsPrimary  :: Bool
    , _storeZfsPool    :: Maybe Text
    , _storeZfsPath    :: Maybe FilePath
    -- 'storeTargets' is a list of (StoreName, [FilesetName]), where the
    -- containers involved are looked up from the Container list by the
    -- Store/Fileset name pair for both source and target.
    , _storeTargets    :: [(Text, [Text])]
    , _storeAnnexName  :: Text
    , _storeAnnexFlags :: [(Text, [Text])]
    } deriving (Show, Eq)

makeLenses ''Store

$(deriveJSON (drop 6) ''Store)

instance NFData Store where
  rnf a = a `seq` ()

data Info = Info
    { _infoHostName  :: Text
    , _infoStore     :: Store
    , _infoContainer :: Container
    } deriving (Show, Eq)

makeLenses ''Info

data Binding = Binding
    { _bindingThis    :: Info
    , _bindingThat    :: Info
    , _bindingFileset :: Fileset
    } deriving (Show, Eq)

makeLenses ''Binding

main :: IO ()
main = execParser opts >>= \o -> do
    _ <- GHC.Conc.setNumCapabilities (jobs o)
    runPushme o
    stopGlobalPool
  where
    opts = info (helper <*> pushmeOpts)
                (fullDesc <> progDesc "" <> header pushmeSummary)

runPushme :: PushmeOpts -> IO ()
runPushme opts = do
  let level | debug opts   = DEBUG
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

pushmeCommand :: PushmeOpts -> [Store] -> [Fileset] -> [Container] -> IO ()
pushmeCommand opts sts fsets cts
  | stores opts =
    let fss    = fromString $ filesets opts
        cls    = fromString $ classes opts
        sorted =
          sortBy (compare `on` (^._2.filesetPriority)) $
          filter
            (\(_, fs) ->
                (T.null fss || matchText fss (fs^.filesetName))
              && (T.null cls || matchText cls (fs^.filesetClass))) $
          map (\x -> (x, findFileset (x^.containerFileset) fsets)) $
          case cliArgs opts of
            [] -> cts
            xs -> nub $ mconcat $
                 map (\n -> containersForStore (findStore (fromString n) sts)
                                              cts) xs

    in for_ sorted $ \(ct, _) ->
         putStrLn $
           printf "%-12s %-38.38s   %19s %5d"
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
      foldl'
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
              map (\ct -> foldl' mergeContainers ct updatedCts) innerCts)
        (return cts) (cliArgs opts)

    unless (dryRun opts || noSync opts) $
      writeDataFile ".pushme/containers.yml" cts'

processHost :: (Text, Store) -> [Store] -> [Fileset] -> [Container] -> Text
            -> Sh [Container]
processHost defaultSelf@(here, _) sts fsets conts thereRaw
  | T.head thereRaw == '@' = do
    let there = T.drop 1 thereRaw
        that  = findStore there sts
    bindings <-
      traverse (\x -> createBinding defaultSelf (there,that) fsets conts
                                   (x^.containerFileset))
               (containersForStore that conts)
    sorted   <- filterAndSortBindings bindings
    mconcat <$>
      (sequence $ flip map sorted $ \bnd -> do
          let info = bnd^.bindingThat
          createSnapshot (storeIsLocal here (info^.infoStore)) info)

  | otherwise = do
    let (self@(_, this), there) =
          if "/" `T.isInfixOf` thereRaw
          then let [b, e] = T.splitOn "/" thereRaw
               in ((b, findStore b sts), e)
          else (defaultSelf, thereRaw)
        that = findStore there sts

    debugL $ (this^.storeHostRe) <> " -> " <> fromString (show this)
    debugL $ there <> " -> " <> fromString (show that)

    noticeL $ format "\ESC[31mSynchronizing {} -> {}\ESC[0m"
                     [this^.storeName , that^.storeName]

    xs <- case lookup (that^.storeName) (this^.storeTargets) of
      Nothing -> warningL "Nothing to do" >> return []

      Just xs -> do
        bindings <- traverse (createBinding self (there,that) fsets conts) xs
                    >>= filterAndSortBindings
        for_ bindings $ \bnd ->
          when (bnd^.bindingFileset.filesetReportMissing) $
            reportMissingFiles (bnd^.bindingFileset.filesetName)
                               (bnd^.bindingThis.infoContainer)
        return . mconcat =<<
          (liftIO . parallel . map (shelly . syncContainers) $ bindings)

    noticeL $ format "\ESC[32mDone synchronizing {} -> {}\ESC[0m"
                     [this^.storeName , that^.storeName]
    return xs

  where
    filterAndSortBindings bindings = do
      fss <- fromString <$> getOption filesets
      cls <- fromString <$> getOption classes
      return
        $ sortBy (compare `on` (^.bindingFileset.filesetPriority))
        $ filter
          (\x -> (T.null fss ||
                  matchText fss (x^.bindingFileset.filesetName))
                 && (T.null cls ||
                     matchText cls (x^.bindingFileset.filesetClass)))
          bindings

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
        remote vrun_ u h snapCmd

    return [containerLastRev .~ Just nextRev $ cont]

  | otherwise = return []

reportMissingFiles :: Text -> Container -> Sh ()
reportMissingFiles label cont = do
  (Pathname contPath) <- liftIO $ getPathname (cont^.containerPath)

  files    <- map (T.drop (T.length (toTextIgnore contPath))
                     . toTextIgnore)
              <$> ls contPath
  optsFile <- liftIO $ getHomePath (".pushme/filters/"
                                    <> cont^.containerFileset)
  exists   <- liftIO $ isFile optsFile

  when exists $ do
    optsText <- readfile optsFile
    let stringify    = (\x -> if T.head x == '/' then T.tail x else x)
                       . (\x -> if T.index x (T.length x - 1) == '/'
                                then T.init x else x)
                       . T.drop 2
        regexToGlob  = T.replace "].*" "]*"
                       . T.replace "*" ".*"
                       . T.replace "?" "."
                       . T.replace "." "\\."
        patterns     = map regexToGlob
                       . filter (`notElem` ["*", "*/", ".*", ".*/"])
                       . map stringify . T.lines $ optsText
        patMatch f p = toString f =~ toString p
        files'       =
          foldl' (\acc x ->
                     if any (patMatch x) patterns
                     then acc
                     else x:acc) []
                 (filter (`notElem` [ ".DS_Store", ".localized" ]) files)

    for_ files' $ \f ->
      warningL $ format "{}: unknown: \"{}\"" [label, f]

createBinding :: (Text, Store) -> (Text, Store) -> [Fileset] -> [Container]
              -> Text -> Sh Binding
createBinding (here,this) (there,that) fsets conts n = do
  let bnd =
        case find (\x -> x^.filesetName == n) fsets of
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
      let l = infoContainer.containerLastRev -- this one is a mutator
          m = infoContainer.containerLastRev -- this one is an accessor
      thisContRev <- if isZfs (bnd^.bindingThis)
                    then containerRev (bnd^.bindingThis)
                    else return $ bnd^.bindingThis.m
      thatContRev <- if isZfs (bnd^.bindingThat)
                    then containerRev (bnd^.bindingThat)
                    else return $ bnd^.bindingThat.m
      return $ bindingThis.l .~ thisContRev
             $ bindingThat.l .~ thatContRev $ bnd
    else return bnd

  where
    containerRev info = silently $ do
      p <- liftIO $ zfsFilePath info
      let p' = toTextIgnore $ p </> fromText ".zfs" </> fromText "snapshot"
          u  = info^.infoStore.storeUserName
          h  = info^.infoHostName
      listing <- sort <$> map (read . toString) <$>
                filter (T.all isDigit) <$> T.lines <$>
                if storeIsLocal here (info^.infoStore)
                then vrun "ls" ["-1", p']
                else remote vrun u h ["ls", "-1", p']
      if null listing
        then return Nothing
        else return . Just . last $ listing

containersForStore :: Store -> [Container] -> [Container]
containersForStore store =
  filter (\x ->
             let stName = x^.containerStore
                 names  = if "," `T.isInfixOf` stName
                          then T.splitOn "," stName
                          else [stName]
             in (store^.storeName) `elem` names)

findContainer :: Fileset -> Store -> [Container] -> Container
findContainer fileset store conts =
  fromMaybe
    (error $ toString $
     format "Could not find container for Store {} + Fileset {}"
     [ store^.storeName, fileset^.filesetName ]) $
    find (\x -> (x^.containerFileset) == (fileset^.filesetName))
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

findFileset :: Text -> [Fileset] -> Fileset
findFileset n fsets =
  fromMaybe
    (error $ toString $ "Could not find fileset matching name " <> n) $
    find (\x -> n == x^.filesetName) fsets

findStore :: Text -> [Store] -> Store
findStore n sts =
  fromMaybe
    (error $ toString $ "Could not find store matching hostname " <> n) $
    find (\x -> matchText (x^.storeHostRe) n) sts

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
                   deriving (Show, Eq)

data FullPath = LocalPath ContainerPath
              | RemotePath Text Text ContainerPath
              deriving (Show, Eq)

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
           return $ LocalPath $ if thisRev > thatRev
                                then ZfsSnapshotRange poolPath thatRev thisRev
                                else NoPath
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
  unless (null sendArgs) $ do
    noticeL $ T.intercalate " " sendArgs
    recvArgs <- recvCmd
    if null recvArgs
      then vrun_ (fromText (head sendArgs)) (tail sendArgs)
      else escaping False $
           vrun_ (fromText (head sendArgs)) $ tail sendArgs <> ["|"] <> recvArgs
  updater

createSyncCommands :: Binding -> Sh (Sh [Text], Sh [Text], Sh [Container])
createSyncCommands bnd = do
  src   <- liftIO $ sourcePath bnd
  dst   <- liftIO $ destinationPath bnd
  verb  <- getOption verbose
  cpAll <- getOption copyAll

  let thisCont    = bnd^.bindingThis.infoContainer
      thatCont    = bnd^.bindingThat.infoContainer
      recurseThis = thisCont^.containerRecurse
      recurseThat = thatCont^.containerRecurse
      annexFlags  = fromMaybe [ "--not", "--in"
                              , bnd^.bindingThat.infoStore.storeAnnexName ]
                    $ lookup (bnd^.bindingThat.infoStore.storeName)
                             (bnd^.bindingThis.infoStore.storeAnnexFlags)

      annexCmds isRemote path = do
          vrun_ "git-annex" $ ["-q" | not verb] <> ["add", "."]
          vrun_ "git-annex" $ ["-q" | not verb] <> ["sync"]
          vrun_ "git-annex" $ ["-q" | not verb]
                <> [ "--auto"
                   | not ((bnd^.bindingThat.infoStore.storeIsPrimary)
                          || cpAll) ]
                <> [ "copy" ]
                <> annexFlags
                <> [ "--to", bnd^.bindingThat.infoStore.storeAnnexName ]

          if isRemote
              then sub $ do
              let u = bnd^.bindingThat.infoStore.storeUserName
                  h = bnd^.bindingThat.infoHostName
              remote vrun_ u h
                  [ T.concat $
                    [ "\"cd '", toTextIgnore path, "'; git-annex" ]
                    <> [" -q" | not verb]
                    <> [" sync", "\""] ]

              else sub $ do
                   cd path
                   vrun_ "git-annex" $ [" -q" | not verb] <> [" sync"]

          noticeL $ format "{}: Git Annex synchronized"
                           [ bnd^.bindingFileset.filesetName ]

  case (src, dst) of
    (LocalPath NoPath, _) ->
      return ( infoL "No sync needed" >> return []
             , return []
             , return [] )

    (LocalPath (Pathname l), LocalPath (Pathname r)) ->
      if thisCont^.containerIsAnnex && thatCont^.containerIsAnnex
      then
      return ( (if verb then id else silently) $ chdir l $
                 annexCmds False r >> return []
             , return []
             , updateContainers Nothing bnd )
      else
      return ( systemVolCopy
                 ((bnd^.bindingThat.infoStore.storeUserName) /= "root")
                 (bnd^.bindingFileset.filesetName) l (toTextIgnore r)
             , return []
             , updateContainers Nothing bnd )

    (LocalPath (Pathname l), RemotePath u h (Pathname r)) ->
      if thisCont^.containerIsAnnex && thatCont^.containerIsAnnex
      then
      return ( escaping False $ (if verb then id else silently) $
               chdir l $ annexCmds True r >> return []
             , return []
             , updateContainers Nothing bnd )
      else
      return ( systemVolCopy
                 ((bnd^.bindingThat.infoStore.storeUserName) /= "root")
                 (bnd^.bindingFileset.filesetName) l
                 (format "{}@{}:{}" [u, h, escape (toTextIgnore r)])
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
    escape x = if "\"" `T.isInfixOf` x || " " `T.isInfixOf` x
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
      remote (\x xs -> return (toTextIgnore x:xs)) u h  $
             ["zfs", "recv"] <> ["-d" | recurse] <> ["-F", pool ]

systemVolCopy :: Bool -> Text -> FilePath -> Text -> Sh [Text]
systemVolCopy useSudo label src dest = do
  optsFile <- liftIO $ getHomePath (".pushme/filters/" <> label)
  exists   <- liftIO $ isFile optsFile
  let rsyncOptions = ["--include-from=" <> toTextIgnore optsFile | exists]
  volcopy label useSudo rsyncOptions (toTextIgnore src) dest

volcopy :: Text -> Bool -> [Text] -> Text -> Text -> Sh [Text]
volcopy label useSudo options src dest = do
  infoL $ format "{} → {}" [src, dest]

  dry    <- getOption dryRun
  noSy   <- getOption noSync
  deb    <- getOption debug
  verb   <- getOption verbose
  sshCmd <- getOption ssh

  let shhh     = not deb && not verb
      toRemote = ":" `T.isInfixOf` dest
      options' =
        [ "-aHEy"               -- jww (2012-09-23): maybe -A too?
        -- , "--fileflags"
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

        <> (if not (null sshCmd)
            then ["--rsh", fromString sshCmd]
            else [])
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
      if shhh && not noSy
      then silently $ do
        output <- doCopy (drun False) r toRemote useSudo options'
        let stats = M.fromList $
                    map (liftM2 (,) head (head . T.words . last)
                           . T.splitOn ": ")
                    . filter (": " `T.isInfixOf`)
                    . T.lines $ output
            files = field "Number of files" stats
            sent  = field "Number of files transferred" stats
            total = field "Total file size" stats
            xfer  = field "Total transferred file size" stats
        noticeL $ format
            "{}: \ESC[34mSent \ESC[35m{}\ESC[0m\ESC[34m in {} files\ESC[0m (out of {} in {})"
            [ label
            , fromString (humanReadable xfer),
              commaSep (fromIntegral sent)
            , fromString (humanReadable total),
              commaSep (fromIntegral files) ]
      else
        doCopy (drun_ False) r toRemote useSudo options'

  return []

  where
    commaSep :: Int -> Text
    commaSep = fst . T.foldr (\x (xs, num :: Int) ->
                               if num /= 0 && num `mod` 3 == 0
                               then (x `T.cons` ',' `T.cons` xs, num + 1)
                               else (x `T.cons` xs, num + 1))
                             ("", 0)
                   . intToText

    field :: Text -> M.Map Text Text -> Integer
    field x stats = fromMaybe 0 $ read . toString <$> M.lookup x stats

    doCopy f rsync False False os = f rsync os
    doCopy f rsync False True os  = sudo f rsync os
    doCopy f rsync True False os  = f rsync (remoteRsync False rsync:os)
    doCopy f rsync True True os   = asRoot f rsync (remoteRsync True rsync:os)

    remoteRsync useSudo' x = if useSudo'
                             then format "--rsync-path=sudo {}" [toTextIgnore x]
                             else format "--rsync-path={}" [toTextIgnore x]

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

remote :: (FilePath -> [Text] -> Sh a) -> Text -> Text -> [Text] -> Sh a
remote f user host xs = do
  sshCmd <- getOption ssh
  p <- if null sshCmd
      then do sshPath <- which "ssh"
              return (toTextIgnore <$> sshPath)
      else return . Just . T.pack $ sshCmd
  case p of
    Nothing -> error "Could not find ssh!"
    Just r  -> let ys = T.words r <> [format "{}@{}" [user, host]] <> xs
               in f (fromText (head ys)) (tail ys)

asRoot :: (FilePath -> [Text] -> Sh a) -> FilePath -> [Text] -> Sh a
asRoot f p xs =
  f "sudo" [ "su", "-", "root", "-c"
           , T.unwords (map (\x -> "\"" <> x <> "\"") xs') ]
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
  | x < (1024^(2 :: Integer)) =
    printf "%.0fK" (fromIntegral x / (1024 :: Double))
  | x < (1024^(3 :: Integer)) =
    printf "%.1fM" (fromIntegral x / (1024^(2 :: Integer) :: Double))
  | x < (1024^(4 :: Integer)) =
    printf "%.2fG" (fromIntegral x / (1024^(3 :: Integer) :: Double))
  | x < (1024^(5 :: Integer)) =
    printf "%.3fT" (fromIntegral x / (1024^(4 :: Integer) :: Double))
  | x < (1024^(6 :: Integer)) =
    printf "%.3fP" (fromIntegral x / (1024^(5 :: Integer) :: Double))
  | x < (1024^(7 :: Integer)) =
    printf "%.3fX" (fromIntegral x / (1024^(6 :: Integer) :: Double))
  | otherwise  = printf "%db" x

-- Main.hs (pushme) ends here
