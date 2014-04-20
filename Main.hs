{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Conduit
import           Control.Arrow
import           Control.Concurrent.ParallelIO (stopGlobalPool, parallel_)
import           Control.DeepSeq (NFData(..))
import           Control.Lens hiding (argument)
import           Control.Logging
import           Control.Monad
import           Control.Monad.Logger (LogLevel(..))
import           Data.Aeson
import qualified Data.ByteString.Char8 as BC (readFile)
import           Data.Char (isDigit, toLower)
import           Data.Data (Data)
import           Data.Foldable (for_)
import           Data.Function (on)
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromMaybe, isJust, fromJust)
import           Data.Monoid (mempty)
import           Data.Ord (comparing)
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.Format as Fmt
import qualified Data.Text.Format.Params as Fmt
import           Data.Text.Lazy (toStrict)
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.LocalTime (LocalTime, getCurrentTimeZone, utcToLocalTime)
import           Data.Typeable (Typeable)
import           Data.Yaml (decode)
import           Filesystem (isFile, getHomeDirectory)
import           Filesystem.Path.CurrentOS hiding (null)
import           GHC.Conc (setNumCapabilities)
import           Options.Applicative hiding (Success, (&))
import           Prelude hiding (FilePath)
import           Safe hiding (at)
import           Shelly hiding ((</>), find)
import           System.IO.Storage (withStore, putValue, getValue)
import           Text.Printf (printf)
import           Text.Regex.Posix ((=~))

instance FromJSON FilePath where
    parseJSON = parseJSON >=> return . fromText

version :: String
version = "2.0.0"

copyright :: String
copyright = "2013-4"

pushmeSummary :: String
pushmeSummary =
    "pushme " ++ version ++ ", (C) " ++ copyright ++ " John Wiegley"

data PushmeOpts = PushmeOpts
    { jobs      :: Int
    , dryRun    :: Bool
    , noSync    :: Bool
    , copyAll   :: Bool
    , dump      :: Bool
    , ssh       :: String
    , filesets  :: String
    , classes   :: String
    , siUnits   :: Bool
    , verbose   :: Bool
    , quiet     :: Bool
    , cliArgs   :: [String]
    } deriving (Data, Typeable, Show, Eq)

pushmeOpts :: Parser PushmeOpts
pushmeOpts = PushmeOpts
    <$> option
        (   short 'j'
         <> long "jobs"
         <> value 1
         <> help "Run INT concurrent finds at once (default: 1)")
    <*> switch
        (   short 'n'
         <> long "dry-run"
         <> help "Don't take any actions")
    <*> switch
        (   short 'N'
         <> long "no-sync"
         <> help "Don't even attempt a dry-run sync")
    <*> switch
        (   long "copy-all"
         <> help "For git-annex directories, copy all files")
    <*> switch
        (   long "dump"
         <> help "Show all the containers that would be synced")
    <*> strOption
        (   long "ssh"
         <> value ""
         <> help "Use a specific ssh command")
    <*> strOption
        (   short 'f'
         <> long "filesets"
         <> value ""
         <> help "Synchronize the given fileset(s) (comma-sep)")
    <*> strOption
        (   short 'c'
         <> long "classes"
         <> value ""
         <> help "Filesets classes to synchronize (comma-sep)")
    <*> switch
        (   long "si"
         <> help "Use 1000 instead of 1024 to divide")
    <*> switch
        (   short 'v'
         <> long "verbose"
         <> help "Report progress verbosely")
    <*> switch
        (   short 'q'
         <> long "quiet"
         <> help "Be a little quieter")
    <*> many (argument Just (metavar "ARGS"))

-- | A 'Host' provides a way to map command-line arguments to hostnames.
data Host = Host
    { _hostName     :: Text
    , _hostHostRe   :: Text
    , _hostSelfRe   :: Maybe Text
    , _hostUserName :: Maybe Text
    , _hostIsLocal  :: Bool
    -- , _hostIsPrimary  :: Bool
    -- , _hostZfsPool    :: Maybe Text
    -- , _hostZfsPath    :: Maybe FilePath
    -- , _hostAnnexName  :: Text
    -- , _hostAnnexFlags :: [(Text, [(Text, [Text])])]
    } deriving (Show, Eq)

instance FromJSON Host where
    parseJSON (Object v) = do
        name <- v .: "name"
        Host <$> pure name
              <*> v .:? "hostRe" .!= name
              <*> v .:? "selfRe"
              <*> v .:? "userName"
              <*> pure False
    parseJSON _ = errorL "Error parsing Host"

instance NFData Host where
  rnf a = a `seq` ()

makeLenses ''Host

data Rsync = Rsync
    { _rsyncPath    :: FilePath
    , _rsyncFilters :: [Text]
    }
    deriving (Show, Eq)

instance FromJSON Rsync where
    parseJSON (Object v) =
        Rsync <$> v .: "path"
              <*> v .:? "filters" .!= []
    parseJSON _ = errorL "Error parsing Rsync"

makeLenses ''Rsync

data Zfs = Zfs
    { _zfsPool     :: Text
    , _zfsPath     :: FilePath
    , _zfsPoolPath :: FilePath
    , _zfsLastRev  :: Int
    }
    deriving (Show, Eq)

instance FromJSON Zfs where
    parseJSON (Object v) =
        Zfs <$> v .: "pool"
            <*> v .: "path"
            <*> v .: "poolPath"
            <*> pure 1
    parseJSON _ = errorL "Error parsing Zfs"

makeLenses ''Zfs

data Annex = Annex
    { _annexPath      :: FilePath
    , _annexName      :: Text
    , _annexFlags     :: [(Text, [(Text, [Text])])]
    , _annexIsPrimary :: Bool
    }
    deriving (Show, Eq)

instance FromJSON Annex where
    parseJSON (Object v) = do
        p <- v .: "path"
        Annex <$> pure p
              <*> v .:? "name"      .!= toTextIgnore p
              <*> v .:? "Flags"     .!= []
              <*> v .:? "isPrimary" .!= False
    parseJSON _ = errorL "Error parsing Annex"

makeLenses ''Annex

data StorageScheme = SchemeRsync Rsync
                   | SchemeZfs Zfs
                   | SchemeAnnex Annex
    deriving (Show, Eq)

makePrisms ''StorageScheme

-- | A 'Container' is a physical grouping of files, reflecting an instance of
--   a 'Fileset' somewhere on a storage medium.  For every 'Fileset', there
--   are many 'Container' instances.  When a fileset is synchronized, it means
--   copying it by some means between different containers.
data Container = Container
    { _containerHostName :: Text
    , _containerSchemes  :: [StorageScheme]
    } deriving (Show, Eq)

instance FromJSON Container where
    parseJSON (Object v) = do
        mrsync   <- fmap (\p -> [SchemeRsync (Rsync p [])]) <$> v .:? "path"
        mschemes <- v .:? "schemes"
        Container
            <$> v .: "name"
            <*> parseSchemes mrsync mschemes
      where
        parseSchemes mrsync mschemes = do
            let f (name, o) = case map toLower name of
                    "rsync" -> SchemeRsync <$> parseJSON o
                    "zfs"   -> SchemeZfs   <$> parseJSON o
                    "annex" -> SchemeAnnex <$> parseJSON o
                    _       -> errorL $ "Unrecognized scheme type: " <> pack name
            case mschemes of
                Nothing -> return $ fromMaybe [] mrsync
                Just xs -> mapM f xs
    parseJSON _ = errorL "Error parsing Container"

instance NFData Container where
  rnf a = a `seq` ()

makeLenses ''Container

rsyncScheme :: Container -> Maybe Rsync
rsyncScheme = firstOf (containerSchemes.traverse._SchemeRsync)

zfsScheme :: Container -> Maybe Zfs
zfsScheme = firstOf (containerSchemes.traverse._SchemeZfs)

annexScheme :: Container -> Maybe Annex
annexScheme = firstOf (containerSchemes.traverse._SchemeAnnex)

-- | A 'Fileset' is a logical grouping of files, with an assigned class and
--   priority.  It may also have an associated filter.
data Fileset = Fileset
    { _filesetName          :: Text
    , _filesetClass         :: Text
    , _filesetPriority      :: Int
    , _filesetReportMissing :: Bool
    , _filesetContainers    :: Map Text Container
    , _filesetOtherOptions  :: Map Text Value
    } deriving (Show, Eq)

instance FromJSON Fileset where
    parseJSON (Object v) =
        Fileset
            <$> v .: "name"
            <*> v .:? "class"         .!= ""
            <*> v .:? "priority"      .!= 1000
            <*> v .:? "reportMissing" .!= False
            <*> pure mempty
            <*> pure mempty
    parseJSON _ = errorL "Error parsing Fileset"

makeLenses ''Fileset

data Info = Info
    { _infoHostName  :: Text
    , _infoHost      :: Host
    , _infoContainer :: Container
    } deriving (Show, Eq)

makeLenses ''Info

data BindingCommand = BindingSync | BindingSnapshot
    deriving (Show, Eq)

makePrisms ''BindingCommand

data Binding = Binding
    { _bindingFileset :: Fileset
    , _bindingThis    :: Info
    , _bindingThat    :: Info
    , _bindingCommand :: BindingCommand
    } deriving (Show, Eq)

makeLenses ''Binding

main :: IO ()
main = execParser opts >>= \o -> do
    _ <- GHC.Conc.setNumCapabilities (jobs o)
    runPushme o
    stopGlobalPool
  where
    opts = info
        (helper <*> pushmeOpts)
        (fullDesc <> progDesc "" <> header pushmeSummary)

runPushme :: PushmeOpts -> IO ()
runPushme opts = withStdoutLogging $ do
    setLogLevel $ if verbose opts then LevelDebug else LevelInfo
    setLogTimeFormat "%H:%M:%S"

    withStore "main" $ do
        putValue "main" "opts" opts

        when (dryRun opts || noSync opts) $
            warn' "`--dryrun' specified, no changes will be made!"

        here <- shelly $ silently $ cmd "hostname"
        sts  <- readDataFile ".pushme/hosts.yml" :: IO [Host]
        let sts' = map f sts
            f s  = s & hostIsLocal .~ matchText regex here
              where
                regex = fromMaybe (s^.hostHostRe) (s^.hostSelfRe)

        fsets <- runResourceT $
            sourceDirectory ".pushme"
                $= filterC (\n -> extension n == Just ".yml"
                               && filename n /= "hosts.yml")
                $= mapMC (liftIO . (readDataFile :: FilePath -> IO Fileset))
                $$ sinkList

        pushmeCommand opts sts'
            $ M.fromList
            $ map ((^.filesetName) &&& id) fsets

pushmeCommand :: PushmeOpts -> [Host] -> Map Text Fileset -> IO ()
pushmeCommand opts hosts fsets =
    shelly (silently $ cmd "hostname")
        >>= parallel_
            . map (shelly . (go =<<) . annotateBinding)
            . findBindings opts hosts fsets
  where
    go | dump opts = printBinding
       | otherwise = syncBinding

    printBinding bnd = do
        let fs = bnd^.bindingFileset
        printContainer fs (bnd^.bindingThis.infoContainer)
        printContainer fs (bnd^.bindingThat.infoContainer)
      where
        printContainer fs c =
            liftIO $ putStrLn $ printf "%-12s %-12s %-28.38s"
                (unpack (c^.containerHostName))
                (unpack (fs^.filesetName))
                (show (c^.containerSchemes))

    syncBinding bnd@((^.bindingCommand) -> BindingSnapshot) = do
        let that = bnd^.bindingThat
        createSnapshot (that^.infoHost.hostIsLocal) that

    syncBinding bnd = do
        when (bnd^.bindingFileset.filesetReportMissing) $
            reportMissingFiles
                (bnd^.bindingFileset)
                (bnd^.bindingThis.infoContainer)
        syncContainers bnd

annotateBinding :: Binding -> Sh Binding
annotateBinding bnd = do
    let thisInfo = bnd^.bindingThis
        thatInfo = bnd^.bindingThat
    case (zfsScheme (thisInfo^.infoContainer),
          zfsScheme (thatInfo^.infoContainer)) of
        (Just z1, Just z2)  -> do
            thisInfo' <- annotateInfo thisInfo z1
            thatInfo' <- annotateInfo thatInfo z2
            return $ bindingThis .~ thisInfo'
                   $ bindingThat .~ thatInfo' $ bnd
        _ -> return bnd
  where
    annotateInfo i z = do
        mrev <- containerRev i z
        return $ case mrev of
            Nothing -> i
            Just rev ->
                i & infoContainer
                  . containerSchemes
                  . traverse
                  . _SchemeZfs
                  . zfsLastRev
                  .~ rev

    containerRev i z = silently $ do
        let p  = (z^.zfsPath) </> (z^.zfsPoolPath)
            p' = p </> ".zfs" </> "snapshot"
            u  = i^.infoHost.hostUserName
            h  = i^.infoHostName
        fmap lastMay
              $ sort
            <$> map (read . unpack)
            <$> filter (T.all isDigit)
            <$> T.lines
            <$> if i^.infoHost.hostIsLocal
                then vrun "ls" ["-1", toTextIgnore p']
                else remote vrun u h ["ls", "-1", toTextIgnore p']

findBindings :: PushmeOpts -> [Host] -> Map Text Fileset -> Text -> [Binding]
findBindings opts hosts fsets here
    = sortBy (comparing (^.bindingFileset.filesetPriority))
    $ filter matching
    $ catMaybes
    $ createBinding
        <$> M.elems fsets
        <*> pure here
        <*> map pack (cliArgs opts)
  where
    matching bnd =
        let fs = bnd^.bindingFileset
        in (T.null fss || matchText fss (fs^.filesetName))
         && (T.null cls || matchText cls (fs^.filesetClass))
      where
        fss = pack (filesets opts)
        cls = pack (classes opts)

    createBinding :: Fileset -> Text -> Text -> Maybe Binding
    createBinding fs hereRaw thereRaw = do
        let atsign = T.head thereRaw == '@'
            f | atsign = second T.tail
              | "/" `T.isInfixOf` thereRaw =
                  let [b, e] = T.splitOn "/" thereRaw
                  in const (b, e)
              | otherwise = id
            (here', there) = f (hereRaw, thereRaw)
        Binding
            <$> pure fs
            <*> createInfo here'
            <*> createInfo there
            <*> pure (if atsign then BindingSnapshot else BindingSync)
      where
        createInfo :: Text -> Maybe Info
        createInfo n = do
            st <- findHost n hosts
            Info <$> pure here
                 <*> pure st
                 <*> fs^.filesetContainers.at (st^.hostName)

infoZfsPoolPath :: Info -> FilePath
infoZfsPoolPath i =
      fromMaybe (errorL $ "hostZfsPool is Nothing: " <> tshow i)
                (zfs ^? _Just.zfsPool.to fromText)
  </> fromMaybe (errorL $ "containerPoolPath is Nothing: " <> tshow i)
                (zfs ^? _Just.zfsPoolPath)
  where
    zfs = zfsScheme (i^.infoContainer)

infoZfsFilePath :: Info -> IO FilePath
infoZfsFilePath i = do
    Pathname p <-
      getPathname $
            fromMaybe (errorL $ "hostZfsPath is Nothing: " <> tshow i)
                      (zfs ^? _Just.zfsPath)
        </> fromMaybe (errorL $ "containerPoolPath is Nothing: " <> tshow i)
                      (zfs ^? _Just.zfsPoolPath)
    return p
  where
    zfs = zfsScheme (i^.infoContainer)

getZfsPath :: Info -> Bool -> IO ContainerPath
getZfsPath i bothZfs
    | bothZfs   = return $ ZfsFilesystem $ infoZfsPoolPath i
    | isZfs i   = Pathname <$> infoZfsFilePath i
    | otherwise = getRsyncPath (i^.infoContainer)

createSnapshot :: Bool -> Info -> Sh ()
createSnapshot local i
  | isZfs i = do
    let cont         = i^.infoContainer
        zfs          = zfsScheme cont
        nextRev      = zfs^?_Just.zfsLastRev.to succ
        poolName     = infoZfsPoolPath i
        thatSnapshot = poolName <> "@" <> decodeString (show nextRev)
        snapCmd      = map toTextIgnore $ ["zfs", "snapshot"] <> [thatSnapshot]

    liftIO $ log' $ format "Creating snapshot {}" [toTextIgnore thatSnapshot]

    if local
        then vrun_ "sudo" snapCmd
        else remote vrun_
            (i^.infoHost.hostUserName)
            (i^.infoHostName)
            snapCmd

  | otherwise = return ()

reportMissingFiles :: Fileset -> Container -> Sh ()
reportMissingFiles fs cont = do
    Pathname contPath <- liftIO $ getRsyncPath cont
    let len = T.length (toTextIgnore contPath)
    files    <- map (T.drop len . toTextIgnore) <$> ls contPath

    let rsync = rsyncScheme cont
        rfs   = (Success <$> rsync ^? _Just.rsyncFilters)
            <|> (fromJSON <$> fs^.filesetOtherOptions.at "RsyncFilters")
        filters = case rfs of
            Just (Error e)   -> errorL (pack e)
            Just (Success x) -> x
            Nothing          -> []
        patterns
            = map regexToGlob
            $ filter (`notElem` ["*", "*/", ".*", ".*/"])
            $ map stringify filters
        stringify
            = (\x -> if T.head x == '/' then T.tail x else x)
            . (\x -> if T.index x (T.length x - 1) == '/'
                     then T.init x else x)
            . T.drop 2
        regexToGlob
            = T.replace "].*" "]*"
            . T.replace "*" ".*"
            . T.replace "?" "."
            . T.replace "." "\\."
        patMatch f p = unpack f =~ unpack p
        label = fs^.filesetName
        files' =
          foldl'
              (\acc x ->
                if any (patMatch x) patterns
                then acc
                else x:acc) []
              (filter (`notElem` [ ".DS_Store", ".localized" ]) files)
    for_ files' $ \f ->
        liftIO $ warn' $ format "{}: unknown: \"{}\"" [label, f]

idContainers :: Binding -> Sh [Container]
idContainers bnd =
  return [ bnd^.bindingThis.infoContainer
         , bnd^.bindingThat.infoContainer
         ]

updateContainers :: Int -> Binding -> Sh [Container]
updateContainers rev bnd =
  return [ bnd^.bindingThis.infoContainer
         , bnd^.bindingThat.infoContainer
               & containerSchemes
               . traverse
               . _SchemeZfs
               . zfsLastRev
              .~ rev
         ]

findFileset :: Text -> [Fileset] -> Maybe Fileset
findFileset n = find (\x -> n == x^.filesetName)

findHost :: Text -> [Host] -> Maybe Host
findHost n = find (\x -> matchText (x^.hostHostRe) n)

isZfs :: Info -> Bool
isZfs i = isJust (zfsScheme (i^.infoContainer))

bothSidesZfs :: Info -> Info -> Bool
bothSidesZfs = (&&) `on` isZfs

data ContainerPath = NoPath
                   | Pathname FilePath
                   | ZfsFilesystem FilePath
                   | ZfsSnapshot Text Int
                   | ZfsSnapshotRange Text Int Int
                   deriving (Show, Eq)

data FullPath = LocalPath ContainerPath
              | RemotePath (Maybe Text) Text ContainerPath
              deriving (Show, Eq)

convertPath :: FilePath -> String
convertPath = unpack . toTextIgnore

getHomePath :: FilePath -> IO FilePath
getHomePath p = (</> p) <$> getHomeDirectory

getPathname :: FilePath -> IO ContainerPath
getPathname fp = do
  let text = toTextIgnore fp
  p <- if T.head text == '~'
       then toTextIgnore <$> getHomePath (fromText (T.drop 2 text))
       else return text
  return $ Pathname $ fromText $ if T.last p /= '/'
                                 then T.append p "/"
                                 else p

getRsyncPath :: Container -> IO ContainerPath
getRsyncPath c = getPathname (rsyncScheme c ^. _Just.rsyncPath)

sourcePath :: Binding -> IO FullPath
sourcePath bnd
  | bothSidesZfs this that =
    let poolPath = infoZfsPoolPath this
    in case (zfsScheme thisCont ^? _Just.zfsLastRev,
             zfsScheme thatCont ^? _Just.zfsLastRev) of
         (Just thisRev, Just thatRev) ->
           return $ LocalPath $
               if thisRev > thatRev
               then ZfsSnapshotRange (toTextIgnore poolPath) thatRev thisRev
               else NoPath
         (Just thisRev, Nothing) ->
           return $ LocalPath $ ZfsSnapshot (toTextIgnore poolPath) thisRev
         (Nothing, _) ->
           return $ LocalPath $ ZfsFilesystem poolPath

  | isZfs this =
    LocalPath <$> (Pathname <$> infoZfsFilePath this)

  | otherwise =
    LocalPath <$> getRsyncPath thisCont

  where
    this      = bnd^.bindingThis
    that      = bnd^.bindingThat
    thisCont  = this^.infoContainer
    thatCont  = that^.infoContainer

destinationPath :: Binding -> IO FullPath
destinationPath bnd
  | that^.infoHost.hostIsLocal =
    LocalPath <$> getZfsPath that (bothSidesZfs this that)

  | otherwise = do
    fullpath <- getZfsPath that (bothSidesZfs this that)
    return $
      RemotePath (that^.infoHost.hostUserName)
                 (that^.infoHostName) fullpath

  where this = bnd^.bindingThis
        that = bnd^.bindingThat

syncContainers :: Binding -> Sh ()
syncContainers bnd = errExit False $ do
  liftIO $ log' $ format "Sending {}/{} -> {}"
                   [ bnd^.bindingThis.infoHost.hostName
                   , bnd^.bindingFileset.filesetName
                   , bnd^.bindingThat.infoHost.hostName ]

  (sendCmd, recvCmd, updater) <- createSyncCommands bnd

  sendArgs <- sendCmd
  unless (null sendArgs) $ do
    liftIO $ log' $ T.intercalate " " sendArgs
    recvArgs <- recvCmd
    if null recvArgs
      then vrun_ (fromText (head sendArgs)) (tail sendArgs)
      else
        escaping False
          $ vrun_ (fromText (head sendArgs))
          $ tail sendArgs <> ["|"] <> recvArgs
  void updater

createSyncCommands :: Binding -> Sh (Sh [Text], Sh [Text], Sh [Container])
createSyncCommands bnd = do
  src   <- liftIO $ sourcePath bnd
  dst   <- liftIO $ destinationPath bnd
  verb  <- getOption verbose
  cpAll <- getOption copyAll

  let thisCont = bnd^.bindingThis.infoContainer
      thatCont = bnd^.bindingThat.infoContainer

      thisAnnex = annexScheme (bnd^.bindingThis.infoContainer)
      thatAnnex = annexScheme (bnd^.bindingThat.infoContainer)

      defaultFlags = [ "--not", "--in", thatAnnex ^. _Just.annexName ]
      hostAFlags =
          lookup (bnd^.bindingThat.infoHost.hostName)
                 (thisAnnex ^. _Just.annexFlags)
      annexFlags' =
          flip (maybe defaultFlags) hostAFlags $ \flags ->
              fromMaybe (fromMaybe defaultFlags $ lookup "<default>" flags)
                  $ lookup (bnd^.bindingFileset.filesetName) flags

      annexCmds isRemote path' = do
          vrun_ "git-annex" $ ["-q" | not verb]
              <> ["add", "-c", "alwayscommit=false", "."]
          vrun_ "git-annex" $ ["-q" | not verb]
                <> [ "--auto"
                   | not (orOf (_Just.annexIsPrimary) thatAnnex || cpAll) ]
                <> [ "copy", "-c", "alwayscommit=false" ]
                <> annexFlags'
                <> [ "--to"
                   , thatAnnex ^. _Just.annexName
                   ]
          vrun_ "git-annex" $ ["-q" | not verb] <> ["sync"]

          sub $
              if isRemote
              then do
                let u = bnd^.bindingThat.infoHost.hostUserName
                    h = bnd^.bindingThat.infoHostName
                remote vrun_ u h
                    [ T.concat $
                      [ "\"cd '", path', "'; git-annex" ]
                      <> [" -q" | not verb]
                      <> [" sync", "\""] ]

              else do
                cd (fromText path')
                vrun_ "git-annex" $ ["-q" | not verb] <> ["sync"]

          liftIO $ log' $ format "{}: Git Annex synchronized"
                           [ bnd^.bindingFileset.filesetName ]

  case (src, dst) of
    (LocalPath NoPath, _) ->
      return ( liftIO $ log' "No sync needed" >> return []
             , return []
             , return [] )

    (LocalPath (Pathname l), LocalPath (Pathname r)) ->
      return ( if isJust (annexScheme thisCont) && isJust (annexScheme thatCont)
               then (if verb then id else silently) $ chdir l $
                 annexCmds False (toTextIgnore r) >> return []
               else do
                 u2' <- maybe (cmd "whoami") return
                   (bnd^.bindingThat.infoHost.hostUserName)
                 systemVolCopy
                   (u2' /= "root")
                   (bnd^.bindingFileset.filesetName)
                   l
                   (toTextIgnore r)
                 return []
             , return []
             , idContainers bnd )

    (LocalPath (Pathname l), RemotePath u h (Pathname r)) ->
      return ( if isJust (annexScheme thisCont) && isJust (annexScheme thatCont)
               then escaping False $ (if verb then id else silently) $
                 chdir l $ annexCmds True (toTextIgnore r) >> return []
               else do
                 u' <- maybe (cmd "whoami") return u
                 u2' <- maybe (cmd "whoami") return
                   (bnd^.bindingThat.infoHost.hostUserName)
                 systemVolCopy
                   (u2' /= "root")
                   (bnd^.bindingFileset.filesetName)
                   l
                   (format "{}@{}:{}" [u', h, escape (toTextIgnore r)])
                 return []
             , return []
             , idContainers bnd )

    (LocalPath (ZfsFilesystem l),
     LocalPath (ZfsFilesystem r)) ->
      return ( localSend (toTextIgnore l)
             , localReceive (toTextIgnore r)
             , (case zfsScheme thisCont ^? _Just.zfsLastRev of
                     Nothing  -> idContainers
                     Just rev -> updateContainers rev) bnd )

    (LocalPath (ZfsFilesystem l),
     RemotePath u h (ZfsFilesystem r)) ->
      return ( localSend (toTextIgnore l)
             , remoteReceive u h (toTextIgnore r)
             , (case zfsScheme thisCont ^? _Just.zfsLastRev of
                     Nothing -> idContainers
                     Just rev -> updateContainers rev) bnd )

    (LocalPath (ZfsSnapshot l e),
     LocalPath (ZfsFilesystem r)) ->
      return ( localSendRev l e
             , localReceive (toTextIgnore r)
             , updateContainers e bnd )

    (LocalPath (ZfsSnapshot l e),
     RemotePath u h (ZfsFilesystem r)) ->
      return ( localSendRev l e
             , remoteReceive u h (toTextIgnore r)
             , updateContainers e bnd )

    (LocalPath (ZfsSnapshotRange l b e),
     LocalPath (ZfsFilesystem r)) ->
      return ( localSendTwoRevs l b e
             , localReceive (toTextIgnore r)
             , updateContainers e bnd )

    (LocalPath (ZfsSnapshotRange l b e),
     RemotePath u h (ZfsFilesystem r)) ->
      return ( localSendTwoRevs l b e
             , remoteReceive u h (toTextIgnore r)
             , updateContainers e bnd )

    (l, r) -> errorL $
         format "Unexpected paths {} and {}"
                [ pack (show l), pack (show r) ]

  where
    escape x = if "\"" `T.isInfixOf` x || " " `T.isInfixOf` x
               then "'" <> T.replace "\"" "\\\"" x <> "'"
               else x

    localSend pool =
      return $ ["sudo", "zfs", "send"] <> [pool]

    localSendRev pool r1 = do
      verb <- getOption verbose
      return $ ["sudo", "zfs", "send"]
               <> ["-v" | verb]
               <> [ pool <> "@" <> intToText r1 ]

    localSendTwoRevs pool r1 r2 = do
      verb <- getOption verbose
      return $ ["sudo", "zfs", "send"]
               <> ["-v" | verb]
               <> [ "-I"
                  , pool <> "@" <> intToText r1
                  , pool <> "@" <> intToText r2 ]

    localReceive pool = return $ ["sudo", "zfs", "recv"] <> ["-F", pool]

    remoteReceive u h pool =
      remote (\x xs -> return (toTextIgnore x:xs)) u h  $
             ["zfs", "recv"] <> ["-F", pool ]

systemVolCopy :: Bool -> Text -> FilePath -> Text -> Sh ()
systemVolCopy useSudo label src dest = do
  optsFile <- liftIO $ getHomePath (".pushme/filters" </> fromText label)
  exists   <- liftIO $ isFile optsFile
  let rsyncOptions = ["--include-from=" <> optsFile | exists]
  volcopy label useSudo (map toTextIgnore rsyncOptions) (toTextIgnore src) dest

volcopy :: Text -> Bool -> [Text] -> Text -> Text -> Sh ()
volcopy label useSudo options src dest = do
  liftIO $ log' $ format "{} -> {}" [src, dest]

  dry    <- getOption dryRun
  noSy   <- getOption noSync
  verb   <- getOption verbose
  den    <- (\x -> if x then 1000 else 1024) <$> getOption siUnits
  sshCmd <- getOption ssh

  let shhh     = not verb
      toRemote = ":" `T.isInfixOf` dest
      options' =
        [ "-aHEy"               -- jww (2012-09-23): maybe -A too?
        -- , "--fileflags"
        , "--delete-after"
        , "--ignore-errors"
        , "--force"

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
            then ["--rsh", pack sshCmd]
            else [])
        <> ["-n" | dry]
        <> (if shhh
            then ["--stats"]
            else if verb
                 then ["-P"]
                 else ["-v"])
        <> options
        <> [src, dest]

  rsync <- which "rsync"
  case rsync of
    Nothing -> errorL "Could not find rsync!"
    Just r  ->
      if shhh && not noSy
      then silently $ do
        output <- doCopy drun r toRemote useSudo options'
        let stats = M.fromList $
                    map (fmap (T.filter (/= ',') . (!! 1) . T.words) . T.breakOn ": ")
                    . filter (": " `T.isInfixOf`)
                    . T.lines $ output
            files = field "Number of files" stats
            sent  = field "Number of files transferred" stats
            total = field "Total file size" stats
            xfer  = field "Total transferred file size" stats
        liftIO $ log' $ format
            "{}: \ESC[34mSent \ESC[35m{}\ESC[0m\ESC[34m in {} files\ESC[0m (out of {} in {})"
            [ label
            , pack (humanReadable den xfer),
              commaSep (fromIntegral sent)
            , pack (humanReadable den total),
              commaSep (fromIntegral files) ]
      else
        doCopy drun_ r toRemote useSudo options'

  where
    commaSep :: Int -> Text
    commaSep = fst . T.foldr (\x (xs, num :: Int) ->
                               if num /= 0 && num `mod` 3 == 0
                               then (x `T.cons` ',' `T.cons` xs, num + 1)
                               else (x `T.cons` xs, num + 1))
                             ("", 0)
                   . intToText

    field :: Text -> M.Map Text Text -> Integer
    field x stats = fromMaybe 0 $ read . unpack <$> M.lookup x stats

    doCopy f rsync False False os = f rsync os
    doCopy f rsync False True os  = sudo f rsync os
    doCopy f rsync True False os  = f rsync (remoteRsync False rsync:os)
    doCopy f rsync True True os   = asRoot f rsync (remoteRsync True rsync:os)

    -- remoteRsync useSudo' x = if useSudo'
    --                          then format "--rsync-path=sudo {}" [x]
    --                          else format "--rsync-path={}" [x]
    remoteRsync useSudo' _ = if useSudo'
                             then "--rsync-path=sudo rsync"
                             else "--rsync-path=rsync"

readDataFile :: FromJSON a => FilePath -> IO a
readDataFile p = do
  p' <- getHomePath p
  d  <- Data.Yaml.decode <$> BC.readFile (convertPath p')
  case d of
    Nothing -> errorL $ "Failed to read file " <> toTextIgnore p
    Just d' -> return d'

currentLocalTime :: IO LocalTime
currentLocalTime = do
  tz <- getCurrentTimeZone
  tm <- getCurrentTime
  return $ utcToLocalTime tz tm

getOption' :: Data a => (a -> b) -> IO b
getOption' opt = do
  opts <- getValue "main" "opts"
  return $ fromJust $ opt <$> opts

getOption :: Data a => (a -> b) -> Sh b
getOption = liftIO . getOption'

matchText :: Text -> Text -> Bool
matchText = flip (=~) `on` unpack

intToText :: Int -> Text
intToText = pack . show

remote :: (FilePath -> [Text] -> Sh a) -> Maybe Text -> Text -> [Text] -> Sh a
remote f user host xs = do
  sshCmd <- getOption ssh
  p <- if null sshCmd
      then which "ssh"
      else return $ Just (decodeString sshCmd)
  user' <- maybe (cmd "whoami") return user
  case p of
    Nothing -> errorL "Could not find ssh!"
    Just r  ->
        let ys = T.words (toTextIgnore r)
                <> [format "{}@{}" [user', host]] <> xs
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

drun :: FilePath -> [Text] -> Sh Text
drun = doRun run (liftIO . debug') (return "") True False

drun_ :: FilePath -> [Text] -> Sh ()
drun_ = (void .) . doRun run_ (liftIO . debug') (return ()) True False

vrun :: FilePath -> [Text] -> Sh Text
vrun = doRun run (liftIO . log') (return "") True True

vrun_ :: FilePath -> [Text] -> Sh ()
vrun_ = (void .) . doRun run_ (liftIO . log') (return ()) True True

srun :: FilePath -> [Text] -> Sh Text
srun = doRun run  (liftIO . log') (return "") False True

srun_ :: FilePath -> [Text] -> Sh ()
srun_ = (void .) . doRun run_  (liftIO . log') (return ()) False True

tshow :: Show a => a -> Text
tshow = pack . show

format :: Fmt.Params a => Fmt.Format -> a -> Text
format = (toStrict .) . Fmt.format

humanReadable :: Integer -> Integer -> String
humanReadable den x
  | x < den   = printf "%db" x
  | x < (den^(2 :: Integer)) =
    printf "%.0fK" (fromIntegral x / (fromIntegral den :: Double))
  | x < (den^(3 :: Integer)) =
    printf "%.1fM" (fromIntegral x / (fromIntegral den^(2 :: Integer) :: Double))
  | x < (den^(4 :: Integer)) =
    printf "%.2fG" (fromIntegral x / (fromIntegral den^(3 :: Integer) :: Double))
  | x < (den^(5 :: Integer)) =
    printf "%.3fT" (fromIntegral x / (fromIntegral den^(4 :: Integer) :: Double))
  | x < (den^(6 :: Integer)) =
    printf "%.3fP" (fromIntegral x / (fromIntegral den^(5 :: Integer) :: Double))
  | x < (den^(7 :: Integer)) =
    printf "%.3fX" (fromIntegral x / (fromIntegral den^(6 :: Integer) :: Double))
  | otherwise  = printf "%db" x

-- Main.hs (pushme) ends here
