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
import           Control.Exception
import           Control.Lens hiding (argument)
import           Control.Logging
import           Control.Monad
import           Control.Monad.Logger (LogLevel(..))
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import qualified Data.ByteString.Char8 as BC (readFile)
import           Data.Char (isDigit)
import           Data.Data (Data)
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
import           Data.Typeable (Typeable)
import           Data.Yaml (decode)
import           Filesystem
import           Filesystem.Path.CurrentOS hiding (null)
import           GHC.Conc (setNumCapabilities)
import           Options.Applicative hiding (Success, (&))
import           Prelude hiding (FilePath)
import           Safe hiding (at)
import           Shelly hiding ((</>), find)
import           System.IO.Storage (withStore, putValue, getValue)
import           Text.Printf (printf)
import           Text.Regex.Posix ((=~))
import           Text.Show.Pretty hiding (Value)

version :: String
version = "2.0.0"

copyright :: String
copyright = "2013-4"

pushmeSummary :: String
pushmeSummary =
    "pushme " ++ version ++ ", (C) " ++ copyright ++ " John Wiegley"

instance FromJSON FilePath where
    parseJSON = fmap fromText . parseJSON

data Options = Options
    { jobs     :: Int
    , dryRun   :: Bool
    , noSync   :: Bool
    , copyAll  :: Bool
    , dump     :: Bool
    , ssh      :: String
    , fromName :: String
    , filesets :: String
    , classes  :: String
    , siUnits  :: Bool
    , verbose  :: Bool
    , quiet    :: Bool
    , cliArgs  :: [String]
    }
    deriving (Data, Typeable, Show, Eq)

pushmeOpts :: Parser Options
pushmeOpts = Options
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
         <> help "Show all the stores that would be synced")
    <*> strOption
        (   long "ssh"
         <> value ""
         <> help "Use a specific ssh command")
    <*> strOption
        (   long "from"
         <> help "Provide the name of the current host")
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

data Rsync = Rsync
    { _rsyncPath    :: FilePath
    , _rsyncSshHost :: Maybe FilePath
    , _rsyncFilters :: [Text]
    }
    deriving (Show, Eq)

instance FromJSON Rsync where
    parseJSON (Object v) = Rsync
        <$> v .:  "Path"
        <*> v .:? "Host"
        <*> v .:? "Filters" .!= []
    parseJSON _ = errorL "Error parsing Rsync"

makeLenses ''Rsync

data Zfs = Zfs
    { _zfsPath     :: FilePath
    , _zfsPoolPath :: FilePath
    , _zfsLastRev  :: Maybe Int
    }
    deriving (Show, Eq)

instance FromJSON Zfs where
    parseJSON (Object v) = Zfs
        <$> v .: "Path"
        <*> v .: "PoolPath"
        <*> pure Nothing
    parseJSON _ = errorL "Error parsing Zfs"

makeLenses ''Zfs

data Annex = Annex
    { _annexPath      :: FilePath
    , _annexName      :: Maybe Text
    , _annexFlags     :: [(Text, [(Text, [Text])])]
    , _annexIsPrimary :: Bool
    }
    deriving (Show, Eq)

instance FromJSON Annex where
    parseJSON (Object v) = do
        p <- v .: "Path"
        Annex <$> pure p
              <*> v .:? "Name"
              <*> v .:? "Flags"   .!= []
              <*> v .:? "Primary" .!= False
    parseJSON _ = errorL "Error parsing Annex"

makeLenses ''Annex

data StorageScheme
    = SchemeRsync Rsync
    | SchemeZfs Zfs
    | SchemeAnnex Annex
    deriving (Show, Eq)

makePrisms ''StorageScheme

data Store = Store
    { _schemes :: Map Text StorageScheme
    } deriving (Show, Eq)

makeLenses ''Store

instance FromJSON Store where
    parseJSON (Object v) = do
        mpath  <- fmap rsyncDefault <$> v .:? "Path"
        mrsync <- fmap SchemeRsync <$> v .:? "Rsync"
        mzfs   <- fmap SchemeZfs   <$> v .:? "Zfs"
        mannex <- fmap SchemeAnnex <$> v .:? "Annex"
        return $ Store mempty
            & schemes.at "rsync" .~ (mrsync <|> mpath)
            & schemes.at "zfs"   .~ mzfs
            & schemes.at "annex" .~ mannex
      where
        rsyncDefault p = SchemeRsync (Rsync p Nothing [])

    parseJSON _ = errorL "Error parsing Store"

rsyncScheme :: Traversal' Store Rsync
rsyncScheme = schemes.ix "rsync"._SchemeRsync

zfsScheme :: Traversal' Store Zfs
zfsScheme = schemes.ix "zfs"._SchemeZfs

annexScheme :: Traversal' Store Annex
annexScheme = schemes.ix "annex"._SchemeAnnex

data Fileset = Fileset
    { _fsName          :: Text
    , _fsClass         :: Text
    , _fsPriority      :: Int
    , _fsReportMissing :: Bool
    , _stores          :: Map Text Store
    , _otherOptions    :: Map Text Value
    } deriving (Show, Eq)

instance FromJSON Fileset where
    parseJSON (Object v) = Fileset
        <$> v .:  "Name"
        <*> v .:? "Class"         .!= ""
        <*> v .:? "Priority"      .!= 1000
        <*> v .:? "ReportMissing" .!= False
        <*> v .:? "Stores"        .!= mempty
        <*> pure mempty
    parseJSON _ = errorL "Error parsing Fileset"

makeLenses ''Fileset

data BindingCommand = BindingSync | BindingSnapshot
    deriving (Show, Eq)

makePrisms ''BindingCommand

data Binding = Binding
    { _fileset     :: Fileset
    , _source      :: Text
    , _target      :: Text
    , _this        :: Store
    , _that        :: Store
    , _bindCommand :: BindingCommand
    } deriving (Show, Eq)

makeLenses ''Binding

isLocal :: Binding -> Bool
isLocal bnd = bnd^.source == bnd^.target

data StorePath
    = NoPath
    | Pathname { unPathname :: FilePath }
    | ZfsFilesystem FilePath
    | ZfsSnapshot FilePath Int
    | ZfsSnapshotRange FilePath Int Int
    deriving (Show, Eq)

makePrisms ''StorePath

data FullPath
    = LocalPath StorePath
    | RemotePath Text StorePath
    deriving (Show, Eq)

makePrisms ''FullPath

main :: IO ()
main = execParser opts >>= \o ->
    withOptions o execute `finally` stopGlobalPool
  where
    opts = info
        (helper <*> pushmeOpts)
        (fullDesc <> progDesc "" <> header pushmeSummary)

    withOptions :: Options -> (Options -> IO ()) -> IO ()
    withOptions o go = do
        _ <- GHC.Conc.setNumCapabilities (jobs o)
        withStdoutLogging $ do
            setLogLevel $ if verbose o then LevelDebug else LevelInfo
            setLogTimeFormat "%H:%M:%S"
            withStore "main" $ do
                putValue "main" "opts" o
                when (dryRun o || noSync o) $
                    warn' "`--dryrun' specified, no changes will be made!"
                go o

    execute :: Options -> IO ()
    execute o = do
        confD <- getHomePath (".pushme" </> "conf.d")
        exists <- isDirectory confD
        unless exists $
            errorL $ "Please define filesets, "
                <> "using the pattern ~/.pushme/conf.d/<name>.yml"
        fsets <- fmap (M.fromList . map ((^.fsName) &&& id))
            $ runResourceT
            $ sourceDirectory confD
                $= filterC (\n -> extension n == Just "yml")
                $= mapMC (liftIO . (readDataFile :: FilePath -> IO Fileset))
                $$ sinkList
        debug' "Filesets:"
        debug' $ pack (ppShow fsets)
        let bindings = relevantBindings o fsets
        annotated <- mapM (shelly . annotateBinding) bindings
        debug' "Bindings:"
        debug' $ pack (ppShow bindings)
        parallel_ $ map (shelly . applyBinding) annotated
      where
        readDataFile :: FromJSON a => FilePath -> IO a
        readDataFile p = do
            d  <- Data.Yaml.decode <$> BC.readFile (encodeString p)
            case d of
                Nothing -> errorL $ "Failed to read file " <> toTextIgnore p
                Just d' -> return d'

        applyBinding :: Binding -> Sh ()
        applyBinding bnd
            | dump o = liftIO $ printBinding bnd
            | bnd^.bindCommand == BindingSnapshot = snapshotBinding bnd
            | otherwise = syncBinding o bnd

printBinding :: Binding -> IO ()
printBinding bnd = do
    go (bnd^.fileset) (bnd^.this)
    go (bnd^.fileset) (bnd^.that)
  where
    go fs c = putStrLn $ printf "%-12s %s"
        (unpack (fs^.fsName)) (show (c^.schemes))

snapshotBinding :: Binding -> Sh ()
snapshotBinding bnd =
    createSnapshot (isLocal bnd) (bnd^.target) (bnd^.that)
  where
    createSnapshot :: Bool -> Text -> Store -> Sh ()
    createSnapshot isLocal' targ ((^? zfsScheme) -> Just z) = do
        let nextRev      = z^.zfsLastRev.non 1.to succ
            thatSnapshot = z^.zfsPoolPath <> "@" <> decodeString (show nextRev)
            snapCmd      =
                map toTextIgnore $ ["zfs", "snapshot"] <> [thatSnapshot]
        liftIO $ log' $ format "Creating snapshot {}"
            [toTextIgnore thatSnapshot]
        if isLocal'
            then vrun_ "sudo" snapCmd
            else remote vrun_ targ snapCmd
    createSnapshot _ _ _ = return ()

syncBinding :: Options -> Binding -> Sh ()
syncBinding opts bnd = do
    when (bnd^.fileset.fsReportMissing) $
        liftIO $ reportMissingFiles (bnd^.fileset) (bnd^.this)
    syncStores opts bnd

annotateBinding :: Binding -> Sh Binding
annotateBinding bnd =
    case (bnd^?this.zfsScheme, bnd^?that.zfsScheme) of
        (Just z1, Just z2)  -> do
            thisInfo' <- annotateInfo (bnd^.this) z1
            thatInfo' <- annotateInfo (bnd^.that) z2
            return $ this .~ thisInfo'
                   $ that .~ thatInfo' $ bnd
        _ -> return bnd
  where
    annotateInfo :: Store -> Zfs -> Sh Store
    annotateInfo c z = fmap (fromMaybe c) $ runMaybeT $ do
        rev <- MaybeT $ getLastRev z
        return $ c & schemes.traverse._SchemeZfs.zfsLastRev .~ Just rev

    getLastRev :: Zfs -> Sh (Maybe Int)
    getLastRev z = silently $ do
        let p = (z^.zfsPath) </> ".zfs" </> "snapshot"
            h = bnd^.target
        fmap lastMay
              $ sort
            <$> map (read . unpack)
            <$> filter (T.all isDigit)
            <$> T.lines
            <$> if isLocal bnd
                then vrun "ls" ["-1", toTextIgnore p]
                else remote vrun h ["ls", "-1", toTextIgnore p]

relevantBindings :: Options -> Map Text Fileset -> [Binding]
relevantBindings opts fsets
    = sortBy (comparing (^.fileset.fsPriority))
    $ filter matching
    $ catMaybes
    $ createBinding
        <$> M.elems fsets
        <*> pure (pack (fromName opts))
        <*> map pack (cliArgs opts)
  where
    matching bnd =
        let fs = bnd^.fileset
        in (T.null fss || matchText fss (fs^.fsName))
         && (T.null cls || matchText cls (fs^.fsClass))
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
            (here, there) = f (hereRaw, thereRaw)
        Binding
            <$> pure fs
            <*> pure here
            <*> pure there
            <*> fs^.stores.at here
            <*> fs^.stores.at there
            <*> pure (if atsign
                      then BindingSnapshot
                      else BindingSync)

reportMissingFiles :: Fileset -> Store -> IO ()
reportMissingFiles fs cont =
    runResourceT $ sourceDirectory contPath
        =$ mapC (T.drop len . toTextIgnore)
        =$ filterC (\x -> not (any (patMatch x) patterns))
        =$ filterC (`notElem` [ ".DS_Store", ".localized" ])
        $$ mapM_C (\f -> warn' $ format "{}: unknown: \"{}\"" [label, f])
  where
    contPath = unPathname $ getRsyncPath cont

    len = T.length (toTextIgnore contPath)

    rfs = (Success <$> cont^?rsyncScheme.rsyncFilters)
      <|> (fromJSON <$> fs^.otherOptions.at "RsyncFilters")

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

    label = fs^.fsName

getHomePath :: FilePath -> IO FilePath
getHomePath p = (</> p) <$> getHomeDirectory

getPathname :: FilePath -> StorePath
getPathname (toTextIgnore -> fp) =
    Pathname $ fromText $ if T.null fp || T.last fp /= '/'
                          then T.append fp "/"
                          else fp

getRsyncPath :: Store -> StorePath
getRsyncPath = getPathname . (^.rsyncScheme.rsyncPath)

sourcePath :: Binding -> FullPath
sourcePath bnd =
    case (bnd^?this.zfsScheme, bnd^?that.zfsScheme) of
        (Just z1, Just z2)  ->
            let p = z1^.zfsPoolPath
            in case (z1^.zfsLastRev, z2^.zfsLastRev) of
                (Just thisRev, Just thatRev) ->
                    LocalPath $
                        if thisRev > thatRev
                        then ZfsSnapshotRange p thatRev thisRev
                        else NoPath
                (Just thisRev, Nothing) ->
                    LocalPath $ ZfsSnapshot p thisRev
                (Nothing, _) ->
                    LocalPath $ ZfsFilesystem p
        (Just z1, Nothing)  ->
            LocalPath $ Pathname $ z1^.zfsPath
        _ ->
            case bnd^?this.annexScheme of
                Just annex ->
                    LocalPath $ getPathname $ annex^.annexPath
                Nothing ->
                    case bnd^?this.rsyncScheme of
                        Just rsync ->
                            LocalPath $ getPathname $ rsync^.rsyncPath
                        Nothing ->
                            LocalPath NoPath

destinationPath :: Binding -> FullPath
destinationPath bnd =
    case (bnd^?this.zfsScheme, bnd^?that.zfsScheme) of
        (Just _, Just z2)  ->
            buildPath $ ZfsFilesystem (z2^.zfsPoolPath)
        (Nothing, Just z2)  ->
            buildPath $ Pathname $ z2^.zfsPath
        _ ->
            case bnd^?that.annexScheme of
                Just annex ->
                    buildPath $ getPathname $ annex^.annexPath
                Nothing ->
                    case bnd^?that.rsyncScheme of
                        Just rsync ->
                            buildPath $ getPathname $ rsync^.rsyncPath
                        Nothing ->
                            buildPath NoPath
  where
    buildPath
        | isLocal bnd = LocalPath
        | otherwise   = RemotePath (bnd^.target)

syncStores :: Options -> Binding -> Sh ()
syncStores opts bnd = errExit False $ do
  liftIO $ log' $ format "Sending {}/{} -> {}"
                   [ bnd^.source
                   , bnd^.fileset.fsName
                   , bnd^.target ]

  let (sendCmd, recvCmd) = getSyncCommands opts bnd

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

getSyncCommands :: Options -> Binding -> (Sh [Text], Sh [Text])
getSyncCommands opts bnd =
  let verb  = verbose opts
      cpAll = copyAll opts

      thisCont  = bnd^.this
      thatCont  = bnd^.that
      thisAnnex = thisCont^.singular annexScheme
      thatAnnex = thatCont^.singular annexScheme

      annexTarget = thatAnnex^.annexName.non (bnd^.target)
      defaultFlags = [ "--not", "--in", annexTarget ]
      hostAFlags = lookup (bnd^.target) (thisAnnex^.annexFlags)
      annexFlags' =
          flip (maybe defaultFlags) hostAFlags $ \flags ->
              fromMaybe (fromMaybe defaultFlags $ lookup "<default>" flags)
                  $ lookup (bnd^.fileset.fsName) flags

      annexCmds isRemote path' = do
          vrun_ "git-annex" $ ["-q" | not verb]
              <> ["add", "-c", "alwayscommit=false", "."]
          vrun_ "git-annex" $ ["-q" | not verb]
              <> [ "--auto"
                 | not (orOf annexIsPrimary thatAnnex || cpAll) ]
              <> [ "copy", "-c", "alwayscommit=false" ]
              <> annexFlags'
              <> [ "--to", annexTarget ]
          vrun_ "git-annex" $ ["-q" | not verb] <> ["sync"]

          sub $
              if isRemote
              then remote vrun_ (bnd^.target)
                  [ T.concat
                        $ [ "\"cd '", path', "'; git-annex" ]
                       <> [" -q" | not verb]
                       <> [" sync", "\""]
                  ]
              else do
                  cd (fromText path')
                  vrun_ "git-annex" $ ["-q" | not verb] <> ["sync"]

          liftIO $ log' $ format "{}: Git Annex synchronized"
              [ bnd^.fileset.fsName ]

  in case (sourcePath bnd, destinationPath bnd) of
    (LocalPath NoPath, _) ->
        ( do liftIO $ log' "No sync needed"
             return []
        , return []
        )

    (LocalPath (Pathname l), LocalPath (Pathname r)) ->
        ( if   isJust (thisCont^?annexScheme)
             && isJust (thatCont^?annexScheme)
          then (if verb then id else silently)
              $ chdir l
              $ do
                  annexCmds False (toTextIgnore r)
                  return []
          else do
              u2' <- maybe (cmd "whoami") return undefined
              systemVolCopy
                  (u2' /= "root")
                  (bnd^.fileset.fsName)
                  l
                  (toTextIgnore r)
              return []
        , return []
        )

    (LocalPath (Pathname l), RemotePath h (Pathname r)) ->
        ( if   isJust (thisCont^?annexScheme)
             && isJust (thatCont^?annexScheme)
          then escaping False
              $ (if verb then id else silently)
              $ chdir l
              $ do
                  annexCmds True (toTextIgnore r)
                  return []
          else do
              u' <- maybe (cmd "whoami") return undefined
              u2' <- maybe (cmd "whoami") return undefined
              systemVolCopy
                  (u2' /= "root")
                  (bnd^.fileset.fsName)
                  l
                  (format "{}@{}:{}" [u', h, escape (toTextIgnore r)])
              return []
        , return []
        )

    (LocalPath (ZfsFilesystem l), LocalPath (ZfsFilesystem r)) ->
        ( localSend (toTextIgnore l)
        , localReceive (toTextIgnore r)
        )

    (LocalPath (ZfsFilesystem l), RemotePath h (ZfsFilesystem r)) ->
        ( localSend (toTextIgnore l)
        , remoteReceive h (toTextIgnore r)
        )

    (LocalPath (ZfsSnapshot l e), LocalPath (ZfsFilesystem r)) ->
        ( localSendRev l e
        , localReceive (toTextIgnore r)
        )

    (LocalPath (ZfsSnapshot l e), RemotePath h (ZfsFilesystem r)) ->
        ( localSendRev l e
        , remoteReceive h (toTextIgnore r)
        )

    (LocalPath (ZfsSnapshotRange l b e), LocalPath (ZfsFilesystem r)) ->
        ( localSendTwoRevs l b e
        , localReceive (toTextIgnore r)
        )

    (LocalPath (ZfsSnapshotRange l b e), RemotePath h (ZfsFilesystem r)) ->
        ( localSendTwoRevs l b e
        , remoteReceive h (toTextIgnore r)
        )

    (l, r) -> errorL $ format "Unexpected paths {} and {}"
        [ pack (show l), pack (show r) ]

  where
    escape x
        | "\"" `T.isInfixOf` x || " " `T.isInfixOf` x =
            "'" <> T.replace "\"" "\\\"" x <> "'"
        | otherwise =  x

    localSend pool = return $ ["sudo", "zfs", "send"] <> [pool]

    localSendRev poolPath r1 = do
        verb <- getOption verbose
        return $ ["sudo", "zfs", "send"]
                 <> ["-v" | verb]
                 <> [ toTextIgnore poolPath <> "@" <> tshow r1 ]

    localSendTwoRevs poolPath r1 r2 = do
        verb <- getOption verbose
        return $ ["sudo", "zfs", "send"]
                 <> ["-v" | verb]
                 <> [ "-I"
                    , toTextIgnore poolPath <> "@" <> tshow r1
                    , toTextIgnore poolPath <> "@" <> tshow r2 ]

    localReceive pool = return $ ["sudo", "zfs", "recv"] <> ["-F", pool]

    remoteReceive h pool =
        remote (\x xs -> return (toTextIgnore x:xs)) h  $
               ["zfs", "recv"] <> ["-F", pool ]

systemVolCopy :: Bool -> Text -> FilePath -> Text -> Sh ()
systemVolCopy useSudo label src dest = do
  optsFile <- liftIO $ getHomePath (".pushme" </> "filters" </> fromText label)
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
            , "--filter=-p .com.apple.timemachine.supported"
            ]

            <> (if not (null sshCmd)
                then ["--rsh", pack sshCmd]
                else [])
            <> ["-n" | dry]
            <> (if shhh
                then ["--stats"]
                else if verb then ["-P"] else ["-v"])
            <> options
            <> [src, dest]

    rsync <- which "rsync"
    case rsync of
        Nothing -> errorL "Could not find rsync!"
        Just r
            | shhh && not noSy -> silently $ runRsync r toRemote options' den
            | otherwise     -> doCopy drun_ r toRemote useSudo options'

  where
    commaSep :: Int -> Text
    commaSep = fst
        . T.foldr (\x (xs, num :: Int) ->
                               if num /= 0 && num `mod` 3 == 0
                               then (x `T.cons` ',' `T.cons` xs, num + 1)
                               else (x `T.cons` xs, num + 1))
                             ("", 0)
                   . tshow

    field :: Text -> M.Map Text Text -> Integer
    field x stats = fromMaybe 0 $ read . unpack <$> M.lookup x stats

    doCopy f rsync False False os = f rsync os
    doCopy f rsync False True os  = sudo f rsync os
    doCopy f rsync True False os  = f rsync (remoteRsync False rsync:os)
    doCopy f rsync True True os   = asRoot f rsync (remoteRsync True rsync:os)

    runRsync r toRemote os den = do
        output <- doCopy drun r toRemote useSudo os
        let stats = M.fromList
                $ map (fmap (T.filter (/= ',') . (!! 1) . T.words)
                           . T.breakOn ": ")
                $ filter (": " `T.isInfixOf`)
                $ T.lines output
            files = field "Number of files" stats
            sent  = field "Number of files transferred" stats
            total = field "Total file size" stats
            xfer  = field "Total transferred file size" stats
        liftIO $ log' $ format
            ("{}: \ESC[34mSent \ESC[35m{}\ESC[0m\ESC[34m "
                <> "in {} files\ESC[0m (out of {} in {})")
            [ label
            , humanReadable den xfer
            , commaSep (fromIntegral sent)
            , humanReadable den total
            , commaSep (fromIntegral files)
            ]

    remoteRsync useSudo' _ = if useSudo'
                             then "--rsync-path=sudo rsync"
                             else "--rsync-path=rsync"

    asRoot :: (FilePath -> [Text] -> Sh a) -> FilePath -> [Text] -> Sh a
    asRoot f p xs =
        f "sudo" [ "su", "-", "root", "-c"
                 , T.unwords (map (\x -> "\"" <> x <> "\"") xs')
                 ]
        where xs' = toTextIgnore p:xs

    sudo :: (FilePath -> [Text] -> Sh a) -> FilePath -> [Text] -> Sh a
    sudo f p xs = f "sudo" (toTextIgnore p:xs)

remote :: (FilePath -> [Text] -> Sh a) -> Text -> [Text] -> Sh a
remote f host xs = do
    sshCmd <- getOption ssh
    p <- if null sshCmd
        then which "ssh"
        else return $ Just (decodeString sshCmd)
    case p of
        Nothing -> errorL "Could not find ssh!"
        Just r  ->
            let ys = T.words (toTextIgnore r) <> [host] <> xs
            in f (fromText (head ys)) (tail ys)

doRun :: (FilePath -> [Text] -> Sh a)
      -> (Text -> Sh ())
      -> Sh a
      -> Bool
      -> FilePath
      -> [Text]
      -> Sh a
doRun f logf retval heedDry n xs = do
    logf $ format "{} {}" [toTextIgnore n, T.unwords xs]
    dry   <- getOption dryRun
    drier <- getOption noSync
    if drier || (dry && heedDry)
        then retval
        else f n xs

drun :: FilePath -> [Text] -> Sh Text
drun = doRun run (liftIO . debug') (return "") False

drun_ :: FilePath -> [Text] -> Sh ()
drun_ = (void .) . doRun run_ (liftIO . debug') (return ()) False

vrun :: FilePath -> [Text] -> Sh Text
vrun = doRun run (liftIO . log') (return "") True

vrun_ :: FilePath -> [Text] -> Sh ()
vrun_ = (void .) . doRun run_ (liftIO . log') (return ()) True

getOption :: Data a => (a -> b) -> Sh b
getOption opt = liftIO $ do
    opts <- getValue "main" "opts"
    return $ fromJust $ opt <$> opts

matchText :: Text -> Text -> Bool
matchText = flip (=~) `on` unpack

tshow :: Show a => a -> Text
tshow = pack . show

format :: Fmt.Params a => Fmt.Format -> a -> Text
format = (toStrict .) . Fmt.format

humanReadable :: Integer -> Integer -> Text
humanReadable den x =
    pack $ fromJust
          $ f 0 "b"
        <|> f 1 "K"
        <|> f 2 "M"
        <|> f 3 "G"
        <|> f 4 "T"
        <|> f 5 "P"
        <|> f 6 "X"
        <|> Just (printf "%db" x)
  where
    f :: Integer -> String -> Maybe String
    f n s | x < (den^succ n) =
        Just $ printf
            (if n == 0
             then "%d" ++ s
             else "%." ++ show (min 3 (pred n)) ++ "f" ++ s)
            (fromIntegral x / (fromIntegral den^n :: Double))
    f _ _ = Nothing

-- Main.hs (pushme) ends here
