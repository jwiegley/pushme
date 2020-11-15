{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent.ParallelIO (stopGlobalPool, parallel_)
import           Control.Exception
import qualified Control.Foldl as L
import           Control.Lens
import           Control.Logging
import           Control.Monad
import           Control.Monad.Trans.Reader
import           Data.Aeson hiding (Options)
import qualified Data.ByteString as B (readFile)
import           Data.Char (isDigit)
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromMaybe, fromJust, isNothing)
import           Data.Monoid ((<>), mempty)
import           Data.Ord (comparing)
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import           Data.Yaml (decodeThrow)
import           GHC.Conc (setNumCapabilities)
import           Pipes as P
import qualified Pipes.Group as P
import qualified Pipes.Prelude as P
import           Pipes.Safe as P hiding (finally)
import qualified Pipes.Text as Text
import qualified Pipes.Text.IO as Text
import           Pushme.Options (Options(..), getOptions)
import           Safe hiding (at)
import           Shelly.Lifted hiding ((</>))
import           System.Directory
import           System.FilePath.Posix
import           Text.Printf (printf)
import           Text.Regex.Posix ((=~))

--import Debug.Trace

data Rsync = Rsync
    { _rsyncPath           :: FilePath
    , _rsyncName           :: Maybe Text
    , _rsyncFilters        :: [Text]
    , _rsyncReportMissing  :: Bool
    , _rsyncNoLinks        :: Bool
    , _rsyncDeleteExcluded :: Bool
    , _rsyncSendOnly       :: Bool
    , _rsyncReceiveOnly    :: Bool
    , _rsyncReceiveFrom    :: Maybe [Text]
    }
    deriving (Show, Eq)

defaultRsync :: FilePath -> Rsync
defaultRsync p = Rsync p Nothing [] False False False False Nothing

instance FromJSON Rsync where
    parseJSON (Object v) = Rsync
        <$> v .:  "Path"
        <*> v .:? "Host"
        <*> v .:? "Filters"        .!= []
        <*> v .:? "ReportMissing"  .!= False
        <*> v .:? "NoLinks"        .!= False
        <*> v .:? "DeleteExcluded" .!= False
        <*> v .:? "SendOnly"       .!= False
        <*> v .:? "ReceiveOnly"    .!= False
        <*> v .:? "ReceiveFrom"    .!= Nothing
    parseJSON _ = errorL "Error parsing Rsync"

makeLenses ''Rsync

data Zfs = Zfs
    { _zfsPath     :: FilePath
    , _zfsPoolPath :: FilePath
    }
    deriving (Show, Eq)

instance FromJSON Zfs where
    parseJSON (Object v) = Zfs
        <$> v .: "Path"
        <*> v .: "PoolPath"
    parseJSON _ = errorL "Error parsing Zfs"

makeLenses ''Zfs

data Annex = Annex
    { _annexPath      :: FilePath
    , _annexName      :: Maybe Text
    , _annexFlags     :: [Text]
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

newtype Store = Store
    { _schemes :: Map Text StorageScheme
    } deriving (Show, Eq)

makeLenses ''Store

instance FromJSON Store where
    parseJSON (Object v) = do
        mpath  <- fmap (SchemeRsync . defaultRsync) <$> v .:? "Path"
        mrsync <- fmap SchemeRsync <$> v .:? "Rsync"
        mzfs   <- fmap SchemeZfs   <$> v .:? "Zfs"
        mannex <- fmap SchemeAnnex <$> v .:? "Annex"
        return $ Store mempty
            & schemes.at "rsync" .~ (mrsync <|> mpath)
            & schemes.at "zfs"   .~ mzfs
            & schemes.at "annex" .~ mannex
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
    , _stores          :: Map Text Store
    } deriving (Show, Eq)

makeLenses ''Fileset

fromJSON' :: FromJSON a => Value -> a
fromJSON' a = case fromJSON a of
    Error e -> errorL (pack e)
    Success x -> x

instance FromJSON Fileset where
    parseJSON (Object v) = do
        fset <- Fileset
            <$> v .:  "Name"
            <*> v .:? "Class"    .!= ""
            <*> v .:? "Priority" .!= 1000
            <*> v .:? "Stores"   .!= mempty
        opts <- v .:? "Options" .!= mempty
        return $ M.foldlWithKey' f fset (opts :: Map Text (Map Text Value))
      where
        f fs "Rsync" =
            M.foldlWithKey' k fs
          where
            k fs' "Filters" xs =
                fs' & stores.traverse.rsyncScheme.rsyncFilters <>~ fromJSON' xs
            k fs' "ReportMissing" xs =
                fs' & stores.traverse.rsyncScheme.rsyncReportMissing &&~ fromJSON' xs
            k fs' "NoLinks" xs =
                fs' & stores.traverse.rsyncScheme.rsyncNoLinks &&~ fromJSON' xs
            k fs' "DeleteExcluded" xs =
                fs' & stores.traverse.rsyncScheme.rsyncDeleteExcluded &&~ fromJSON' xs
            k fs' "SendOnly" xs =
                fs' & stores.traverse.rsyncScheme.rsyncSendOnly &&~ fromJSON' xs
            k fs' "ReceiveOnly" xs =
                fs' & stores.traverse.rsyncScheme.rsyncReceiveOnly &&~ fromJSON' xs
            k fs' "ReceiveFrom" xs =
                fs' & stores.traverse.rsyncScheme.rsyncReceiveFrom <>~ fromJSON' xs
            k fs' _ _ = fs'

        f fs "Zfs"   = const fs

        f fs "Annex" =
            M.foldlWithKey' k fs
          where
            k fs' "Flags" xs =
                fs' & stores.traverse.annexScheme.annexFlags <>~ fromJSON' xs
            k fs' _ _ = fs'

        f fs _       = const fs
    parseJSON _ = errorL "Error parsing Fileset"

data Host = Host
    { _hostName    :: Text
    , _hostAliases :: [Text]
    }
    deriving (Show, Eq)

defaultHost :: Text -> Host
defaultHost n = Host n []

makeLenses ''Host

data BindingCommand = BindingSync | BindingSnapshot
    deriving (Show, Eq)

makePrisms ''BindingCommand

data Binding = Binding
    { _fileset     :: Fileset
    , _source      :: Host
    , _target      :: Host
    , _this        :: Store
    , _that        :: Store
    , _bindCommand :: BindingCommand
    } deriving (Show, Eq)

makeLenses ''Binding

isLocal :: Binding -> Bool
isLocal bnd =
       bnd^.source.hostName == bnd^.target.hostName
    || bnd^.source.hostName `elem` bnd^.target.hostAliases

targetHost :: Binding -> Maybe Host
targetHost bnd | isLocal bnd = Nothing
               | otherwise   = Just (bnd^.target)

data ExeMode = Normal | Sudo | SudoAsRoot

data ExeEnv = ExeEnv
    { exeMode    :: ExeMode
    , exeRemote  :: Maybe Host
    , exeCwd     :: Maybe FilePath
    , exeDiscard :: Bool         -- ^ Discard process output.
    , exeFindCmd :: Bool         -- ^ Look for command with "which".
    }

type App a = ReaderT Options Sh a

defaultExeEnv :: ExeEnv
defaultExeEnv = ExeEnv Normal Nothing Nothing False True

env :: Binding -> ExeEnv
env bnd = ExeEnv Normal (targetHost bnd) Nothing False True

sudoEnv :: Binding -> ExeEnv
sudoEnv bnd = (env bnd) { exeMode = Sudo }

main :: IO ()
main = withStdoutLogging $ do
    opts <- getOptions

    when (dryRun opts || noSync opts) $
        warn' "`--dryrun' specified, no changes will be made!"

    _ <- GHC.Conc.setNumCapabilities (jobs opts)

    setLogLevel $ if verbose opts then LevelDebug else LevelInfo
    setLogTimeFormat "%H:%M:%S"

    hosts <- readHostsFile opts
    processBindings opts hosts `finally` stopGlobalPool

readHostsFile :: Options -> IO (Map Text Host)
readHostsFile opts = do
    hostsFile <- expandPath ((configDir opts) </> "hosts")
    exists <- doesFileExist hostsFile
    if exists
        then do
            hosts <- runSafeT $ P.toListM $
                L.purely P.folds L.mconcat
                    (Text.readFile hostsFile ^. Text.lines)
                >-> P.map (\l -> let (x:xs) = T.words l
                                     h = Host x xs
                                 in (x,h) : map (,h) xs)
            return $ M.fromList (concat hosts)
        else
            return mempty

directoryContents :: FilePath -> Producer FilePath IO ()
directoryContents topPath = do
    names <- lift $ listDirectory topPath
    let properNames =
            filter (`notElem` [".", "..", ".DS_Store", ".localized"]) names
    forM_ properNames $ \name -> yield (topPath </> name)

readFilesets :: Options -> IO (Map Text Fileset)
readFilesets opts = do
    confD <- expandPath (configDir opts </> "conf.d")
    exists <- doesDirectoryExist confD
    unless exists $
        errorL $ "Please define filesets, "
            <> "using files named "
            <> T.pack (configDir opts </> "conf.d" </> "<name>.yml")

    fmap (M.fromList . map ((^.fsName) &&& id))
        $ P.toListM
        $ directoryContents confD
            >-> P.filter (\n -> takeExtension n == ".yml")
            >-> P.mapM (liftIO . readDataFile)

readDataFile :: FromJSON a => FilePath -> IO a
readDataFile p = do
    d  <- decodeThrow <$> B.readFile p
    case d of
        Nothing -> errorL $ "Failed to read file " <> toTextIgnore p
        Just d' -> return d'

processBindings :: Options -> Map Text Host -> IO ()
processBindings opts hosts = do
    fsets    <- readFilesets opts
    thisHost <- T.init <$> shelly (silently $ cmd "hostname")
    let dflt = defaultHost (pack (fromName opts))
        here = case dflt of
            Host "" _ -> hosts^.at thisHost.non dflt.hostName
            _ -> dflt^.hostName
    when (T.null here) $
        errorL "Please identify the current host using --from"
    parallel_
        $ map (applyBinding opts)
        $ relevantBindings opts here hosts fsets

relevantBindings :: Options -> Text -> Map Text Host -> Map Text Fileset
                 -> [Binding]
relevantBindings opts thisHost hosts fsets
    = sortBy (comparing (^.fileset.fsPriority))
    . filter matching'
    . catMaybes
    $ createBinding
        <$> M.elems fsets
        <*> pure thisHost
        <*> map pack (cliArgs opts)
  where
    matching' bnd =
           (T.null fss || matchText fss (fs^.fsName))
         && (T.null cls || matchText cls (fs^.fsClass))
      where
        fs  = bnd^.fileset
        fss = pack (filesets opts)
        cls = pack (classes opts)

    getHost h = fromMaybe (Host h []) (hosts^.at h)

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
            <*> pure (getHost here)
            <*> pure (getHost there)
            <*> fs^.stores.at here
            <*> fs^.stores.at there
            <*> pure (if atsign
                      then BindingSnapshot
                      else BindingSync)

applyBinding :: Options -> Binding -> IO ()
applyBinding opts bnd
    | dump opts =
        printBinding bnd
    | bnd^.bindCommand == BindingSnapshot =
        shelly $ runReaderT (snapshotBinding bnd) opts
    | otherwise =
        shelly $ silently $ runReaderT (syncBinding bnd) opts

printBinding :: Binding -> IO ()
printBinding bnd = do
    go (bnd^.fileset) (bnd^.this)
    go (bnd^.fileset) (bnd^.that)
  where
    go fs c = putStrLn $ printf "%-12s %s"
        (unpack (fs^.fsName)) (show (c^.schemes))

snapshotBinding :: Binding -> App ()
snapshotBinding bnd@((^? that.zfsScheme) -> Just z) = do
    mrev <- determineLastRev (env bnd) z
    let nextRev = maybe 1 succ mrev
        thatSnapshot =
            toTextIgnore $ z^.zfsPoolPath <> "@" <> show nextRev
    liftIO $ log' $ "Creating snapshot " <> thatSnapshot
    execute_ (env bnd) "zfs" ["snapshot", thatSnapshot]
snapshotBinding _ = return ()

syncBinding :: Binding -> App ()
syncBinding bnd = errExit False $ do
    liftIO $ log' $ "Sending "
        <> (bnd^.source.hostName)
        <> "/"
        <> (bnd^.fileset.fsName)
        <> " -> "
        <> (bnd^.target.hostName)
    syncStores bnd (bnd^.this) (bnd^.that)

syncStores :: Binding -> Store -> Store -> App ()
syncStores bnd ((^? annexScheme) -> Just a1) ((^? annexScheme) -> Just a2) =
    syncAnnexSchemes bnd a1 a2
syncStores bnd ((^? zfsScheme) -> Just z1) ((^? zfsScheme) -> Just z2) =
    syncZfsSchemes bnd z1 z2
syncStores bnd s1 s2 = syncUsingRsync bnd s1 s2

checkDirectory :: Binding -> FilePath -> Bool -> App Bool
checkDirectory _ path False  = test_d path
checkDirectory (isLocal -> True) path True = test_d path
checkDirectory bnd path True = do
    execute_ (env bnd) "test" ["-d", escape (toTextIgnore path)]
    (== 0) <$> lastExitCode

getStorePath :: Binding -> Store -> Bool -> Maybe FilePath
getStorePath bnd s wantTarget
    =   (s^?rsyncScheme.rsyncPath)
    <|> (s^?zfsScheme.zfsPath)
    <|> (s^?annexScheme.annexPath)
    <|> errorL ("Could not find path for "
                <> ((if wantTarget
                     then bnd^.target
                     else bnd^.source)^.hostName)
                <> "/" <> (bnd^.fileset.fsName))

syncAnnexSchemes :: Binding -> Annex -> Annex -> App ()
syncAnnexSchemes bnd a1 a2 = do
    opts <- ask
    exists1 <- checkDirectory bnd (a1^.annexPath) False
    exists2 <- checkDirectory bnd (a2^.annexPath) True
    if exists1 && exists2
        then do
        let runner1_ = execute_ $
                (env bnd) { exeCwd    = Just (a1^.annexPath)
                          , exeRemote = Nothing
                          }
            runner2_ = execute_ $
                (env bnd) { exeCwd     = Just (a2^.annexPath)
                          , exeFindCmd = isLocal bnd
                          }

        -- Add, copy, and sync from the source.
        runner1_ "git-annex" $ ["-q" | not (verbose opts)]
            <> ["add", "-c", "alwayscommit=false", "."]
        runner1_ "git-annex" $ ["-q" | not (verbose opts)]
            <> [ "--auto"
               | not (a2^.annexIsPrimary || copyAll opts) ]
            <> [ "copy", "-c", "alwayscommit=false" ]
            <> [ "--not", "--in", annexTarget ]
            <> a1^.annexFlags
            <> [ "--to", annexTarget ]
        runner1_ "git-annex" $ ["-q" | not (verbose opts)] <> ["sync"]

        -- Sync to the destination.
        runner2_ "git-annex" $ ["-q" | not (verbose opts)] <> ["sync"]

        liftIO $ log' $ (bnd^.fileset.fsName) <> ": Git Annex synchronized"

        else liftIO $ warn $ "Remote directory missing: "
                 <> toTextIgnore (a2^.annexPath)
  where
    annexTarget = a2^.annexName.non (bnd^.target.hostName)

syncZfsSchemes :: Binding -> Zfs -> Zfs -> App ()
syncZfsSchemes bnd z1 z2 = do
    exists1 <- checkDirectory bnd (z1^.zfsPath) False
    exists2 <- checkDirectory bnd (z2^.zfsPath) True
    if exists1 && exists2
        then do
        rev1 <- determineLastRev (env bnd) { exeRemote = Nothing } z1
        rev2 <- determineLastRev (env bnd) z2
        opts <- ask
        let p = z1^.zfsPoolPath
            r = toTextIgnore (z2^.zfsPoolPath)
            msendArgs = case (rev1, rev2) of
                (Just thisRev, Just thatRev) ->
                    if thisRev > thatRev
                    then Just $ sendTwoRevs opts p thatRev thisRev
                    else Nothing
                (Just thisRev, Nothing) ->
                    Just $ sendRev opts p thisRev
                (Nothing, _) ->
                    Just $ send (toTextIgnore p)
            env'' = defaultExeEnv { exeMode = Sudo }

        case msendArgs of
            Nothing      -> liftIO $ warn "Remote has newer snapshot revision"
            Just (c, xs) ->
                execute_ env'' c $ xs <> ["|", "zfs", "recv", "-F", r]

        else liftIO $ warn $ "Remote directory missing: "
                 <> toTextIgnore (z2^.zfsPath)
  where
    send pool = ("zfs", ["send", pool])

    sendRev opts poolPath r1 =
        ("zfs",
         ["send"]
         <> ["-v" | verbose opts]
         <> [ toTextIgnore poolPath <> "@" <> tshow r1 ])

    sendTwoRevs opts poolPath r1 r2 =
        ("zfs",
         ["send"]
         <> ["-v" | verbose opts]
         <> [ "-I"
            , toTextIgnore poolPath <> "@" <> tshow r1
            , toTextIgnore poolPath <> "@" <> tshow r2
            ])

determineLastRev :: ExeEnv -> Zfs -> App (Maybe Int)
determineLastRev env' zfs = do
    let p = toTextIgnore $ (zfs^.zfsPath) </> ".zfs" </> "snapshot"
    fmap lastMay
          $ sort
          . map (read . unpack)
          . filter (T.all isDigit)
          . T.lines
        <$> execute env' "ls" ["-1", p]

syncUsingRsync :: Binding -> Store -> Store -> App ()
syncUsingRsync bnd s1 s2 = do
    exists1 <- checkDirectory bnd l False
    exists2 <- checkDirectory bnd r True
    if exists1 && exists2
        then
        rsync
            bnd
            (fromMaybe (defaultRsync l) (s1^?rsyncScheme))
            l
            (fromMaybe (defaultRsync r) (s2^?rsyncScheme))
            (case h of
                  Nothing   -> toTextIgnore r
                  Just targ -> targ <> ":" <> toTextIgnore r)

        else do
            liftIO $ warn $ "Either local directory missing: " <> toTextIgnore l
            liftIO $ warn $ "OR remote directory missing: " <> toTextIgnore r
  where
    h = case targetHost bnd of
        Nothing -> Nothing
        Just targ
            | Just (Just n) <- s2^?rsyncScheme.rsyncName -> Just n
            | otherwise -> Just (targ^.hostName)

    Just (asDirectory -> l) = getStorePath bnd s1 False
    Just (asDirectory -> r) = getStorePath bnd s2 True

rsync :: Binding -> Rsync -> FilePath -> Rsync -> Text -> App ()
rsync bnd srcRsync src destRsync dest =
    if srcRsync^.rsyncReceiveOnly
        || destRsync^.rsyncSendOnly
        || maybe False (not . (bnd^.source.hostName `elem`))
                       (destRsync^.rsyncReceiveFrom)
    then do
        opts <- ask
        let analyze = not (verbose opts) && not (noSync opts)
        when analyze $
            liftIO $ log' $ (fs^.fsName)
                <> ": \ESC[34mSkipped: "
                <> (if srcRsync^.rsyncReceiveOnly
                    then "<- ReceiveOnly"
                    else if destRsync^.rsyncSendOnly
                       then "-> SendOnly"
                       else if maybe False (not . (bnd^.source.hostName `elem`))
                                           (destRsync^.rsyncReceiveFrom)
                            then "! ReceiveFrom"
                            else "Unknown")
                <> "\ESC[34m\ESC[0m"
    else do
        let rfs   = (srcRsync^.rsyncFilters) <> (destRsync^.rsyncFilters)
            nol   = (srcRsync^.rsyncNoLinks) || (destRsync^.rsyncNoLinks)
            dex   = (srcRsync^.rsyncDeleteExcluded) || (destRsync^.rsyncDeleteExcluded)
            go xs = doRsync (fs^.fsName) xs (toTextIgnore src) dest nol dex
        case rfs of
            [] -> go []

            filters -> do
                when (srcRsync^.rsyncReportMissing) $
                    liftIO $ reportMissingFiles fs srcRsync

                withTmpDir $ \p -> do
                    let fpath = p </> "filters"
                    writefile fpath (T.unlines filters)
                    go ["--include-from=" <> toTextIgnore fpath]
  where
    fs = bnd^.fileset

reportMissingFiles :: Fileset -> Rsync -> IO ()
reportMissingFiles fs r =
    runEffect
        $ for (directoryContents rpath
               >-> P.map (T.drop len . toTextIgnore)
               >-> P.catch (P.filter (\x -> not (any (matchText x) patterns)))
                           (\(_ :: SomeException) -> P.cat))
        $ \f -> liftIO $ warn' $ label <> ": unknown: \"" <> f <> "\""
  where
    label   = fs^.fsName
    rpath   = asDirectory (r^.rsyncPath)
    len     = T.length (toTextIgnore rpath)
    filters = r^.rsyncFilters

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

doRsync :: Text -> [Text] -> Text -> Text -> Bool -> Bool -> App ()
doRsync label options src dest noLinks deleteExcluded = do
    opts <- ask
    let den      = (\x -> if x then 1000 else 1024) $ siUnits opts
        sshCmd   = ssh opts
        rsyncCmd = rsyncOpt opts
        toRemote = ":" `T.isInfixOf` dest
        args     =
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
            <> ["-n" | dryRun opts]
            <> ["--no-links" | noLinks]
            <> ["--delete-excluded" | deleteExcluded]
            <> ["--checksum" | checksum opts]
            <> (if verbose opts then ["-P"] else ["--stats"])
            <> [pack ("--rsync-path=sudo " ++ if not (null rsyncCmd)
                                              then rsyncCmd
                                              else "rsync") | toRemote]
            <> options
            <> [src, if toRemote
                      then T.intercalate "\\ " (T.words dest)
                      else dest]
        analyze = not (verbose opts) && not (noSync opts)
        env' =  defaultExeEnv
            { exeMode    = if toRemote then SudoAsRoot else Sudo
            , exeDiscard = not analyze
            }

    output <- execute env' "rsync" args
    when analyze $ do
        let stats = M.fromList
                $ map (fmap (T.filter (/= ',') . (!! 1) . T.words)
                           . T.breakOn ": ")
                $ filter (": " `T.isInfixOf`)
                $ T.lines output
            files = field "Number of files" stats
            sent  = field "Number of regular files transferred" stats
                <|> field "Number of files transferred" stats
            total = field "Total file size" stats
            xfer  = field "Total transferred file size" stats
        liftIO $ log' $ label
            <> ": \ESC[34mSent \ESC[35m"
            <> humanReadable den (fromMaybe 0 xfer)
            <> "\ESC[0m\ESC[34m in "
            <> commaSep (fromIntegral (fromMaybe 0 sent))
            <> " files\ESC[0m (out of "
            <> humanReadable den (fromMaybe 0 total)
            <> " in "
            <> commaSep (fromIntegral (fromMaybe 0 files))
            <> ")"
  where
    field :: Text -> M.Map Text Text -> Maybe Integer
    field x stats = read . unpack <$> M.lookup x stats

    commaSep :: Int -> Text
    commaSep = fst
        . T.foldr (\x (xs, num :: Int) ->
                    if num /= 0 && num `mod` 3 == 0
                    then (x `T.cons` ',' `T.cons` xs, num + 1)
                    else (x `T.cons` xs, num + 1)) ("", 0)
        . tshow

execute :: ExeEnv -> FilePath -> [Text] -> App Text
execute ExeEnv {..} name args = do
    opts    <- ask
    cmdName <- (if exeFindCmd then findCmd else return) name
    let (name', args') = case exeMode of
            Normal     -> (cmdName, args)
            Sudo       -> ("sudo", toTextIgnore cmdName:args)
            SudoAsRoot -> sudoAsRoot cmdName args

        (modifier, name'', args'') = case exeRemote of
            Nothing -> (id, name', args')
            Just h  -> remote opts h $ case exeCwd of
                Nothing  -> (id, name', args')
                Just cwd ->
                    (escaping False, fromText $ T.concat $
                         [ "\"cd "
                         , escape (toTextIgnore cwd)
                         , "; "
                         , escape (toTextIgnore name')
                         , " "
                         ]
                        <> intersperse " " (map escape args')
                        <> ["\""], [])
        runner p xs
            | exeDiscard = run_ p xs >> return ""
            | otherwise  = run p xs
        runner' p xs =
            (case exeCwd of
                  Just cwd | isNothing exeRemote -> chdir cwd
                  _ -> id) $ modifier $ runner p xs
    if dryRun opts || noSync opts
        then return ""
        else do
            let (sshCmd:sshArgs) = words name''
            n <- findCmd sshCmd
            liftIO $ debug' $ toTextIgnore n
               <> " "
               <> T.intercalate " "
                    (map tshow (map T.pack sshArgs ++ args''))
            runner' n args''
  where
    findCmd n
        -- Assume commands with spaces in them are "known"
        | " " `T.isInfixOf` toTextIgnore n = return n
        | isRelative n = do
            c <- which n
            case c of
                Nothing -> errorL $ "Failed to find command: " <> toTextIgnore n
                Just c' -> return c'
        | otherwise  = return n

    remote :: Options -> Host -> (App a -> App a, FilePath, [Text])
           -> (App a -> App a, FilePath, [Text])
    remote opts host (m, p, xs) =
        let sshCmd = ssh opts
        in (m, if null sshCmd then "ssh" else sshCmd,
            host^.hostName : toTextIgnore p : xs)

    sudoAsRoot :: FilePath -> [Text] -> (FilePath, [Text])
    sudoAsRoot p xs =
        ("sudo", [ "su", "-", "root", "-c"
                 -- Pass the argument to su as a single, escaped string.
                 , T.unwords (map escape (toTextIgnore p:xs))
                 ])

execute_ :: ExeEnv -> FilePath -> [Text] -> App ()
execute_ env' fp args = void $ execute env' { exeDiscard = True } fp args

expandPath :: FilePath -> IO FilePath
expandPath ('~':'/':p) = (</> p) <$> getHomeDirectory
expandPath p = return p

asDirectory :: FilePath -> FilePath
asDirectory (toTextIgnore -> fp) =
    fromText $ if T.null fp || T.last fp /= '/'
               then T.append fp "/"
               else fp

escape :: Text -> Text
escape x
    | "\"" `T.isInfixOf` x || " " `T.isInfixOf` x =
        "'" <> T.replace "\"" "\\\"" x <> "'"
    | otherwise = x

matchText :: Text -> Text -> Bool
matchText x y = unpack x =~ unpack y

tshow :: Show a => a -> Text
tshow = pack . show

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
        Just $ if n == 0
               then printf ("%d" ++ s) x
               else printf ("%." ++ show (min 3 (pred n)) ++ "f" ++ s)
                   (fromIntegral x / (fromIntegral den^n :: Double))
    f _ _ = Nothing

-- Main.hs (pushme) ends here
