{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Concurrent.ParallelIO (parallel_, stopGlobalPool)
import Control.Exception
import qualified Control.Foldl as L
import Control.Lens
import Control.Logging
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Aeson hiding (Options)
import qualified Data.ByteString as B (readFile)
import Data.Char (isDigit)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
  ( catMaybes,
    fromJust,
    fromMaybe,
    isNothing,
    mapMaybe,
  )
import Data.Ord (comparing)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Yaml (decodeThrow)
import GHC.Conc (setNumCapabilities)
import Pushme.Options (Options (..), getOptions)
import Safe hiding (at)
import System.Directory
import System.Exit
import System.FilePath.Posix
import System.Process hiding (env)
import Text.Printf (printf)
import Text.Regex.Posix ((=~))

data Rsync = Rsync
  { _rsyncPath :: FilePath,
    _rsyncName :: Maybe Text,
    _rsyncFilters :: Maybe [Text],
    _rsyncReportMissing :: Maybe Bool,
    _rsyncNoLinks :: Maybe Bool,
    _rsyncDeleteExcluded :: Maybe Bool,
    _rsyncSendOnly :: Maybe Bool,
    _rsyncReceiveOnly :: Maybe Bool,
    _rsyncReceiveFrom :: Maybe [Text]
  }
  deriving (Show, Eq)

instance FromJSON Rsync where
  parseJSON (Object v) =
    Rsync
      <$> v .: "Path"
      <*> v .:? "Host"
      <*> v .:? "Filters"
      <*> v .:? "ReportMissing"
      <*> v .:? "NoLinks"
      <*> v .:? "DeleteExcluded"
      <*> v .:? "SendOnly"
      <*> v .:? "ReceiveOnly"
      <*> v .:? "ReceiveFrom"
  parseJSON _ = errorL "Error parsing Rsync"

makeLenses ''Rsync

data Fileset = Fileset
  { _fsName :: Text,
    _fsClass :: Text,
    _fsPriority :: Int,
    _stores :: Map Text Rsync
  }
  deriving (Show, Eq)

makeLenses ''Fileset

fromJSON' :: (FromJSON a) => Value -> a
fromJSON' a = case fromJSON a of
  Error e -> errorL (pack e)
  Success x -> x

instance FromJSON Fileset where
  parseJSON (Object v) = do
    fset <-
      Fileset
        <$> v .: "Name"
        <*> v .:? "Class" .!= ""
        <*> v .:? "Priority" .!= 1000
        <*> v .:? "Stores" .!= mempty
    opts <- v .:? "Options" .!= mempty
    return $ M.foldrWithKey k fset (opts :: Map Text Value)
    where
      k :: Text -> Value -> Fileset -> Fileset
      k "Filters" xs fs' =
        fs'
          & stores . traverse . rsyncFilters
            %~ \case Nothing -> Just (fromJSON' xs); x -> x
      k "ReportMissing" xs fs' =
        fs'
          & stores . traverse . rsyncReportMissing
            %~ \case Nothing -> Just (fromJSON' xs); x -> x
      k "NoLinks" xs fs' =
        fs'
          & stores . traverse . rsyncNoLinks
            %~ \case Nothing -> Just (fromJSON' xs); x -> x
      k "DeleteExcluded" xs fs' =
        fs'
          & stores . traverse . rsyncDeleteExcluded
            %~ \case Nothing -> Just (fromJSON' xs); x -> x
      k "SendOnly" xs fs' =
        fs'
          & stores . traverse . rsyncSendOnly
            %~ \case Nothing -> Just (fromJSON' xs); x -> x
      k "ReceiveOnly" xs fs' =
        fs'
          & stores . traverse . rsyncReceiveOnly
            %~ \case Nothing -> Just (fromJSON' xs); x -> x
      k "ReceiveFrom" xs fs' =
        fs'
          & stores . traverse . rsyncReceiveFrom
            %~ \case Nothing -> Just (fromJSON' xs); x -> x
      k _ _ fs' = fs'
  parseJSON _ = errorL "Error parsing Fileset"

data Host = Host
  { _hostName :: Text,
    _hostAliases :: [Text]
  }
  deriving (Show, Eq)

defaultHost :: Text -> Host
defaultHost n = Host n []

makeLenses ''Host

data BindingCommand = BindingSync | BindingSnapshot
  deriving (Show, Eq)

makePrisms ''BindingCommand

data Binding = Binding
  { _fileset :: Fileset,
    _source :: Host,
    _target :: Host,
    _this :: Rsync,
    _that :: Rsync,
    _bindCommand :: BindingCommand
  }
  deriving (Show, Eq)

makeLenses ''Binding

isLocal :: Binding -> Bool
isLocal bnd =
  bnd ^. source . hostName == bnd ^. target . hostName
    || bnd ^. source . hostName `elem` bnd ^. target . hostAliases

targetHost :: Binding -> Maybe Host
targetHost bnd
  | isLocal bnd = Nothing
  | otherwise = Just (bnd ^. target)

data ExeEnv = ExeEnv
  { exeRemote :: Maybe Host,
    exeCwd :: Maybe FilePath,
    -- | Discard process output.
    exeDiscard :: Bool,
    -- | Look for command with "which".
    exeFindCmd :: Bool
  }

type App a = ReaderT Options IO a

defaultExeEnv :: ExeEnv
defaultExeEnv = ExeEnv Nothing Nothing False True

env :: Binding -> ExeEnv
env bnd = ExeEnv (targetHost bnd) Nothing False True

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
  filePath <- expandPath (configDir opts </> "hosts")
  exists <- doesFileExist filePath
  if exists
    then do
      contents <- readFile filePath
      let linesOfWords = map words (lines contents)
          wordMappings = mapMaybe lineToMapping linesOfWords
      return $ M.fromList (concat wordMappings)
    else return mempty
  where
    lineToMapping :: [String] -> Maybe [(Text, Host)]
    lineToMapping [] = Nothing
    lineToMapping (map T.pack -> (x : xs)) =
      let h = Host x xs
       in Just $ (x, h) : map (,h) xs

directoryContents :: FilePath -> IO [FilePath]
directoryContents topPath = do
  names <- listDirectory topPath
  let properNames =
        filter (`notElem` [".", "..", ".DS_Store", ".localized"]) names
  pure $ map (topPath </>) properNames

readFilesets :: Options -> IO (Map Text Fileset)
readFilesets opts = do
  confD <- expandPath (configDir opts </> "conf.d")
  exists <- doesDirectoryExist confD
  unless exists $
    errorL $
      "Please define filesets, "
        <> "using files named "
        <> T.pack (configDir opts </> "conf.d" </> "<name>.yml")

  fmap (M.fromList . map ((^. fsName) &&& id)) $ do
    contents <- directoryContents confD
    forM (filter (\n -> takeExtension n == ".yml") contents) $
      liftIO . readDataFile

readDataFile :: (FromJSON a) => FilePath -> IO a
readDataFile p = do
  d <- decodeThrow <$> B.readFile p
  case d of
    Nothing -> errorL $ "Failed to read file " <> pack p
    Just d' -> return d'

processBindings :: Options -> Map Text Host -> IO ()
processBindings opts hosts = do
  fsets <- readFilesets opts
  thisHost <- init <$> readProcess "hostname" [] ""
  let dflt = defaultHost (pack (fromName opts))
      here = case dflt of
        Host "" _ -> hosts ^. at (pack thisHost) . non dflt . hostName
        _ -> dflt ^. hostName
  when (T.null here) $
    errorL "Please identify the current host using --from"
  parallel_ $
    map (applyBinding opts) $
      relevantBindings opts here hosts fsets

relevantBindings ::
  Options ->
  Text ->
  Map Text Host ->
  Map Text Fileset ->
  [Binding]
relevantBindings opts thisHost hosts fsets =
  sortBy (comparing (^. fileset . fsPriority))
    . filter matching'
    . catMaybes
    $ createBinding
      <$> M.elems fsets
      <*> pure thisHost
      <*> map pack (cliArgs opts)
  where
    matching' bnd =
      (T.null fss || matchText fss (fs ^. fsName))
        && (T.null cls || matchText cls (fs ^. fsClass))
      where
        fs = bnd ^. fileset
        fss = pack (filesets opts)
        cls = pack (classes opts)

    getHost h = fromMaybe (Host h []) (hosts ^. at h)

    createBinding :: Fileset -> Text -> Text -> Maybe Binding
    createBinding fs hereRaw thereRaw = do
      let atsign = T.head thereRaw == '@'
          f
            | atsign = second T.tail
            | "/" `T.isInfixOf` thereRaw =
                let [b, e] = T.splitOn "/" thereRaw
                 in const (b, e)
            | otherwise = id
          (here, there) = f (hereRaw, thereRaw)
      Binding fs (getHost here) (getHost there)
        <$> fs ^. stores . at here
        <*> fs ^. stores . at there
        <*> pure
          ( if atsign
              then BindingSnapshot
              else BindingSync
          )

applyBinding :: Options -> Binding -> IO ()
applyBinding opts bnd
  | dump opts = printBinding bnd
  | otherwise = runReaderT (syncBinding bnd) opts

printBinding :: Binding -> IO ()
printBinding bnd = do
  go (bnd ^. fileset) (bnd ^. this)
  go (bnd ^. fileset) (bnd ^. that)
  where
    go fs c =
      putStrLn $
        printf
          "%-12s %s"
          (unpack (fs ^. fsName))
          (show c)

syncBinding :: Binding -> App ()
syncBinding bnd = do
  liftIO $
    log' $
      "Sending "
        <> (bnd ^. source . hostName)
        <> "/"
        <> (bnd ^. fileset . fsName)
        <> " -> "
        <> (bnd ^. target . hostName)
  syncStores bnd (bnd ^. this) (bnd ^. that)

checkDirectory :: Binding -> FilePath -> Bool -> App Bool
checkDirectory _ path False = liftIO $ doesDirectoryExist path
checkDirectory (isLocal -> True) path True = liftIO $ doesDirectoryExist path
checkDirectory bnd path True = do
  ec <- execute_ (env bnd) "test" ["-d", escape (pack path)]
  pure $ ec == ExitSuccess

getStorePath :: Binding -> Rsync -> Bool -> FilePath
getStorePath bnd s wantTarget = s ^. rsyncPath

syncStores :: Binding -> Rsync -> Rsync -> App ()
syncStores bnd s1 s2 = do
  exists1 <- checkDirectory bnd l False
  exists2 <- checkDirectory bnd r True
  if exists1 && exists2
    then
      rsync
        bnd
        s1
        l
        s2
        ( case h of
            Nothing -> pack r
            Just targ -> targ <> ":" <> pack r
        )
    else liftIO $ do
      warn $ "Either local directory missing: " <> pack l
      warn $ "OR remote directory missing: " <> pack r
  where
    h = case targetHost bnd of
      Nothing -> Nothing
      Just targ
        | Just n <- s2 ^. rsyncName -> Just n
        | otherwise -> Just (targ ^. hostName)

    (asDirectory -> l) = getStorePath bnd s1 False
    (asDirectory -> r) = getStorePath bnd s2 True

rsync :: Binding -> Rsync -> FilePath -> Rsync -> Text -> App ()
rsync bnd srcRsync src destRsync dest =
  if srcRsync ^. rsyncReceiveOnly . non False
    || destRsync ^. rsyncSendOnly . non False
    || maybe
      False
      (not . (bnd ^. source . hostName `elem`))
      (destRsync ^. rsyncReceiveFrom)
    then do
      opts <- ask
      let analyze = not (verbose opts) && not (noSync opts)
      when analyze $
        liftIO $
          log' $
            (fs ^. fsName)
              <> ": \ESC[34mSkipped: "
              <> ( case srcRsync ^. rsyncReceiveOnly of
                     Just True -> "<- ReceiveOnly"
                     _ ->
                       case destRsync ^. rsyncSendOnly of
                         Just True -> "-> SendOnly"
                         _ ->
                           if maybe
                             False
                             (not . (bnd ^. source . hostName `elem`))
                             (destRsync ^. rsyncReceiveFrom)
                             then "! ReceiveFrom"
                             else "Unknown"
                 )
              <> "\ESC[34m\ESC[0m"
    else do
      let rfs =
            fromMaybe
              []
              ( destRsync ^. rsyncFilters
                  <|> srcRsync ^. rsyncFilters
              )
          nol =
            fromMaybe
              False
              ( srcRsync ^. rsyncNoLinks
                  <|> destRsync ^. rsyncNoLinks
              )
          dex =
            fromMaybe
              False
              ( srcRsync ^. rsyncDeleteExcluded
                  <|> destRsync ^. rsyncDeleteExcluded
              )
          go xs =
            doRsync
              (fs ^. fsName)
              xs
              (pack src)
              dest
              nol
              dex
      case rfs of
        [] -> go []
        filters -> do
          when (srcRsync ^. rsyncReportMissing . non False) $
            liftIO $
              reportMissingFiles fs srcRsync

          tmpDir <- liftIO getTemporaryDirectory
          let fpath = tmpDir </> "filters"
          liftIO $ T.writeFile fpath (T.unlines filters)

          ignoreFile <- liftIO $ expandPath "~/.config/ignore.lst"
          exists <- liftIO $ doesFileExist ignoreFile
          opts <- ask
          when (verbose opts) $ do
            c <- liftIO $ B.readFile fpath
            liftIO $ debug' $ "INCLUDE FROM:\n" <> T.decodeUtf8 c
            when exists $ do
              c' <- liftIO $ B.readFile ignoreFile
              liftIO $ debug' $ "INCLUDE FROM:\n" <> T.decodeUtf8 c'
          go $
            "--include-from=" <> pack fpath
              : ["--include-from=" <> pack ignoreFile | exists]
  where
    fs = bnd ^. fileset

reportMissingFiles :: Fileset -> Rsync -> IO ()
reportMissingFiles fs r = do
  contents <- directoryContents rpath
  forM_
    ( filter (\x -> not (any (matchText x) patterns))
        . map (T.drop len . pack)
        $ contents
    )
    $ \f ->
      warn' $ label <> ": unknown: \"" <> f <> "\""
  where
    label = fs ^. fsName
    rpath = asDirectory (r ^. rsyncPath)
    len = T.length (pack rpath)
    filters = r ^. rsyncFilters . non []

    patterns =
      map regexToGlob $
        filter (`notElem` ["*", "*/", ".*", ".*/"]) $
          map stringify filters

    stringify =
      (\x -> if T.head x == '/' then T.tail x else x)
        . ( \x ->
              if T.index x (T.length x - 1) == '/'
                then T.init x
                else x
          )
        . T.drop 2

    regexToGlob =
      T.replace "].*" "]*"
        . T.replace "*" ".*"
        . T.replace "?" "."
        . T.replace "." "\\."

doRsync :: Text -> [Text] -> Text -> Text -> Bool -> Bool -> App ()
doRsync label options src dest noLinks deleteExcluded = do
  opts <- ask
  let den = (\x -> if x then 1000 else 1024) $ siUnits opts
      sshCmd = ssh opts
      rsyncCmd = rsyncOpt opts
      toRemote = ":" `T.isInfixOf` dest
      args =
        [ "-aHEy", -- jww (2012-09-23): maybe -A too?
        -- , "--fileflags"
          "--delete-after",
          "--ignore-errors",
          "--force",
          "--exclude=/.Caches/",
          "--exclude=/.Spotlight-V100/",
          "--exclude=/.TemporaryItems/",
          "--exclude=/.Trash/",
          "--exclude=/.Trashes/",
          "--exclude=/.fseventsd/",
          "--exclude=/.zfs/",
          "--exclude=/Temporary Items/",
          "--exclude=/Network Trash Folder/",
          "--filter=-p .DS_Store",
          "--filter=-p .localized",
          "--filter=-p .AppleDouble/",
          "--filter=-p .AppleDB/",
          "--filter=-p .AppleDesktop/",
          "--filter=-p .com.apple.timemachine.supported"
        ]
          <> ( if not (null sshCmd)
                 then ["--rsh", pack sshCmd]
                 else []
             )
          <> ["-n" | dryRun opts]
          <> ["--no-links" | noLinks]
          <> ["--delete-excluded" | deleteExcluded]
          <> ["--checksum" | checksum opts]
          <> (if verbose opts then ["-P"] else ["--stats"])
          <> options
          <> [ src,
               if toRemote
                 then T.intercalate "\\ " (T.words dest)
                 else dest
             ]
      analyze = not (verbose opts) && not (noSync opts)
      env' = defaultExeEnv {exeDiscard = not analyze}

  (ec, output) <- execute env' "rsync" args
  when (ec == ExitSuccess && analyze) $ do
    let stats =
          M.fromList
            $ map
              ( fmap (T.filter (/= ',') . (!! 1) . T.words)
                  . T.breakOn ": "
              )
            $ filter (": " `T.isInfixOf`)
            $ T.lines output
        files = field "Number of files" stats
        sent =
          field "Number of regular files transferred" stats
            <|> field "Number of files transferred" stats
        total = field "Total file size" stats
        xfer = field "Total transferred file size" stats
    liftIO $
      log' $
        label
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
    commaSep =
      fst
        . T.foldr
          ( \x (xs, num :: Int) ->
              if num /= 0 && num `mod` 3 == 0
                then (x `T.cons` ',' `T.cons` xs, num + 1)
                else (x `T.cons` xs, num + 1)
          )
          ("", 0)
        . tshow

execute :: ExeEnv -> FilePath -> [Text] -> App (ExitCode, Text)
execute ExeEnv {..} name args = do
  opts <- ask
  cmdName <-
    liftIO $
      if exeFindCmd
        then findCmd name
        else return name
  let (name', args') = (cmdName, args)

      (modifier, name'', args'') = case exeRemote of
        Nothing -> (id, name', args')
        Just h -> remote opts h $ case exeCwd of
          Nothing -> (id, name', args')
          Just cwd ->
            ( id, -- jww (2025-03-23): Was `escaping False`
              unpack $
                T.concat $
                  [ "\"cd ",
                    escape (pack cwd),
                    "; ",
                    escape (pack name'),
                    " "
                  ]
                    <> intersperse " " (map escape args')
                    <> ["\""],
              []
            )
      runner p xs = readProcessWithExitCode p xs ""
      runner' p xs =
        ( case exeCwd of
            Just cwd
              | isNothing exeRemote -> \action -> do
                  opts <- ask
                  liftIO $
                    do
                      cur <- getCurrentDirectory
                      (setCurrentDirectory cwd >> runReaderT action opts)
                        `finally` setCurrentDirectory cur
            _ -> id
        )
          $ modifier
          $ liftIO (runner p xs)
  if dryRun opts || noSync opts
    then return (ExitSuccess, "")
    else do
      let (sshCmd : sshArgs) = words name''
      n <- liftIO $ findCmd sshCmd
      liftIO $
        debug' $
          pack n
            <> " "
            <> T.intercalate
              " "
              (map tshow (map T.pack sshArgs ++ args''))
      (ec, out, _err) <- runner' n (map unpack args'')
      pure (ec, pack out)
  where
    findCmd n
      -- Assume commands with spaces in them are "known"
      | " " `T.isInfixOf` pack n = return n
      | isRelative n = do
          c <- findExecutable n
          case c of
            Nothing -> errorL $ "Failed to find command: " <> pack n
            Just c' -> return c'
      | otherwise = return n

    remote ::
      Options ->
      Host ->
      (App a -> App a, FilePath, [Text]) ->
      (App a -> App a, FilePath, [Text])
    remote opts host (m, p, xs) =
      let sshCmd = ssh opts
       in ( m,
            if null sshCmd then "ssh" else sshCmd,
            host ^. hostName : pack p : xs
          )

    sudoAsRoot :: FilePath -> [Text] -> (FilePath, [Text])
    sudoAsRoot p xs =
      ( "sudo",
        [ "su",
          "-",
          "root",
          "-c",
          -- Pass the argument to su as a single, escaped string.
          T.unwords (map escape (pack p : xs))
        ]
      )

execute_ :: ExeEnv -> FilePath -> [Text] -> App ExitCode
execute_ env' fp args = do
  (ec, _) <- execute env' {exeDiscard = True} fp args
  pure ec

expandPath :: FilePath -> IO FilePath
expandPath ('~' : '/' : p) = (</> p) <$> getHomeDirectory
expandPath p = return p

asDirectory :: FilePath -> FilePath
asDirectory (pack -> fp) =
  unpack $
    if T.null fp || T.last fp /= '/'
      then T.append fp "/"
      else fp

escape :: Text -> Text
escape x
  | "\"" `T.isInfixOf` x || " " `T.isInfixOf` x =
      "'" <> T.replace "\"" "\\\"" x <> "'"
  | otherwise = x

matchText :: Text -> Text -> Bool
matchText x y = unpack x =~ unpack y

tshow :: (Show a) => a -> Text
tshow = pack . show

humanReadable :: Integer -> Integer -> Text
humanReadable den x =
  pack $
    fromJust $
      f 0 "b"
        <|> f 1 "K"
        <|> f 2 "M"
        <|> f 3 "G"
        <|> f 4 "T"
        <|> f 5 "P"
        <|> f 6 "X"
        <|> Just (printf "%db" x)
  where
    f :: Integer -> String -> Maybe String
    f n s
      | x < (den ^ succ n) =
          Just $
            if n == 0
              then printf ("%d" ++ s) x
              else
                printf
                  ("%." ++ show (min 3 (pred n)) ++ "f" ++ s)
                  (fromIntegral x / (fromIntegral den ^ n :: Double))
    f _ _ = Nothing
