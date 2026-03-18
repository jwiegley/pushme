{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.ParallelIO (parallel, parallel_, stopGlobalPool)
import Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Control.Exception (finally)
import Control.Lens
import Control.Logging
import Control.Monad (guard, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson hiding (Options)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import Data.Function (on)
import Data.List (isInfixOf, isSuffixOf, sortOn, (\\))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (
  fromJust,
  fromMaybe,
  maybeToList,
 )
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.IO as T
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.Traversable (forM)
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Pushme.Options
import System.Directory (
  doesDirectoryExist,
  getHomeDirectory,
  listDirectory,
 )
import System.Exit (ExitCode (..))
import System.FilePath.Posix (
  takeExtension,
  (</>),
 )
import System.IO (hClose, hPutStr, hSetBinaryMode)
import System.IO.Temp (withSystemTempFile)
import System.Process hiding (env)
import Text.Printf (printf)
import Text.Regex.Posix ((=~))
import Text.Show.Pretty (ppShow)

data TransferStatus = TransferSuccess | TransferWarning | TransferError
  deriving (Show, Eq, Ord)

data SyncDirection = Push | Pull
  deriving (Show, Eq, Ord)

data Fileset = Fileset
  { _filesetName :: Text
  , _filesetClasses :: Maybe [Text]
  , _filesetPriority :: Int
  , _filesetStores :: Map Text (FilePath, RsyncOptions)
  , _filesetCommon :: Maybe RsyncOptions
  }
  deriving (Show, Eq)

makeLenses ''Fileset

decodeEnrichedOptions :: Map Text Value -> Parser (FilePath, RsyncOptions)
decodeEnrichedOptions m =
  parseM (m ^. at "Path") >>= \case
    Nothing -> fail "Missing value for Path"
    Just path -> do
      preserveAll <- fromMaybe False <$> parseM (m ^. at "PreserveAttrs")
      (path,)
        <$> ( RsyncOptions
                <$> parseM (m ^. at "Filters")
                <*> parseM (m ^. at "ExtraFilters")
                <*> (fromMaybe False <$> parseM (m ^. at "NoBasicOptions"))
                <*> (fromMaybe False <$> parseM (m ^. at "NoDelete"))
                <*> (fromMaybe preserveAll <$> parseM (m ^. at "PreserveACLs"))
                <*> (fromMaybe preserveAll <$> parseM (m ^. at "PreserveXattrs"))
                <*> (fromMaybe preserveAll <$> parseM (m ^. at "PreserveAtimes"))
                <*> (fromMaybe preserveAll <$> parseM (m ^. at "PreserveCrtimes"))
                <*> (fromMaybe preserveAll <$> parseM (m ^. at "PreserveHardLinks"))
                <*> (fromMaybe preserveAll <$> parseM (m ^. at "PreserveExecutability"))
                <*> (fromMaybe False <$> parseM (m ^. at "ProtectTopLevel"))
                <*> parseM (m ^. at "Options")
                <*> parseM (m ^. at "ReceiveFrom")
                <*> (fromMaybe True <$> parseM (m ^. at "Active"))
            )
 where
  parseM :: (FromJSON a) => Maybe Value -> Parser (Maybe a)
  parseM Nothing = pure Nothing
  parseM (Just v) = parseJSON v

instance FromJSON Fileset where
  parseJSON (Object v) =
    Fileset
      <$> v .: "Name"
      <*> v .:? "Classes"
      <*> v .:? "Priority" .!= 1000
      <*> (v .: "Stores" >>= traverse decodeEnrichedOptions)
      <*> v .:? "Common"
  parseJSON _ = errorL "Error parsing Fileset"

data Host = Host
  { _hostName :: Text
  , _hostMaxJobs :: Int
  }
  deriving (Show, Eq, Ord)

makeLenses ''Host

parseHost :: Text -> Host
parseHost name = case T.split (== '@') name of
  [n] -> Host n 1
  [n, j] -> Host n (read (unpack j))
  _ -> errorL $ "Cannot parse hostname: " <> name

-- | Extract Host from HostRef for compatibility
hostFromRef :: HostRef -> Host
hostFromRef ref =
  let (name, jobs) = ref ^. hostRefActualHost
   in Host name jobs

{- | Parse a host reference from CLI argument, checking aliases first.
Supports @N suffix for overriding MaxJobs on both raw hostnames and aliases.
Examples: "hera@24", "tank@8" (where tank is an alias)
-}
parseHostRef :: Options -> Text -> HostRef
parseHostRef opts name =
  let (baseName, overrideJobs) = case T.split (== '@') name of
        [n] -> (n, Nothing)
        [n, j] -> (n, Just (read (unpack j)))
        _ -> errorL $ "Cannot parse host reference: " <> name
   in case opts ^. optsAliases . at baseName of
        Just alias ->
          let (actualName, actualJobs) = case alias ^. aliasHost of
                h
                  | "@" `T.isInfixOf` h ->
                      let [n, j] = T.split (== '@') h
                       in (n, read (unpack j))
                h -> (h, fromMaybe 1 (alias ^. aliasMaxJobs))
              -- Use override from CLI if provided, otherwise use alias config
              finalJobs = fromMaybe actualJobs overrideJobs
           in HostRef
                { _hostRefLogicalName = baseName
                , _hostRefActualHost = (actualName, finalJobs)
                , _hostRefVariables = alias ^. aliasVariables
                , _hostRefOptions = alias ^. aliasOptions
                }
        Nothing ->
          let host = parseHost baseName
              finalJobs = fromMaybe (host ^. hostMaxJobs) overrideJobs
           in HostRef
                { _hostRefLogicalName = baseName
                , _hostRefActualHost = (baseName, finalJobs)
                , _hostRefVariables = M.empty
                , _hostRefOptions = Nothing
                }

{- | Interpolate all $variable references in a path using the provided variable map.
Variable names must match pattern: $[a-zA-Z_][a-zA-Z0-9_]*
Throws error if a variable is referenced but not defined in the map.
-}
interpolatePath :: Map Text Text -> FilePath -> FilePath
interpolatePath variables path
  | "$" `isInfixOf` path = interpolateText variables path
  | otherwise = path
 where
  -- Regex pattern for variable names: $[a-zA-Z_][a-zA-Z0-9_]*
  varPattern :: String
  varPattern = "\\$[a-zA-Z_][a-zA-Z0-9_]*"

  -- Find all variable references and replace them
  interpolateText :: Map Text Text -> String -> String
  interpolateText vars txt =
    let matches = txt =~ varPattern :: [[String]]
        varRefs = [m | (m : _) <- matches] -- Safe head extraction with pattern match
     in if null varRefs
          then txt
          else foldl (replaceVar vars) txt varRefs

  -- Replace a single variable reference with its value
  replaceVar :: Map Text Text -> String -> String -> String
  replaceVar vars txt varRef =
    let varName = pack $ drop 1 varRef -- Remove leading $ and convert to Text
     in case M.lookup varName vars of
          Nothing ->
            error $
              "Path contains undefined variable "
                <> varRef
                <> " in path: "
                <> path
                <> "\nAvailable variables: "
                <> show (M.keys vars)
          Just value ->
            let result = T.replace (pack varRef) value (pack txt)
             in unpack result

data Binding = Binding
  { _bindingFileset :: Fileset
  , _bindingSourceHost :: HostRef
  , _bindingSourcePath :: FilePath
  , _bindingTargetHost :: HostRef
  , _bindingTargetPath :: FilePath
  , _bindingRsyncOpts :: RsyncOptions
  , _bindingDirection :: SyncDirection
  }
  deriving (Show, Eq)

makeLenses ''Binding

bindingRemoteHost :: Binding -> HostRef
bindingRemoteHost bnd = case bnd ^. bindingDirection of
  Push -> bnd ^. bindingTargetHost
  Pull -> bnd ^. bindingSourceHost

isLocal :: Binding -> Bool
isLocal bnd =
  fst (bnd ^. bindingSourceHost . hostRefActualHost)
    == fst (bnd ^. bindingTargetHost . hostRefActualHost)

remoteHost :: Binding -> Maybe Host
remoteHost bnd
  | isLocal bnd = Nothing
  | otherwise = case bnd ^. bindingDirection of
      Push -> Just (hostFromRef (bnd ^. bindingTargetHost))
      Pull -> Just (hostFromRef (bnd ^. bindingSourceHost))

type App a = ReaderT Options IO a

main :: IO ()
main = withStdoutLogging do
  cmdLineOpts <- getOptions
  configOpts <-
    readYaml
      =<< expandPath (cmdLineOpts ^. optsConfigDir </> "config.yaml")
  let opts = configOpts <> cmdLineOpts

  setLogLevel $ if opts ^. optsVerbose then LevelDebug else LevelInfo
  setLogTimeFormat "%H:%M:%S"

  when (opts ^. optsDryRun) $
    warn' "`--dryrun' specified, no changes will be made!"

  debug' $ "Command-line options: " <> pack (ppShow cmdLineOpts)
  debug' $ "Config file options: " <> pack (ppShow configOpts)
  debug' $ "Initial composite options: " <> pack (ppShow opts)

  runReaderT processBindings opts `finally` stopGlobalPool

processBindings :: App ()
processBindings = do
  opts <- ask
  case opts ^. optsCliArgs of
    host : hosts@(_ : _) -> liftIO do
      fsets <- traverse expandFilesetPaths =<< readFilesets opts
      -- Determine direction: --reverse flag or auto-detect from hostname
      localHostname <-
        T.toLower . T.takeWhile (/= '.') . T.strip . pack
          <$> readProcess "hostname" ["-s"] ""
      let hereRef = parseHostRef opts (pack host)
          allHostRefs = map (parseHostRef opts . pack) hosts
          resolvedName ref =
            T.toLower $ T.takeWhile (/= '.') $ fst (ref ^. hostRefActualHost)
          -- Auto-detect: if first arg is not local, find the local host
          -- among the remaining args and treat as reverse (pull) mode
          (here, remoteRefs, pullMode)
            | opts ^. optsReverse = (hereRef, allHostRefs, True)
            | resolvedName hereRef /= localHostname =
                case filter (\r -> resolvedName r == localHostname) allHostRefs of
                  (localRef : _) ->
                    (localRef, hereRef : filter (/= localRef) allHostRefs, True)
                  [] -> (hereRef, allHostRefs, False)
            | otherwise = (hereRef, allHostRefs, False)
          bindings =
            relevantBindings opts here fsets remoteRefs pullMode
      when pullMode $
        debug' "Reverse mode: pulling from remote hosts"
      debug' $
        "Local host "
          <> fst (here ^. hostRefActualHost)
          <> " with "
          <> tshow (snd (here ^. hostRefActualHost))
          <> (if pullMode then " receiving" else " sending")
          <> " jobs"
      hereSlots <- newQSem (snd (here ^. hostRefActualHost))
      thereSlotsAll <- forM (M.keys bindings) $ \there -> do
        debug' $
          "Remote host "
            <> fst (there ^. hostRefActualHost)
            <> " with "
            <> tshow (snd (there ^. hostRefActualHost))
            <> " receiving jobs"
        newQSem (snd (there ^. hostRefActualHost))
      parallel_ do
        (bnds, thereSlots) <- zip (M.toList bindings) thereSlotsAll
        pure (goHost opts hereSlots thereSlots bnds)
    _ -> log' "Usage: pushme FROM TO..."
 where
  -- Process all bindings for a single destination host
  goHost opts p q (there, bnds) = do
    -- Process all bindings for this host in parallel and collect statuses
    statuses <- parallel (map (go opts p q) bnds)
    -- After all bindings for this host are done, print completion message
    unless (null bnds) $ do
      let overallStatus = maximum statuses -- TransferError > TransferWarning > TransferSuccess
          suffix = case overallStatus of
            TransferSuccess -> ""
            TransferWarning -> " (with warnings)"
            TransferError -> " (with errors)"
          msg = (there ^. hostRefLogicalName) <> " done" <> suffix
          coloredMsg =
            if opts ^. optsNoColor
              then msg
              else case overallStatus of
                TransferSuccess -> "\ESC[32m" <> msg <> "\ESC[0m"
                TransferWarning -> "\ESC[33m" <> msg <> "\ESC[0m"
                TransferError -> "\ESC[31m" <> msg <> "\ESC[0m"
      -- Use log' for completion messages since they're informational
      -- (all transfers done), with color/suffix indicating status
      log' coloredMsg

  go :: Options -> QSem -> QSem -> Binding -> IO TransferStatus
  go opts p q bnd = do
    waitQSem p
    (waitQSem q >> runReaderT (applyBinding bnd) opts)
      `finally` (signalQSem q >> signalQSem p)

  applyBinding :: Binding -> App TransferStatus
  applyBinding bnd = do
    log' $ case bnd ^. bindingDirection of
      Push ->
        "Sending "
          <> (bnd ^. bindingFileset . filesetName)
          <> " → "
          <> (bnd ^. bindingTargetHost . hostRefLogicalName)
      Pull ->
        "Receiving "
          <> (bnd ^. bindingFileset . filesetName)
          <> " ← "
          <> (bnd ^. bindingSourceHost . hostRefLogicalName)
    debug' $ pack (ppShow bnd)
    syncStores
      bnd
      (bnd ^. bindingSourcePath)
      (bnd ^. bindingTargetPath)
      (bnd ^. bindingRsyncOpts)

  relevantBindings ::
    Options ->
    HostRef ->
    Map Text Fileset ->
    [HostRef] ->
    Bool ->
    Map HostRef [Binding]
  relevantBindings opts here fsets hosts pullMode =
    M.map
      (sortOn (^. bindingFileset . filesetPriority))
      (collect bindingRemoteHost bindings)
   where
    bindings :: [Binding]
    bindings = do
      fset <- M.elems fsets
      there <- hosts
      maybeToList $
        if pullMode
          then buildPullBinding fset there
          else buildPushBinding fset there

    buildPushBinding :: Fileset -> HostRef -> Maybe Binding
    buildPushBinding fset there = do
      (src, _) <- fset ^. filesetStores . at (here ^. hostRefLogicalName)
      (dest, destOpts) <- fset ^. filesetStores . at (there ^. hostRefLogicalName)
      guard $
        not
          ( maybe
              False
              (not . (here ^. hostRefLogicalName `elem`))
              (destOpts ^. rsyncReceiveFrom)
          )
      let !srcPath = interpolatePath (here ^. hostRefVariables) src
          !destPath = interpolatePath (there ^. hostRefVariables) dest
          binding =
            Binding
              { _bindingFileset = fset
              , _bindingSourceHost = here
              , _bindingSourcePath = srcPath
              , _bindingTargetHost = there
              , _bindingTargetPath = destPath
              , _bindingRsyncOpts =
                  case opts ^. optsRsyncOpts <> fset ^. filesetCommon of
                    Nothing -> destOpts
                    Just common -> common <> destOpts
              , _bindingDirection = Push
              }
      guard $ isMatching binding
      guard $ destOpts ^. rsyncActive
      pure binding

    buildPullBinding :: Fileset -> HostRef -> Maybe Binding
    buildPullBinding fset there = do
      (srcPath0, srcOpts) <- fset ^. filesetStores . at (there ^. hostRefLogicalName)
      (destPath0, destOpts) <- fset ^. filesetStores . at (here ^. hostRefLogicalName)
      -- ReceiveFrom: does local store allow receiving from remote?
      guard $
        not
          ( maybe
              False
              (not . (there ^. hostRefLogicalName `elem`))
              (destOpts ^. rsyncReceiveFrom)
          )
      let !srcPath = interpolatePath (there ^. hostRefVariables) srcPath0
          !destPath = interpolatePath (here ^. hostRefVariables) destPath0
          binding =
            Binding
              { _bindingFileset = fset
              , _bindingSourceHost = there
              , _bindingSourcePath = srcPath
              , _bindingTargetHost = here
              , _bindingTargetPath = destPath
              , _bindingRsyncOpts =
                  case opts ^. optsRsyncOpts <> fset ^. filesetCommon of
                    Nothing -> destOpts
                    Just common -> common <> destOpts
              , _bindingDirection = Pull
              }
      guard $ isMatching binding
      guard $ srcOpts ^. rsyncActive
      guard $ destOpts ^. rsyncActive
      pure binding

    isMatching :: Binding -> Bool
    isMatching bnd =
      (null fss || any (matchText (fs ^. filesetName)) fss)
        && (null cls || or (matchText <$> cs <*> cls))
     where
      fs = bnd ^. bindingFileset
      cs = fromMaybe [] (fs ^. filesetClasses)
      fss = fromMaybe [] (opts ^. optsFilesets)
      cls = fromMaybe [] (opts ^. optsClasses)

  readFilesets :: Options -> IO (Map Text Fileset)
  readFilesets opts = do
    confD <- expandPath (opts ^. optsConfigDir </> "filesets")
    exists <- doesDirectoryExist confD
    unless exists $
      errorL $
        "Please define filesets, "
          <> "using files named "
          <> pack (opts ^. optsConfigDir)
          <> "filesets/<name>.yaml"
    directoryContents confD
      >>= mapM readYaml . filter (\n -> takeExtension n == ".yaml")
      <&> M.fromList . map ((^. filesetName) &&& id)

checkDirectory :: Binding -> FilePath -> Bool -> App Bool
checkDirectory _ path False =
  liftIO $ doesDirectoryExist path
checkDirectory (isLocal -> True) path True =
  liftIO $ doesDirectoryExist path
checkDirectory bnd path True =
  (ExitSuccess ==) . fstOf3
    <$> execute
      (remoteHost bnd)
      "test"
      ["-d", unpack (escape (pack path))]
 where
  escape :: Text -> Text
  escape x
    | "\"" `T.isInfixOf` x || " " `T.isInfixOf` x =
        "'" <> T.replace "\"" "\\\"" x <> "'"
    | otherwise = x

syncStores :: Binding -> FilePath -> FilePath -> RsyncOptions -> App TransferStatus
syncStores bnd src dest roDest = do
  exists <- case bnd ^. bindingDirection of
    Push ->
      (&&)
        <$> checkDirectory bnd l False
        <*> checkDirectory bnd r True
    Pull ->
      (&&)
        <$> checkDirectory bnd l True
        <*> checkDirectory bnd r False
  if exists
    then invokeRsync bnd l roDest (remoteHost bnd) r
    else liftIO do
      let (localDir, remoteDir) = case bnd ^. bindingDirection of
            Push -> (l, r)
            Pull -> (r, l)
      warn $ "Either local directory missing: " <> pack localDir
      warn $ "OR remote directory missing: " <> pack remoteDir
      pure TransferError
 where
  (asDirectory -> l) = src
  (asDirectory -> r) = dest

invokeRsync ::
  Binding ->
  FilePath ->
  RsyncOptions ->
  Maybe Host ->
  FilePath ->
  App TransferStatus
invokeRsync bnd src roDest host dest = do
  opts <- ask
  withProtected $ \args1 ->
    withFilters "Filters" (combineFilters (roDest ^. rsyncFilters) (roDest ^. rsyncExtraFilters)) $ \args2 ->
      doRsync
        ( bindingRemoteHost bnd ^. hostRefLogicalName
            <> "/"
            <> bnd ^. bindingFileset . filesetName
        )
        (rsyncArguments opts (args1 ++ args2))
 where
  withProtected k
    | roDest ^. rsyncProtectTopLevel =
        k ["--filter", "P /*"]
    | otherwise = k []

  withFilters label fs k = case fs of
    Nothing -> k []
    Just filters -> withSystemTempFile "filters" $ \fpath h -> do
      liftIO do
        T.hPutStr h filters
        hClose h
      debug' $ label <> ":\n" <> filters
      k ["--include-from", pack fpath]

  rsyncArguments :: Options -> [Text] -> [Text]
  rsyncArguments opts args =
    ["-a" | not (roDest ^. rsyncNoBasicOptions)]
      <> ["--delete" | not (roDest ^. rsyncNoDelete)]
      <> ["-A" | roDest ^. rsyncPreserveACLs]
      <> ["-X" | roDest ^. rsyncPreserveXattrs]
      <> ["-U" | roDest ^. rsyncPreserveAtimes]
      <> ["-N" | roDest ^. rsyncPreserveCrtimes]
      <> ["-H" | roDest ^. rsyncPreserveHardLinks]
      <> ["-E" | roDest ^. rsyncPreserveExecutability]
      <> ["-n" | opts ^. optsDryRun]
      <> ( if opts ^. optsVerbose
             then ["-v"]
             else ["--stats"]
         )
      <> args
      <> fromMaybe [] (roDest ^. rsyncOptions)
      <> fromMaybe [] (bindingRemoteHost bnd ^. hostRefOptions)
      <> case bnd ^. bindingDirection of
           Push ->
             [ pack src
             , case host ^? _Just . hostName of
                 Nothing -> pack dest
                 Just h -> h <> ":" <> T.intercalate "\\ " (T.words (pack dest))
             ]
           Pull ->
             [ case host ^? _Just . hostName of
                 Nothing -> pack src
                 Just h -> h <> ":" <> T.intercalate "\\ " (T.words (pack src))
             , pack dest
             ]

doRsync :: Text -> [Text] -> App TransferStatus
doRsync label args = do
  opts <- ask
  (ec, diff, output) <- execute Nothing "rsync" (map unpack args)
  let status = case ec of
        ExitSuccess -> TransferSuccess
        ExitFailure 23 -> TransferWarning -- Partial transfer
        ExitFailure 24 -> TransferWarning -- Vanished source files
        _ -> TransferError
  when (ec == ExitSuccess && not (opts ^. optsDryRun)) $
    if opts ^. optsVerbose
      then liftIO $ putStr output
      else do
        let stats =
              M.fromList
                $ map
                  ( fmap (T.filter (/= ',') . (!! 1) . T.words)
                      . T.breakOn ": "
                  )
                $ filter (": " `T.isInfixOf`)
                $ map pack
                $ lines output
            files = field "Number of files" stats
            sent =
              field "Number of regular files transferred" stats
                <|> field "Number of files transferred" stats
            total = field "Total file size" stats
            xfer = field "Total transferred file size" stats
            den = (\x -> if x then 1000 else 1024) $ opts ^. optsSiUnits
        log' $
          label
            <> ": "
            <> purple
              (opts ^. optsNoColor)
              (humanReadable den (fromMaybe 0 xfer))
            <> cyan
              (opts ^. optsNoColor)
              (" in " <> commaSep (fromIntegral (fromMaybe 0 sent)))
            <> " ("
            <> humanReadable den (fromMaybe 0 total)
            <> " in "
            <> commaSep (fromIntegral (fromMaybe 0 files))
            <> ") "
            <> green
              (opts ^. optsNoColor)
              ("[" <> tshow (round diff :: Int) <> "s]")
  pure status
 where
  field :: Text -> M.Map Text Text -> Maybe Integer
  field x = fmap (read . unpack) . M.lookup x

  colored True _ s = s
  colored False n s = "\ESC[" <> tshow (n :: Int) <> "m" <> s <> "\ESC[0m"
  purple b = colored b 35
  cyan b = colored b 36
  green b = colored b 32

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

execute ::
  Maybe Host ->
  FilePath ->
  [String] ->
  App (ExitCode, NominalDiffTime, String)
execute mhost cmdName args = do
  opts <- ask
  let (name', args') = case mhost of
        Nothing -> (cmdName, args)
        Just h -> remote h (cmdName, args)
      runner p xs =
        liftIO $
          timeFunction (readProcessWithExitCodeLenient p xs "")
  debug' $ pack name' <> " " <> T.intercalate " " (map tshow args')
  (diff, (ec, out, err)) <-
    if opts ^. optsDryRun
      then pure (0, (ExitSuccess, "", ""))
      else runner name' args'
  when (ec /= ExitSuccess) $ do
    let errLines = lines err
        numLines = length errLines
        truncatedErr =
          if numLines > 10
            then
              unlines (take 5 errLines)
                <> "... ("
                <> show (numLines - 10)
                <> " more lines) ...\n"
                <> unlines (drop (numLines - 5) errLines)
            else err
    -- Note: Using warn' instead of errorL' because we want to log the error
    -- but not throw an exception - the transfer status is tracked separately
    warn' $
      "Command failed: "
        <> pack cmdName
        <> " "
        <> pack (ppShow args)
        <> ": "
        <> pack truncatedErr
  pure (ec, diff, out)
 where
  -- Use binary I/O and lenient UTF-8 decoding to handle arbitrary byte sequences
  -- from rsync (e.g., filenames with non-UTF-8 characters)
  readProcessWithExitCodeLenient ::
    FilePath -> [String] -> String -> IO (ExitCode, String, String)
  readProcessWithExitCodeLenient cmd cmdArgs stdin = do
    let cp =
          (proc cmd cmdArgs)
            { std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
    (Just hIn, Just hOut, Just hErr, ph) <- createProcess cp

    -- Set binary mode to avoid encoding issues
    hSetBinaryMode hOut True
    hSetBinaryMode hErr True
    hSetBinaryMode hIn True

    -- Write stdin and close
    hPutStr hIn stdin
    hClose hIn

    -- Read stdout and stderr concurrently to avoid deadlock
    outMVar <- newEmptyMVar
    errMVar <- newEmptyMVar

    _ <- forkIO $ do
      outBytes <- BS.hGetContents hOut
      let outStr = unpack $ TE.decodeUtf8With TEE.lenientDecode outBytes
      putMVar outMVar outStr

    _ <- forkIO $ do
      errBytes <- BS.hGetContents hErr
      let errStr = unpack $ TE.decodeUtf8With TEE.lenientDecode errBytes
      putMVar errMVar errStr

    -- Wait for both threads to finish
    outStr <- takeMVar outMVar
    errStr <- takeMVar errMVar

    exitCode <- waitForProcess ph
    pure (exitCode, outStr, errStr)

  timeFunction :: IO a -> IO (NominalDiffTime, a)
  timeFunction function = do
    startTime <- getCurrentTime
    a <- function
    endTime <- getCurrentTime
    pure (diffUTCTime endTime startTime, a)

  remote :: Host -> (FilePath, [String]) -> (FilePath, [String])
  remote host (p, xs) =
    ( "ssh"
    , unpack (host ^. hostName) : p : xs
    )

-- Utility functions

readYaml :: (FromJSON a) => FilePath -> IO a
readYaml p =
  decodeFileEither p >>= \case
    Left err -> errorL $ pack $ p <> ": " <> prettyPrintParseException err
    Right d -> pure d

expandPath :: FilePath -> IO FilePath
expandPath ['~'] = getHomeDirectory
expandPath ('~' : '/' : p) = (</> p) <$> getHomeDirectory
expandPath p = pure p

expandFilesetPaths :: Fileset -> IO Fileset
expandFilesetPaths fs =
  fs & filesetStores %%~ traverse (\(a, b) -> (,) <$> expandPath a <*> pure b)

directoryContents :: FilePath -> IO [FilePath]
directoryContents p = map (p </>) <$> listDirectory p

lsDirectory :: Maybe Host -> FilePath -> App [FilePath]
lsDirectory mhost path = do
  (_ec, _secs, output) <- execute mhost "ls" ["-1ap", path]
  pure $ lines output \\ ["./", "../"]

asDirectory :: FilePath -> FilePath
asDirectory fp
  | "/" `isSuffixOf` fp = fp
  | otherwise = fp <> "/"

collect :: (Ord b) => (a -> b) -> [a] -> Map b [a]
collect f =
  foldl'
    ( \m x ->
        m
          & at (f x) %~ \case
            Nothing -> Just [x]
            Just xs -> Just (x : xs)
    )
    mempty

fstOf3 :: (a, b, c) -> a
fstOf3 (a, _, _) = a

matchText :: Text -> Text -> Bool
matchText = (=~) `on` unpack

tshow :: (Show a) => a -> Text
tshow = pack . show
