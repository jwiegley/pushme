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
import Control.Concurrent.ParallelIO (parallel_, stopGlobalPool)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (bracket_, finally)
import Control.Lens
import Control.Logging
import Control.Monad (guard, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson hiding (Options)
import Data.Aeson.Types (Parser)
import Data.Function (on)
import Data.List (foldl', isSuffixOf, sortOn, (\\))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
  ( fromJust,
    fromMaybe,
    maybeToList,
  )
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.Traversable (forM)
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Pushme.Options
import System.Directory
  ( doesDirectoryExist,
    getHomeDirectory,
    listDirectory,
  )
import System.Exit (ExitCode (..))
import System.FilePath.Posix
  ( takeExtension,
    (</>),
  )
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process hiding (env)
import Text.Printf (printf)
import Text.Regex.Posix ((=~))
import Text.Show.Pretty (ppShow)

data Fileset = Fileset
  { _filesetName :: Text,
    _filesetClasses :: Maybe [Text],
    _filesetPriority :: Int,
    _filesetStores :: Map Text (FilePath, RsyncOptions),
    _filesetCommon :: Maybe RsyncOptions
  }
  deriving (Show, Eq)

makeLenses ''Fileset

decodeEnrichedOptions :: Map Text Value -> Parser (FilePath, RsyncOptions)
decodeEnrichedOptions m =
  parseM (m ^. at "Path") >>= \case
    Nothing -> fail "Missing value for Path"
    Just path ->
      (path,)
        <$> ( RsyncOptions
                <$> parseM (m ^. at "Filters")
                <*> (fromMaybe False <$> parseM (m ^. at "NoBasicOptions"))
                <*> (fromMaybe False <$> parseM (m ^. at "NoDelete"))
                <*> (fromMaybe False <$> parseM (m ^. at "PreserveAttrs"))
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
  { _hostName :: Text,
    _hostMaxJobs :: Int
  }
  deriving (Show, Eq, Ord)

makeLenses ''Host

parseHost :: Text -> Host
parseHost name = case T.split (== '@') name of
  [n] -> Host n 1
  [n, j] -> Host n (read (unpack j))
  _ -> errorL $ "Cannot parse hostname: " <> name

data Binding = Binding
  { _bindingFileset :: Fileset,
    _bindingSourceHost :: Host,
    _bindingSourcePath :: FilePath,
    _bindingTargetHost :: Host,
    _bindingTargetPath :: FilePath,
    _bindingRsyncOpts :: RsyncOptions
  }
  deriving (Show, Eq)

makeLenses ''Binding

isLocal :: Binding -> Bool
isLocal bnd = bnd ^. bindingSourceHost == bnd ^. bindingTargetHost

remoteHost :: Binding -> Maybe Host
remoteHost bnd
  | isLocal bnd = Nothing
  | otherwise = Just (bnd ^. bindingTargetHost)

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
      let here = parseHost (pack host)
          bindings =
            relevantBindings
              opts
              here
              fsets
              (map (parseHost . pack) hosts)
      debug' $
        "Local host "
          <> here ^. hostName
          <> " with "
          <> tshow (here ^. hostMaxJobs)
          <> " sending jobs"
      hereSlots <- newQSem (here ^. hostMaxJobs)
      thereSlotsAll <- forM (M.keys bindings) $ \there -> do
        debug' $
          "Remote host "
            <> there ^. hostName
            <> " with "
            <> tshow (there ^. hostMaxJobs)
            <> " receiving jobs"
        newQSem (there ^. hostMaxJobs)
      parallel_ do
        (bnds, thereSlots) <- zip (M.elems bindings) thereSlotsAll
        map (go opts hereSlots thereSlots) bnds
    _ -> log' "Usage: pushme FROM TO..."
  where
    go opts p q bnd =
      bracket_ (waitQSem p) (signalQSem p) $
        bracket_ (waitQSem q) (signalQSem q) $
          runReaderT (applyBinding bnd) opts

    applyBinding :: Binding -> App ()
    applyBinding bnd = do
      log' $
        "Sending "
          <> (bnd ^. bindingFileset . filesetName)
          <> " → "
          <> (bnd ^. bindingTargetHost . hostName)
      debug' $ pack (ppShow bnd)
      syncStores
        bnd
        (bnd ^. bindingSourcePath)
        (bnd ^. bindingTargetPath)
        (bnd ^. bindingRsyncOpts)

    relevantBindings ::
      Options ->
      Host ->
      Map Text Fileset ->
      [Host] ->
      Map Host [Binding]
    relevantBindings opts here fsets hosts =
      M.map
        (sortOn (^. bindingFileset . filesetPriority))
        (collect (^. bindingTargetHost) bindings)
      where
        bindings :: [Binding]
        bindings = do
          fset <- M.elems fsets
          there <- hosts
          maybeToList do
            (src, _) <- fset ^. filesetStores . at (here ^. hostName)
            (dest, destOpts) <- fset ^. filesetStores . at (there ^. hostName)
            guard $
              not
                ( maybe
                    False
                    (not . (here ^. hostName `elem`))
                    (destOpts ^. rsyncReceiveFrom)
                )
            let binding =
                  Binding
                    { _bindingFileset = fset,
                      _bindingSourceHost = here,
                      _bindingSourcePath = src,
                      _bindingTargetHost = there,
                      _bindingTargetPath = dest,
                      _bindingRsyncOpts =
                        case opts ^. optsRsyncOpts <> fset ^. filesetCommon of
                          Nothing -> destOpts
                          Just common -> common <> destOpts
                    }
            guard $ isMatching binding
            guard $ destOpts ^. rsyncActive
            pure binding

        isMatching :: Binding -> Bool
        isMatching bnd =
          (null fss || any id (matchText (fs ^. filesetName) <$> fss))
            && (null cls || any id (matchText <$> cs <*> cls))
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

syncStores :: Binding -> FilePath -> FilePath -> RsyncOptions -> App ()
syncStores bnd src dest roDest = do
  exists <-
    (&&)
      <$> checkDirectory bnd l False
      <*> checkDirectory bnd r True
  if exists
    then invokeRsync bnd l roDest (remoteHost bnd) r
    else liftIO do
      warn $ "Either local directory missing: " <> pack l
      warn $ "OR remote directory missing: " <> pack r
  where
    (asDirectory -> l) = src
    (asDirectory -> r) = dest

invokeRsync ::
  Binding ->
  FilePath ->
  RsyncOptions ->
  Maybe Host ->
  FilePath ->
  App ()
invokeRsync bnd src roDest host dest = do
  opts <- ask
  withProtected $ \args1 ->
    withFilters "Filters" (roDest ^. rsyncFilters) $ \args2 ->
      doRsync
        ( bnd ^. bindingTargetHost . hostName
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
        <> ["-AXUNHE" | roDest ^. rsyncPreserveAttrs]
        <> ["-n" | opts ^. optsDryRun]
        <> ( if opts ^. optsVerbose
               then ["-v"]
               else ["--stats"]
           )
        <> args
        <> fromMaybe [] (roDest ^. rsyncOptions)
        <> [ pack src,
             case host ^? _Just . hostName of
               Nothing -> pack dest
               Just h -> h <> ":" <> T.intercalate "\\ " (T.words (pack dest))
           ]

doRsync :: Text -> [Text] -> App ()
doRsync label args = do
  opts <- ask
  (ec, diff, output) <- execute Nothing "rsync" (map unpack args)
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
          timeFunction (readProcessWithExitCode p xs "")
  debug' $ pack name' <> " " <> T.intercalate " " (map tshow args')
  (diff, (ec, out, err)) <-
    if opts ^. optsDryRun
      then pure (0, (ExitSuccess, "", ""))
      else runner name' args'
  unless (ec == ExitSuccess) $
    errorL $
      "Error running command: "
        <> pack cmdName
        <> " "
        <> pack (ppShow args)
        <> ": "
        <> pack err
  pure (ec, diff, out)
  where
    timeFunction :: IO a -> IO (NominalDiffTime, a)
    timeFunction function = do
      startTime <- getCurrentTime
      a <- function
      endTime <- getCurrentTime
      pure (diffUTCTime endTime startTime, a)

    remote :: Host -> (FilePath, [String]) -> (FilePath, [String])
    remote host (p, xs) =
      ( "ssh",
        unpack (host ^. hostName) : p : xs
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
