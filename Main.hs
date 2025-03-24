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

import Control.Applicative
import Control.Arrow
import Control.Concurrent.ParallelIO (parallel_, stopGlobalPool)
import Control.Exception
import Control.Lens
import Control.Logging
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Aeson hiding (Options)
import qualified Data.ByteString as B (readFile)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
  ( catMaybes,
    fromJust,
    fromMaybe,
    isNothing,
  )
import Data.Ord (comparing)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Data.Yaml (decodeThrow)
import GHC.Conc (setNumCapabilities)
import Pushme.Options (Options (..), getOptions)
import System.Directory
import System.Exit
import System.FilePath.Posix
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process hiding (env)
import Text.Printf (printf)
import Text.Regex.Posix ((=~))
import Text.Show.Pretty (ppShow)

data Rsync = Rsync
  { _rsyncPath :: FilePath,
    _rsyncName :: Maybe Text,
    _rsyncFilters :: Maybe [Text],
    _rsyncNoLinks :: Maybe Bool,
    _rsyncPreserveAttrs :: Maybe Bool,
    _rsyncDeleteExcluded :: Maybe Bool,
    _rsyncReceiveFrom :: Maybe [Text]
  }
  deriving (Show, Eq)

instance FromJSON Rsync where
  parseJSON (Object v) =
    Rsync
      <$> v .: "Path"
      <*> v .:? "Host"
      <*> v .:? "Filters"
      <*> v .:? "NoLinks"
      <*> v .:? "PreserveAttrs"
      <*> v .:? "DeleteExcluded"
      <*> v .:? "ReceiveFrom"
  parseJSON _ = errorL "Error parsing Rsync"

makeLenses ''Rsync

data RsyncOptions = RsyncOptions
  { _roSource :: Text,
    _roDest :: Text,
    _roNoLinks :: Bool,
    _roPreserveAttrs :: Bool,
    _roDeleteExcluded :: Bool
  }
  deriving (Show, Eq)

makeLenses ''RsyncOptions

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
      k "NoLinks" xs fs' =
        fs'
          & stores . traverse . rsyncNoLinks
            %~ \case Nothing -> Just (fromJSON' xs); x -> x
      k "PreserveAttrs" xs fs' =
        fs'
          & stores . traverse . rsyncPreserveAttrs
            %~ \case Nothing -> Just (fromJSON' xs); x -> x
      k "DeleteExcluded" xs fs' =
        fs'
          & stores . traverse . rsyncDeleteExcluded
            %~ \case Nothing -> Just (fromJSON' xs); x -> x
      k "ReceiveFrom" xs fs' =
        fs'
          & stores . traverse . rsyncReceiveFrom
            %~ \case Nothing -> Just (fromJSON' xs); x -> x
      k _ _ fs' = fs'
  parseJSON _ = errorL "Error parsing Fileset"

type Host = Text

data Binding = Binding
  { _fileset :: Fileset,
    _source :: Host,
    _target :: Host,
    _this :: Rsync,
    _that :: Rsync
  }
  deriving (Show, Eq)

makeLenses ''Binding

isLocal :: Binding -> Bool
isLocal bnd = bnd ^. source == bnd ^. target

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
  config <- expandPath "~/.config/pushme/config.yaml"
  opts' <- readYaml config
  opts <- (opts' <>) <$> getOptions

  when (verbose opts) $
    debug' $
      pack (ppShow opts)
  when (dryRun opts) $
    warn' "`--dryrun' specified, no changes will be made!"

  forM_ (jobs opts) GHC.Conc.setNumCapabilities

  setLogLevel $ if verbose opts then LevelDebug else LevelInfo
  setLogTimeFormat "%H:%M:%S"

  processBindings opts `finally` stopGlobalPool

directoryContents :: FilePath -> IO [FilePath]
directoryContents topPath = do
  names <- listDirectory topPath
  let properNames =
        filter (`notElem` [".", "..", ".DS_Store", ".localized"]) names
  pure $ map (topPath </>) properNames

readFilesets :: IO (Map Text Fileset)
readFilesets = do
  confD <- expandPath "~/.config/pushme/conf.d"
  exists <- doesDirectoryExist confD
  unless exists $
    errorL $
      "Please define filesets, "
        <> "using files named "
        <> "~/.config/pushme/conf.d/<name>.yaml"
  fmap (M.fromList . map ((^. fsName) &&& id)) $ do
    contents <- directoryContents confD
    forM (filter (\n -> takeExtension n == ".yaml") contents) $
      liftIO . readYaml

readYaml :: (FromJSON a) => FilePath -> IO a
readYaml p = do
  d <- decodeThrow <$> B.readFile p
  case d of
    Nothing -> errorL $ "Failed to read file " <> pack p
    Just d' -> return d'

processBindings :: Options -> IO ()
processBindings opts = case cliArgs opts of
  here : hosts@(_ : _) -> do
    fsets <- readFilesets
    parallel_ $
      map (applyBinding opts) $
        relevantBindings opts (pack here) fsets (map pack hosts)
  _ -> errorL $ "Usage: pushme FROM TO..."

relevantBindings ::
  Options ->
  Text ->
  Map Text Fileset ->
  [Text] ->
  [Binding]
relevantBindings opts thisHost fsets hosts =
  sortBy (comparing (^. fileset . fsPriority))
    . filter isMatching
    . catMaybes
    $ createBinding
      <$> M.elems fsets
      <*> pure thisHost
      <*> hosts -- uses the list monad
  where
    isMatching bnd =
      (T.null fss || matchText fss (fs ^. fsName))
        && (T.null cls || matchText cls (fs ^. fsClass))
      where
        fs = bnd ^. fileset
        fss = pack (fromMaybe "" (filesets opts))
        cls = pack (fromMaybe "" (classes opts))

    createBinding :: Fileset -> Text -> Text -> Maybe Binding
    createBinding fs here there = do
      srcRsync <- fs ^. stores . at here
      destRsync <- fs ^. stores . at there
      if maybe
        False
        (not . (here `elem`))
        (destRsync ^. rsyncReceiveFrom)
        then mzero
        else pure $ Binding fs here there srcRsync destRsync

applyBinding :: Options -> Binding -> IO ()
applyBinding opts bnd = runReaderT go opts
  where
    go :: App ()
    go = do
      liftIO $
        log' $
          "Sending "
            <> (bnd ^. source)
            <> "/"
            <> (bnd ^. fileset . fsName)
            <> " -> "
            <> (bnd ^. target)
      syncStores bnd (bnd ^. this) (bnd ^. that)

checkDirectory :: Binding -> FilePath -> Bool -> App Bool
checkDirectory _ path False =
  liftIO $ doesDirectoryExist path
checkDirectory (isLocal -> True) path True =
  liftIO $ doesDirectoryExist path
checkDirectory bnd path True =
  (ExitSuccess ==)
    <$> execute_ (env bnd) "test" ["-d", escape (pack path)]

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
        | otherwise -> Just targ

    (asDirectory -> l) = s1 ^. rsyncPath
    (asDirectory -> r) = s2 ^. rsyncPath

rsync :: Binding -> Rsync -> FilePath -> Rsync -> Text -> App ()
rsync bnd srcRsync src destRsync dest = do
  let go xs =
        doRsync
          (fs ^. fsName)
          xs
          RsyncOptions
            { _roSource = pack src,
              _roDest = dest,
              _roNoLinks =
                fromMaybe
                  False
                  ( (||)
                      <$> srcRsync ^. rsyncNoLinks
                      <*> destRsync ^. rsyncNoLinks
                  ),
              _roPreserveAttrs =
                fromMaybe
                  False
                  ( (&&)
                      <$> srcRsync ^. rsyncPreserveAttrs
                      <*> destRsync ^. rsyncPreserveAttrs
                  ),
              _roDeleteExcluded =
                fromMaybe
                  False
                  ( destRsync ^. rsyncDeleteExcluded
                  )
            }
      rfs =
        fromMaybe
          []
          ( destRsync ^. rsyncFilters
              <|> srcRsync ^. rsyncFilters
          )
  case rfs of
    [] -> go []
    filters -> withSystemTempFile "filters" $ \fpath h -> do
      liftIO $ do
        T.hPutStr h (T.unlines filters)
        hClose h

      opts <- ask
      when (verbose opts) $
        liftIO $
          debug' $
            "INCLUDE FROM:\n" <> T.unlines filters
      incl <- case includeFrom opts of
        Just path -> (: []) . pack <$> liftIO (expandPath path)
        Nothing -> pure []
      go $
        "--include-from=" <> pack fpath
          : ["--include-from=" <> path | path <- incl]
  where
    fs = bnd ^. fileset

doRsync :: Text -> [Text] -> RsyncOptions -> App ()
doRsync label options rsyncOpts = do
  opts <- ask
  let den = (\x -> if x then 1000 else 1024) $ siUnits opts
      toRemote = ":" `T.isInfixOf` (rsyncOpts ^. roDest)
      args =
        [ "-aHEy"
        -- , "--delete-after"
        -- , "--ignore-errors"
        -- , "--force"
        ]
          <> ( case ssh opts of
                 Just cmd -> ["--rsh", pack cmd]
                 _ -> []
             )
          <> ["-n" | dryRun opts]
          <> ["--no-links" | rsyncOpts ^. roNoLinks]
          <> ["-AXUN" | rsyncOpts ^. roPreserveAttrs]
          <> ["--delete-excluded" | rsyncOpts ^. roDeleteExcluded]
          <> ["--checksum" | checksum opts]
          <> ( if verbose opts
                 then ["-P"]
                 else ["--stats"]
             )
          <> options
          <> [ rsyncOpts ^. roSource,
               if toRemote
                 then T.intercalate "\\ " (T.words (rsyncOpts ^. roDest))
                 else rsyncOpts ^. roDest
             ]
      analyze = not (verbose opts)
      env' = defaultExeEnv {exeDiscard = not analyze}

  (ec, diff, output) <- execute env' "rsync" args
  when (ec == ExitSuccess) $
    if analyze
      then do
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
              <> ": \ESC[36mSent \ESC[35m"
              <> humanReadable den (fromMaybe 0 xfer)
              <> "\ESC[0m\ESC[36m in "
              <> commaSep (fromIntegral (fromMaybe 0 sent))
              <> " files\ESC[0m (out of "
              <> humanReadable den (fromMaybe 0 total)
              <> " in "
              <> commaSep (fromIntegral (fromMaybe 0 files))
              <> ") \ESC[32m["
              <> tshow (round diff :: Int)
              <> "s]\ESC[0m"
      else liftIO $ T.putStr output
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

execute :: ExeEnv -> FilePath -> [Text] -> App (ExitCode, NominalDiffTime, Text)
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
      runner p xs = timeFunction (readProcessWithExitCode p xs "")
      runner' p xs =
        ( case exeCwd of
            Just cwd
              | isNothing exeRemote -> \action -> liftIO $ do
                  cur <- getCurrentDirectory
                  (setCurrentDirectory cwd >> runReaderT action opts)
                    `finally` setCurrentDirectory cur
            _ -> id
        )
          $ modifier
          $ liftIO (runner p xs)
  let (sshCmd : sshArgs) = words name''
  n <- liftIO $ findCmd sshCmd
  liftIO $
    debug' $
      pack n
        <> " "
        <> T.intercalate
          " "
          (map tshow (map T.pack sshArgs ++ args''))
  (diff, (ec, out, err)) <-
    if dryRun opts
      then return (0, (ExitSuccess, "", ""))
      else runner' n (map unpack args'')
  unless (ec == ExitSuccess) $
    errorL $
      "Error running command: " <> pack err
  pure (ec, diff, pack out)
  where
    findCmd n
      -- Assume commands with spaces in them are "known"
      | " " `T.isInfixOf` pack n = return n
      | isRelative n =
          findExecutable n >>= \case
            Nothing -> errorL $ "Failed to find command: " <> pack n
            Just c' -> return c'
      | otherwise = return n

    remote ::
      Options ->
      Host ->
      (App a -> App a, FilePath, [Text]) ->
      (App a -> App a, FilePath, [Text])
    remote opts host (m, p, xs) =
      ( m,
        fromMaybe "ssh" (ssh opts),
        host : pack p : xs
      )

execute_ :: ExeEnv -> FilePath -> [Text] -> App ExitCode
execute_ env' fp args = do
  (ec, _, _) <- execute env' {exeDiscard = True} fp args
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

timeFunction :: IO a -> IO (NominalDiffTime, a)
timeFunction function = do
  startTime <- getCurrentTime
  a <- function
  endTime <- getCurrentTime
  pure (diffUTCTime endTime startTime, a)

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
