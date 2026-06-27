{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Yaml (ParseException, decodeEither')
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Pushme.Options
import Test.Tasty
import Test.Tasty.Hedgehog

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "pushme"
    [ testGroup
        "combineFilters"
        [ testProperty "Nothing is left identity" prop_combineFilters_leftId
        , testProperty "Nothing is right identity" prop_combineFilters_rightId
        , testProperty "associative" prop_combineFilters_assoc
        , testProperty "both Just concatenates with newline" prop_combineFilters_concat
        ]
    , testGroup
        "RsyncOptions Semigroup"
        [ testProperty "associative" prop_rsyncOptions_assoc
        ]
    , testGroup
        "filter set references"
        [ testProperty "interpolates a named filter set" prop_filterReference_exact
        , testProperty "resolves Filters and ExtraFilters" prop_resolveRsyncFilterReferences
        , testProperty "parses and resolves config filter sets" prop_optionsParseFilterSets
        ]
    ]

-- Generators

genText :: Gen Text
genText = Gen.text (Range.linear 0 50) Gen.alphaNum

genMaybeText :: Gen (Maybe Text)
genMaybeText = Gen.maybe genText

genMaybeTextList :: Gen (Maybe [Text])
genMaybeTextList = Gen.maybe (Gen.list (Range.linear 0 5) genText)

genMaybeBool :: Gen (Maybe Bool)
genMaybeBool = Gen.maybe Gen.bool

genRsyncOptions :: Gen RsyncOptions
genRsyncOptions =
  RsyncOptions
    <$> genMaybeText
    <*> genMaybeText
    <*> Gen.bool
    <*> Gen.bool
    <*> genMaybeBool -- PreserveACLs
    <*> genMaybeBool -- PreserveXattrs
    <*> genMaybeBool -- PreserveAtimes
    <*> genMaybeBool -- PreserveCrtimes
    <*> genMaybeBool -- PreserveHardLinks
    <*> genMaybeBool -- PreserveExecutability
    <*> Gen.bool
    <*> genMaybeTextList
    <*> genMaybeTextList
    <*> Gen.bool

emptyRsyncOptions :: RsyncOptions
emptyRsyncOptions =
  RsyncOptions
    Nothing
    Nothing
    False
    False
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    False
    Nothing
    Nothing
    True

-- combineFilters properties

prop_combineFilters_leftId :: Property
prop_combineFilters_leftId = property $ do
  x <- forAll genMaybeText
  combineFilters Nothing x === x

prop_combineFilters_rightId :: Property
prop_combineFilters_rightId = property $ do
  x <- forAll genMaybeText
  combineFilters x Nothing === x

prop_combineFilters_assoc :: Property
prop_combineFilters_assoc = property $ do
  a <- forAll genMaybeText
  b <- forAll genMaybeText
  c <- forAll genMaybeText
  combineFilters (combineFilters a b) c === combineFilters a (combineFilters b c)

prop_combineFilters_concat :: Property
prop_combineFilters_concat = property $ do
  a <- forAll genText
  b <- forAll genText
  combineFilters (Just a) (Just b) === Just (a <> "\n" <> b)

-- RsyncOptions Semigroup properties

prop_rsyncOptions_assoc :: Property
prop_rsyncOptions_assoc = property $ do
  a <- forAll genRsyncOptions
  b <- forAll genRsyncOptions
  c <- forAll genRsyncOptions
  (a <> b) <> c === a <> (b <> c)

-- Filter set reference properties

prop_filterReference_exact :: Property
prop_filterReference_exact =
  property $
    interpolateFilterReferences
      (M.fromList [("srcFilters", "- dist/\n- result")])
      "$srcFilters"
      === "- dist/\n- result"

prop_resolveRsyncFilterReferences :: Property
prop_resolveRsyncFilterReferences = property $ do
  let filterSets =
        M.fromList
          [ ("commonFilters", "- dist/")
          , ("extraFilters", "- .cache/")
          ]
      resolved =
        resolveRsyncFilterReferences
          filterSets
          emptyRsyncOptions
            { _rsyncFilters = Just "$commonFilters"
            , _rsyncExtraFilters = Just "- tmp/\n$extraFilters"
            }
  _rsyncFilters resolved === Just "- dist/"
  _rsyncExtraFilters resolved === Just "- tmp/\n- .cache/"

prop_optionsParseFilterSets :: Property
prop_optionsParseFilterSets = property $
  case decodeEither' configYaml :: Either ParseException Options of
    Left err -> do
      annotateShow err
      failure
    Right opts -> do
      _optsFilterSets opts
        === M.fromList [("srcFilters", "- dist/\n- result\n")]
      fmap _rsyncFilters (_optsRsyncOpts (resolveOptionsFilterReferences opts))
        === Just (Just "- dist/\n- result\n")
 where
  configYaml =
    B.pack $
      unlines
        [ "Filters:"
        , "  srcFilters: |"
        , "    - dist/"
        , "    - result"
        , "GlobalOptions:"
        , "  Filters: $srcFilters"
        ]
