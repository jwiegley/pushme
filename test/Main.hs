{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
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
    ]

-- Generators

genText :: Gen Text
genText = Gen.text (Range.linear 0 50) Gen.alphaNum

genMaybeText :: Gen (Maybe Text)
genMaybeText = Gen.maybe genText

genMaybeTextList :: Gen (Maybe [Text])
genMaybeTextList = Gen.maybe (Gen.list (Range.linear 0 5) genText)

genRsyncOptions :: Gen RsyncOptions
genRsyncOptions =
  RsyncOptions
    <$> genMaybeText
    <*> genMaybeText
    <*> Gen.bool
    <*> Gen.bool
    <*> Gen.bool
    <*> Gen.bool
    <*> genMaybeTextList
    <*> genMaybeTextList
    <*> Gen.bool

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
