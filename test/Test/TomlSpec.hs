{-# LANGUAGE RecordWildCards #-}

module Test.TomlSpec where

import Relude
import Relude.Extra.Enum (universe)

import Hedgehog (MonadGen, forAll, property, tripping)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Toml.Bi.Code (decode, encode)

import Summoner.Config (ConfigP (..), PartialConfig, configT)
import Summoner.GhcVer (GhcVer)
import Summoner.License (LicenseName)
import Summoner.ProjectData (CustomPrelude (..))
import Test.DecisionSpec (genDecision)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genText :: MonadGen m => m Text
genText = Gen.text
    (Range.constant 0 10)
    (Gen.frequency [ (1, Gen.element ['.','-']), (10, Gen.alphaNum) ])

genTextArr :: MonadGen m => m [Text]
genTextArr = Gen.list (Range.constant 0 10) genText

genGhcVerArr :: MonadGen m => m [GhcVer]
genGhcVerArr = Gen.list (Range.constant 0 10) Gen.enumBounded

genCustomPrelude :: MonadGen m => m CustomPrelude
genCustomPrelude = Prelude <$> genText <*> genText

genLicense :: MonadGen m => m LicenseName
genLicense = Gen.element universe

genPartialConfig :: MonadGen m => m PartialConfig
genPartialConfig = do
    cOwner      <- Last . Just <$> genText
    cFullName   <- Last . Just <$> genText
    cEmail      <- Last . Just <$> genText
    cLicense    <- Last . Just <$> genLicense
    cGhcVer     <- Last . Just <$> genGhcVerArr
    cCabal      <- genDecision
    cStack      <- genDecision
    cGitHub     <- genDecision
    cTravis     <- genDecision
    cAppVey     <- genDecision
    cPrivate    <- genDecision
    cScript     <- genDecision
    cLib        <- genDecision
    cExe        <- genDecision
    cTest       <- genDecision
    cBench      <- genDecision
    cPrelude    <- Last . Just <$> genCustomPrelude
    cExtensions <- genTextArr
    cWarnings   <- genTextArr
    pure Config{..}

test_Toml :: [TestTree]
test_Toml = pure $ testProperty "decode . encode == id" $ property $ do
    configToml <- forAll genPartialConfig
    tripping configToml (encode configT) (decode configT)
