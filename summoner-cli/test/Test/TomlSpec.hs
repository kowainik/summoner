module Test.TomlSpec
       ( tomlProp
       ) where

import qualified Data.Semigroup as S

import Hedgehog (MonadGen, Property, forAll, property, tripping)
import Toml.Bi.Code (decode, encode)

import Summoner.Config (ConfigP (..), PartialConfig, configT)
import Summoner.GhcVer (GhcVer)
import Summoner.License (LicenseName)
import Summoner.Settings (CustomPrelude (..))
import Summoner.Source (Source (..))
import Test.DecisionSpec (genDecision)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


tomlProp :: Property
tomlProp = property $ do
    configToml <- forAll genPartialConfig
    tripping configToml (encode configT) (decode configT)

----------------------------------------------------------------------------
-- Generators
----------------------------------------------------------------------------

genText :: MonadGen m => m Text
genText = Gen.text
    (Range.constant 0 10)
    (Gen.frequency [ (1, Gen.element ['.','-']), (10, Gen.alphaNum) ])

genTextArr :: MonadGen m => m [Text]
genTextArr = Gen.list (Range.constant 0 10) genText

genGhcVerArr :: MonadGen m => m [GhcVer]
genGhcVerArr = Gen.list (Range.constant 0 10) Gen.enumBounded

genCustomPrelude :: MonadGen m => m CustomPrelude
genCustomPrelude = CustomPrelude <$> genText <*> genText

genLicense :: MonadGen m => m LicenseName
genLicense = Gen.element universe

genSource :: MonadGen m => m Source
genSource = do
    txt <- genText
    s   <- Gen.element [File . toString, Url, Link]
    pure $ s txt

genPartialConfig :: MonadGen m => m PartialConfig
genPartialConfig = do
    cOwner      <- Gen.maybe $ S.Last <$> genText
    cFullName   <- Gen.maybe $ S.Last <$> genText
    cEmail      <- Gen.maybe $ S.Last <$> genText
    cLicense    <- Gen.maybe $ S.Last <$> genLicense
    cGhcVer     <- Gen.maybe $ S.Last <$> genGhcVerArr
    cCabal      <- genDecision
    cStack      <- genDecision
    cGitHub     <- genDecision
    cTravis     <- genDecision
    cAppVey     <- genDecision
    cPrivate    <- genDecision
    cLib        <- genDecision
    cExe        <- genDecision
    cTest       <- genDecision
    cBench      <- genDecision
    cPrelude    <- Gen.maybe $ S.Last <$> genCustomPrelude
    cExtensions <- genTextArr
    cWarnings   <- genTextArr
    cGhcOptions <- genTextArr
    cGitignore  <- genTextArr
    cStylish    <- Gen.maybe $ S.Last <$> genSource
    cContributing <- Gen.maybe $ S.Last <$> genSource
    cNoUpload   <- Any <$> Gen.bool
    pure Config{..}
