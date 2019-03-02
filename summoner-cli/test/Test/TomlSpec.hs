module Test.TomlSpec
       ( tomlProp
       ) where

import Hedgehog (MonadGen, Property, forAll, property, tripping)
import Toml.Bi.Code (decode, encode)

import Summoner.Config (ConfigP (..), PartialConfig, configT)
import Summoner.GhcVer (GhcVer)
import Summoner.License (LicenseName)
import Summoner.Settings (CustomPrelude (..), NixPkgSet (..))
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

genNixPkgSet :: MonadGen m => m NixPkgSet
genNixPkgSet = NixPkgSet <$> genText <*> genText <*> genText <*> genText

genPartialConfig :: MonadGen m => m PartialConfig
genPartialConfig = do
    cOwner      <- Last . Just <$> genText
    cFullName   <- Last . Just <$> genText
    cEmail      <- Last . Just <$> genText
    cLicense    <- Last . Just <$> genLicense
    cGhcVer     <- Last . Just <$> genGhcVerArr
    cCabal      <- genDecision
    cStack      <- genDecision
    cNix        <- genDecision
    cNixPkgSet  <- Last . Just <$> genNixPkgSet
    cGitHub     <- genDecision
    cTravis     <- genDecision
    cAppVey     <- genDecision
    cPrivate    <- genDecision
    cLib        <- genDecision
    cExe        <- genDecision
    cTest       <- genDecision
    cBench      <- genDecision
    cPrelude    <- Last <$> Gen.maybe genCustomPrelude
    cExtensions <- genTextArr
    cWarnings   <- genTextArr
    cGitignore  <- genTextArr
    cStylish    <- Last <$> Gen.maybe genSource
    cContributing <- Last <$> Gen.maybe genSource
    pure Config{..}
