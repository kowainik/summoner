module Test.TomlSpec
       ( tomlProp
       ) where

import Hedgehog (MonadGen, Property, forAll, property, tripping)
import Toml.Bi.Code (decode, encode)

import Summoner.Config (ConfigP (..), PartialConfig, configCodec)
import Summoner.CustomPrelude (CustomPrelude (..))
import Summoner.GhcVer (GhcVer)
import Summoner.License (LicenseName)
import Summoner.Source (Source (..))
import Test.DecisionSpec (genDecision)

import qualified Data.Text as T
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


tomlProp :: Property
tomlProp = property $ do
    configToml <- forAll genPartialConfig
    tripping configToml (encode configCodec) (decode configCodec)

----------------------------------------------------------------------------
-- Generators
----------------------------------------------------------------------------

genTextWithSeparator :: MonadGen m => Char -> m Text
genTextWithSeparator s = do
    x <- T.toUpper <$> genText
    y <- T.toUpper <$> genText
    Gen.element [x, x <> T.cons s y]

genText :: MonadGen m => m Text
genText = Gen.text
    (Range.constant 1 11)
    Gen.alpha

genString :: MonadGen m => m String
genString = Gen.list (Range.constant 0 100) Gen.alphaNum

genTextArr :: MonadGen m => m [Text]
genTextArr = Gen.list (Range.constant 0 10) genText

genGhcVerArr :: MonadGen m => m [GhcVer]
genGhcVerArr = Gen.list (Range.constant 0 10) Gen.enumBounded

genCustomPrelude :: MonadGen m => m CustomPrelude
genCustomPrelude = do
    cpPackage <- genTextWithSeparator '-'
    cpModule <- genTextWithSeparator '.'
    pure CustomPrelude{..}

genLicense :: MonadGen m => m LicenseName
genLicense = Gen.element universe

genSource :: MonadGen m => m Source
genSource = do
    txt <- genText
    s   <- Gen.element [File . toString, Url, Link]
    pure $ s txt

genPartialConfig :: MonadGen m => m PartialConfig
genPartialConfig = do
    cOwner      <- Last <$> Gen.maybe genText
    cFullName   <- Last <$> Gen.maybe genText
    cEmail      <- Last <$> Gen.maybe genText
    cLicense    <- Last <$> Gen.maybe genLicense
    cGhcVer     <- Last <$> Gen.maybe genGhcVerArr
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
    cPrelude    <- Last <$> Gen.maybe genCustomPrelude
    cExtensions <- genTextArr
    cWarnings   <- genTextArr
    cGhcOptions <- genTextArr
    cGitignore  <- genTextArr
    cStylish    <- Last <$> Gen.maybe genSource
    cContributing <- Last <$> Gen.maybe genSource
    cNoUpload   <- Any <$> Gen.bool
    cFiles <- Gen.map (Range.constant 0 10) (liftA2 (,) genString genSource)
    pure Config{..}
