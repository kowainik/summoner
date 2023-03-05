module Test.TomlSpec
       ( tomlSpec
       ) where

import Hedgehog (MonadGen, forAll, tripping)
import Test.Hspec (Spec, describe, it, shouldReturn, shouldSatisfy)
import Test.Hspec.Hedgehog (hedgehog)
import Toml.Codec.Code (decode, encode)
import Validation (isSuccess)

import Summoner.Config (ConfigP (..), PartialConfig, configCodec, defaultConfig, finalise)
import Summoner.CustomPrelude (CustomPrelude (..))
import Summoner.Default (defaultConfigFileContent)
import Summoner.GhcVer (GhcVer)
import Summoner.License (LicenseName)
import Summoner.Source (Source (..))
import Test.DecisionSpec (genDecision)

import qualified Data.Text as T
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


tomlSpec :: Spec
tomlSpec = describe "TOML Testing" $ do
    tomlConfigSpec
    tomlProp

tomlConfigSpec :: Spec
tomlConfigSpec = describe "TOML configuration spec" $ do
    it "finalises default configuration" $
        finalise defaultConfig `shouldSatisfy` isSuccess
    it "parses default configuration" $
        decode configCodec defaultConfigFileContent `shouldSatisfy` isRight
    it "default configuration is up-to-date" $
        readFileText "examples/summoner-default.toml"
            `shouldReturn` defaultConfigFileContent

tomlProp :: Spec
tomlProp = describe "TOML property tests" $
    it "decode . encode == id" $ hedgehog $ do
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
    s   <- Gen.element [Local . toString, Url, Raw]
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
    cGhActions  <- genDecision
    cTravis     <- genDecision
    cAppVey     <- genDecision
    cPrivate    <- genDecision
    cLib        <- genDecision
    cExe        <- genDecision
    cTest       <- genDecision
    cBench      <- genDecision
    cPrelude    <- Last <$> Gen.maybe genCustomPrelude
    cExtensions <- genTextArr
    cGhcOptions <- genTextArr
    cGitignore  <- genTextArr
    cNoUpload   <- Any <$> Gen.bool
    cFiles <- Gen.map (Range.constant 0 10) (liftA2 (,) genString genSource)
    pure ConfigP{..}
