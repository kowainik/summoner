module Test.DecisionSpec where

import Relude

import Hedgehog (MonadGen, forAll, liftGen, property, (===))
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Toml.Bi.Code (decode, encode, prettyException)

import Summoner.Config (ConfigP (..), PartialConfig, configT)
import Summoner.License (License (..), licenseNames)
import Summoner.ProjectData (CustomPrelude (..), Decision, GhcVer (..))

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

test_decisionSemigroupAssoc :: [TestTree]
test_decisionSemigroupAssoc = pure $ testProperty "Decision Semigroup:Assoc" $ property $ do
    x <- forAll genDecision
    y <- forAll genDecision
    z <- forAll genDecision
    (x <> y) <> z === x <> (y <> z)

test_decisionMonoidMempty :: [TestTree]
test_decisionMonoidMempty = pure $ testProperty "Decision Monoid:Mempty" $ property $ do
    x <- forAll genDecision
    x <> mempty === x
    mempty <> x === x

genDecision :: MonadGen m => m Decision
genDecision = Gen.enumBounded

genTextArr :: MonadGen m => m [Text]
genTextArr = Gen.list (Range.constant 0 10) $ Gen.text (Range.constant 0 10) Gen.alpha

genText :: MonadGen m => m Text
genText = Gen.text (Range.constant 0 10) Gen.alpha

genGhcVerArr :: MonadGen m => m [GhcVer]
genGhcVerArr = Gen.list (Range.constant 0 10) Gen.enumBounded

genCustomPrelude :: MonadGen m => m CustomPrelude
genCustomPrelude = Prelude
    <$> Gen.text (Range.constant 0 10) Gen.alpha
    <*> Gen.text (Range.constant 0 10) Gen.alpha

genLicense :: MonadGen m => m License
genLicense = Gen.element licenseNames

randomConfig :: MonadGen m => m PartialConfig
randomConfig = do
    text <- liftGen genText
    list <- liftGen genTextArr
    dec  <- liftGen genDecision
    lic  <- liftGen genLicense
    ghc  <- liftGen genGhcVerArr
    lude <- liftGen genCustomPrelude
    pure Config
        { cOwner      = Last $ Just text
        , cFullName   = Last $ Just text
        , cEmail      = Last $ Just text
        , cLicense    = Last $ Just lic
        , cGhcVer     = Last $ Just ghc
        , cCabal      = dec
        , cStack      = dec
        , cGitHub     = dec
        , cTravis     = dec
        , cAppVey     = dec
        , cPrivate    = dec
        , cScript     = dec
        , cLib        = dec
        , cExe        = dec
        , cTest       = dec
        , cBench      = dec
        , cPrelude    = Last $ Just lude
        , cExtensions = list
        , cWarnings   = list
        }

test_Toml :: [TestTree]
test_Toml = pure $ testProperty "Test TOML" $ property $ do
    configToml     <- forAll randomConfig
    let textToml    = encode configT configToml
    let mConfigToml = decode configT textToml
    case mConfigToml of
        Left err           -> putStrLn $ prettyException err
        Right deConfigToml -> configToml === deConfigToml
