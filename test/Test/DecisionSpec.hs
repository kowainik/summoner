module Test.DecisionSpec where

import Hedgehog (MonadGen, forAll, property, (===))
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

import Summoner.Decision (Decision)

import qualified Hedgehog.Gen as Gen

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
