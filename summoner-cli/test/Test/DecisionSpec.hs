{- HLINT ignore "Monoid law, right identity" -}
{- HLINT ignore "Monoid law, left identity" -}

module Test.DecisionSpec
       ( decisionPropertySpec

         -- * Gen
       , genDecision
       ) where

import Hedgehog (MonadGen, forAll, (===))
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

import Summoner.Decision (Decision)

import qualified Hedgehog.Gen as Gen


decisionPropertySpec :: Spec
decisionPropertySpec = describe "Decision property tests" $ do
    it "Decision Semigroup:Assoc" $ hedgehog $ do
        x <- forAll genDecision
        y <- forAll genDecision
        z <- forAll genDecision
        (x <> y) <> z === x <> (y <> z)

    it "Decision Monoid:Mempty" $ hedgehog $ do
        x <- forAll genDecision
        x <> mempty === x
        mempty <> x === x

genDecision :: MonadGen m => m Decision
genDecision = Gen.enumBounded
