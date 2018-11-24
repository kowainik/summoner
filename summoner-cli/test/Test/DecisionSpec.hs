module Test.DecisionSpec
       ( decisionSemigroupAssoc
       , decisionMonoidMempty

         -- * Gen
       , genDecision
       ) where

import Hedgehog (MonadGen, Property, forAll, property, (===))

import Summoner.Decision (Decision)

import qualified Hedgehog.Gen as Gen


decisionSemigroupAssoc :: Property
decisionSemigroupAssoc = property $ do
    x <- forAll genDecision
    y <- forAll genDecision
    z <- forAll genDecision
    (x <> y) <> z === x <> (y <> z)

decisionMonoidMempty :: Property
decisionMonoidMempty = property $ do
    x <- forAll genDecision
    x <> mempty === x
    mempty <> x === x

genDecision :: MonadGen m => m Decision
genDecision = Gen.enumBounded
