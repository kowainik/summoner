module Summoner.Validation
       ( Validation (..)
       ) where

-- | 'Validation' is 'Either' with a Left that is a 'Monoid'
data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Ord, Show)

instance Functor (Validation e) where
   fmap _ (Failure e) = Failure e
   fmap f (Success a) = Success (f a)

instance Semigroup e => Semigroup (Validation e a) where
  Failure e1 <> Failure e2 = Failure (e1 <> e2)
  Failure _  <> Success a2 = Success a2
  Success a1 <> Failure _  = Success a1
  Success a1 <> Success _  = Success a1

instance Semigroup e => Applicative (Validation e) where
  pure = Success

  Failure e1 <*> Failure e2 = Failure (e1 <> e2)
  Failure e1 <*> Success _  = Failure e1
  Success _  <*> Failure e2 = Failure e2
  Success f  <*> Success a  = Success (f a)
