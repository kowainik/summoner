-- | Decision data type.

module Summoner.Decision
       ( Decision (..)
       , decisionToBool
       ) where

import Generics.Deriving.Monoid (GMonoid (..))
import Generics.Deriving.Semigroup (GSemigroup (..))

import Summoner.Question (YesNoPrompt (..), chooseYesNoBool, falseMessage, trueMessage)


-- | Used for detecting the user decision during CLI input.
data Decision = Idk | Nop | Yes
    deriving (Show, Eq, Enum, Bounded, Generic)

instance Semigroup Decision where
    (<>) :: Decision -> Decision -> Decision
    Idk <> x   = x
    x   <> Idk = x
    _   <> x   = x

instance Monoid Decision where
    mempty  = Idk
    mappend = (<>)

instance GSemigroup Decision where
    gsappend = (<>)

instance GMonoid Decision where
    gmempty = mempty
    gmappend = (<>)

decisionToBool :: Decision -> YesNoPrompt -> IO Bool
decisionToBool decision target@YesNoPrompt {..} = case decision of
    Yes -> trueMessage  yesNoTarget
    Nop -> falseMessage yesNoTarget
    Idk -> chooseYesNoBool target
