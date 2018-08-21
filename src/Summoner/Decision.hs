module Summoner.Decision
       ( Decision (..)
       , decisionToBool
       ) where

import Relude

import Generics.Deriving.Monoid (GMonoid (..))
import Generics.Deriving.Semigroup (GSemigroup (..))

import Summoner.Question (chooseYesNoBool, falseMessage, trueMessage)

----------------------------------------------------------------------------
-- Decision data type
----------------------------------------------------------------------------

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

decisionToBool :: Decision -> Text -> IO Bool
decisionToBool decision target = case decision of
    Yes -> trueMessage  target
    Nop -> falseMessage target
    Idk -> chooseYesNoBool target
