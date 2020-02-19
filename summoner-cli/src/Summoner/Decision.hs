{- |
Copyright: (c) 2017-2019 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Decision data type.
-}

module Summoner.Decision
       ( Decision (..)
       , decisionToBool
       ) where

import Summoner.Question (YesNoPrompt (..), chooseYesNoBool, falseMessage, trueMessage)


-- | Used for detecting the user decision during CLI input.
data Decision
    = Idk
    | Nop
    | Yes
    deriving stock (Show, Eq, Enum, Bounded, Generic)

instance Semigroup Decision where
    (<>) :: Decision -> Decision -> Decision
    Idk <> x   = x
    x   <> Idk = x
    _   <> x   = x

instance Monoid Decision where
    mempty  = Idk
    mappend = (<>)

decisionToBool :: Decision -> YesNoPrompt -> IO Bool
decisionToBool decision target@YesNoPrompt {..} = case decision of
    Yes -> trueMessage  yesNoTarget
    Nop -> falseMessage yesNoTarget
    Idk -> chooseYesNoBool target
