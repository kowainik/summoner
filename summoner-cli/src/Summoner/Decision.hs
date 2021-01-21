{- |
Module                  : Summoner.Decision
Copyright               : (c) 2017-2021 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Decision data type.
-}

module Summoner.Decision
       ( Decision (..)
       , decisionToBool
       , decisionsToBools
       , promptDecisionToBool
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

-- | Translate 'Decision' to 'Bool'.
decisionToBool :: Decision -> Bool
decisionToBool = \case
    Yes -> True
    Nop -> False
    Idk -> False

-- | Prompt the 'Decision' to terminal output depending on the value.
promptDecisionToBool :: Decision -> YesNoPrompt -> IO Bool
promptDecisionToBool decision target@YesNoPrompt {..} = case decision of
    Yes -> trueMessage  yesNoTarget
    Nop -> falseMessage yesNoTarget
    Idk -> chooseYesNoBool target

-- | Translate a pair of dependant 'Decision's to the pair of 'Bool's.
decisionsToBools :: (Decision, Decision) -> (Bool, Bool)
decisionsToBools = \case
    (Idk, Idk) -> (True, True)
    (Yes, Idk) -> (True, False)
    (Idk, Yes) -> (False, True)
    (Nop, Idk) -> (False, True)
    (Idk, Nop) -> (True, False)
    (x, y)     -> (decisionToBool x, decisionToBool y)
