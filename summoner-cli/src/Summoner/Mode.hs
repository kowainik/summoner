{- |
Module                  : Summoner.Mode
Copyright               : (c) 2020-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

This module introduces different modes in which the command-line interface
could be.
-}

module Summoner.Mode
    ( Interactivity (..)
    , isNonInteractive
    , ConnectMode (..)
    , isOffline
    ) where


-- | Switcher for non-interactive mode.
data Interactivity
    = Interactive
    | NonInteractive
    deriving stock (Show, Eq)

-- | Is interactivity mode 'NonInteractive'?
isNonInteractive :: Interactivity -> Bool
isNonInteractive = \case
    NonInteractive -> True
    Interactive -> False

-- | Switcher for offline mode.
data ConnectMode
    = Online
    | Offline
    deriving stock (Show, Eq)

-- | Is connection mode 'Offline'?
isOffline :: ConnectMode -> Bool
isOffline = \case
    Offline -> True
    Online  -> False
