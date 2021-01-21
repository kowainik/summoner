{- |
Module                  : Summoner.CustomPrelude
Copyright               : (c) 2017-2021 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

This module implements data type for representing custom alternative preludes.
-}

module Summoner.CustomPrelude
       ( CustomPrelude (..)
       , customPreludeT
       ) where

import Toml (TomlCodec, (.=))

import Summoner.Text (moduleNameValid, packageNameValid)

import qualified Toml


data CustomPrelude = CustomPrelude
    { cpPackage :: !Text
    , cpModule  :: !Text
    } deriving stock (Show, Eq)

customPreludeT :: TomlCodec CustomPrelude
customPreludeT = CustomPrelude
    <$> Toml.validateIf packageNameValid Toml._Text "package" .= cpPackage
    <*> Toml.validateIf moduleNameValid  Toml._Text "module"  .= cpModule
