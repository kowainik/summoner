{- |
Module                  : Summoner.Template.Stack
Copyright               : (c) 2017-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Templates for @stack.yaml@ files.
-}

module Summoner.Template.Stack
       ( stackFiles
       ) where

import Summoner.Default (defaultGHC)
import Summoner.GhcVer (GhcVer (..), latestLts, showGhcVer)
import Summoner.Settings (Settings (..))
import Summoner.Tree (TreeFs (..))


stackFiles :: Settings -> [TreeFs]
stackFiles Settings{..} = map createStackYaml settingsTestedVersions
 where
    -- create @stack.yaml@ file with LTS corresponding to specified ghc version
    createStackYaml :: GhcVer -> TreeFs
    createStackYaml ghcV = File (toString $ "stack" <> ver <> ".yaml")
        $ "resolver: " <> latestLts ghcV <> "\n"
      where
        ver :: Text
        ver = if ghcV == defaultGHC
              then ""
              else "-" <> showGhcVer ghcV
