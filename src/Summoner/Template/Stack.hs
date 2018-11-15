{-# LANGUAGE QuasiQuotes #-}

module Summoner.Template.Stack
       ( stackFiles
       ) where

import NeatInterpolation (text)

import Summoner.GhcVer (GhcVer (..), baseVer, latestLts, showGhcVer)
import Summoner.Settings (Settings (..))
import Summoner.Tree (TreeFs (..))


stackFiles :: Settings -> [TreeFs]
stackFiles Settings{..} = map createStackYaml settingsTestedVersions
 where
    -- create @stack.yaml@ file with LTS corresponding to specified ghc version
    createStackYaml :: GhcVer -> TreeFs
    createStackYaml ghcV = File (toString $ "stack" <> ver <> ".yaml")
        $ "resolver: lts-" <> latestLts ghcV <> extraDeps <> ghcOpts
      where
        ver :: Text
        ver = case ghcV of
            Ghc844 -> ""
            _      -> "-" <> showGhcVer ghcV

        extraDeps :: Text
        extraDeps = case settingsPrelude of
            Nothing -> ""
            Just _  -> "\n\nextra-deps: [base-noprelude-" <> baseVer ghcV <> "]"

        ghcOpts :: Text
        ghcOpts = memptyIfFalse (ghcV > Ghc802)
            [text|
            $endLine

            ghc-options:
              "$$locals": -fhide-source-paths
            |]
