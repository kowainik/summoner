module Summoner.Template.Stack
       ( stackFiles
       ) where

import Summoner.Default (defaultGHC)
import Summoner.GhcVer (GhcVer (..), baseVer, latestLts, showGhcVer)
import Summoner.Settings (Settings (..))
import Summoner.Tree (TreeFs (..))


stackFiles :: Settings -> [TreeFs]
stackFiles Settings{..} = map createStackYaml settingsTestedVersions
 where
    -- create @stack.yaml@ file with LTS corresponding to specified ghc version
    createStackYaml :: GhcVer -> TreeFs
    createStackYaml ghcV = File (toString $ "stack" <> ver <> ".yaml")
        $ "resolver: lts-" <> latestLts ghcV <> extraDeps
      where
        ver :: Text
        ver = if ghcV == defaultGHC
              then ""
              else "-" <> showGhcVer ghcV

        extraDeps :: Text
        extraDeps = case settingsPrelude of
            Nothing -> ""
            Just _  -> "\n\nextra-deps: [base-noprelude-" <> baseVer ghcV <> "]"
