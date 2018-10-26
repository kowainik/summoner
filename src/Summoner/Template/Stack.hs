{-# LANGUAGE QuasiQuotes #-}

module Summoner.Template.Stack
       ( stackFiles
       ) where

import NeatInterpolation (text)

import Summoner.GhcVer (GhcVer (..), baseVer, latestLts, showGhcVer)
import Summoner.Settings (Settings (..))
import Summoner.Tree (TreeFs (..))


stackFiles :: Settings -> [TreeFs]
stackFiles Settings{..} = memptyIfFalse settingsStack $ toListMap createStackYaml settingsTestedVersions
 where
    -- create @stack.yaml@ file with LTS corresponding to specified ghc version
    createStackYaml :: GhcVer -> TreeFs
    createStackYaml ghcV = let ver = case ghcV of
                                  Ghc843 -> ""
                                  _      -> "-" <> showGhcVer ghcV
        in stackYaml ver (latestLts ghcV) (baseVer ghcV)
      where
        stackYaml :: Text -> Text -> Text -> TreeFs
        stackYaml ghc lts baseV = File (toString $ "stack" <> ghc <> ".yaml")
            [text|
            resolver: lts-${lts}

            $extraDeps

            $ghcOpts
            $endLine
            |]
          where
            extraDeps :: Text
            extraDeps = case settingsPrelude of
                Nothing -> ""
                Just _  -> "extra-deps: [base-noprelude-" <> baseV <> "]"
            ghcOpts :: Text
            ghcOpts = if ghcV <= Ghc802 then
                        ""
                      else
                        [text|
                        ghc-options:
                          "$$locals": -fhide-source-paths
                        |]
