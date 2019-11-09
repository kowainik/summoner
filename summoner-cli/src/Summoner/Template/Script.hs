{-# LANGUAGE QuasiQuotes #-}

{- |
Copyright: (c) 2017-2019 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

File templates for @cabal@ and @stack@ scripts.
-}

module Summoner.Template.Script
       ( scriptFile
       ) where

import NeatInterpolation (text)

import Summoner.GhcVer (GhcVer, baseVer, latestLts)
import Summoner.Settings (Tool (..))


-- | 'Text' content for a single script file.
scriptFile :: GhcVer -> Tool -> Text
scriptFile ghcVer = \case
    Cabal ->
        [text|
        #!/usr/bin/env cabal
        {- cabal:
        build-depends:
          , base ^>= $baseVersion
        -}

        main :: IO ()
        main = putStrLn "Hello, World!"
        |]
    Stack ->
        [text|
        #!/usr/bin/env stack
        {- stack
          --resolver lts-${ltsVersion}
          script
          --package base
        -}

        main :: IO ()
        main = putStrLn "Hello, World!"
        |]
  where
    baseVersion, ltsVersion :: Text
    baseVersion = baseVer ghcVer
    ltsVersion  = latestLts ghcVer
