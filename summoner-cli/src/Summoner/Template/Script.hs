{- |
Module                  : Summoner.Template.Script
Copyright               : (c) 2017-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

File templates for @cabal@ and @stack@ scripts.
-}

module Summoner.Template.Script
       ( scriptFile
       ) where

import Summoner.GhcVer (GhcVer, baseVer, latestLts)
import Summoner.Settings (Tool (..))
import Summoner.Text (quote)


-- | 'Text' content for a single script file.
scriptFile :: GhcVer -> Tool -> Text
scriptFile ghcVer = \case
    Cabal -> unlines
        [ "#!/usr/bin/env cabal"
        , "{- cabal:"
        , "build-depends:"
        , "  , base ^>= " <> baseVersion
        , "-}"
        , ""
        , "main :: IO ()"
        , "main = putStrLn " <> quote "Hello, World!"
        ]
    Stack -> unlines
        [ "#!/usr/bin/env stack"
        , "{- stack"
        , "  --resolver " <> ltsVersion
        , "  script"
        , "  --package base"
        , "-}"
        , ""
        , "main :: IO ()"
        , "main = putStrLn " <> quote "Hello, World!"
        ]
  where
    baseVersion, ltsVersion :: Text
    baseVersion = baseVer ghcVer
    ltsVersion  = latestLts ghcVer
