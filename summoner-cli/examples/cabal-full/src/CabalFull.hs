{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: BSD-3-Clause
Maintainer: Kowainik <xrom.xkov@gmail.com>

Cabal-only example with all integrations
-}

module CabalFull
       ( someFunc
       ) where


someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
