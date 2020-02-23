{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: BSD-3-Clause
Maintainer: Kowainik <xrom.xkov@gmail.com>

Stack-only example with all integrations
-}

module StackFull
       ( someFunc
       ) where


someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
