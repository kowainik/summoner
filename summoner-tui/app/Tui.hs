module Main where

import System.IO (hSetEncoding, utf8)

import Summoner.Tui (summonTui)


main :: IO ()
main = hSetEncoding stdout utf8 >> summonTui
