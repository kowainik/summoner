module Main (main) where

import System.IO (hSetEncoding, utf8)

import Summoner (summonCli)


main :: IO ()
main = hSetEncoding stdout utf8 >> summonCli
