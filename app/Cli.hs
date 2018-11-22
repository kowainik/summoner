module Main where

import System.IO (hSetEncoding, stdout, utf8)

import Summoner (summonCli)

main :: IO ()
main = hSetEncoding stdout utf8 >> summonCli
