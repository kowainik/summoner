module Main where

import System.IO (hSetEncoding, utf8)

import Summoner (summon)

main :: IO ()
main = hSetEncoding stdout utf8 >> summon
