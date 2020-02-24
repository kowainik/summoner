module Main (main) where

import Gauge.Main


main :: IO ()
main = defaultMain [bench "const" (whnf const ())]
