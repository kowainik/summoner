module Main (main) where

import FullBatteries (projectName)


main :: IO ()
main = putStrLn ("Benchmarks for " ++ projectName)
