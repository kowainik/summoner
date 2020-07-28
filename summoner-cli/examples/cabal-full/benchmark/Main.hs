module Main (main) where

import CabalFull (projectName)


main :: IO ()
main = putStrLn ("Benchmarks for " ++ projectName)
