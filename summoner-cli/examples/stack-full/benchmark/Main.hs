module Main (main) where

import StackFull (projectName)


main :: IO ()
main = putStrLn ("Benchmarks for " ++ projectName)
