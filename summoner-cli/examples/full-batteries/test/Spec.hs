module Main (main) where

import FullBatteries (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
