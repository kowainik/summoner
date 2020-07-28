module Main (main) where

import StackFull (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
