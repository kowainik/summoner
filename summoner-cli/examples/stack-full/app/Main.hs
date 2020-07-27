module Main (main) where

import StackFull (projectName)


main :: IO ()
main = putStrLn ("Executable for " ++ projectName)
