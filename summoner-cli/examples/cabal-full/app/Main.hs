module Main (main) where

import CabalFull (projectName)


main :: IO ()
main = putStrLn ("Executable for " ++ projectName)
