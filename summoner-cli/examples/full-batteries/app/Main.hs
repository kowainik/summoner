module Main (main) where

import FullBatteries (projectName)


main :: IO ()
main = putStrLn ("Executable for " ++ projectName)
