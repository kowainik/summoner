module Main (main) where

import Summoner.Golden (generateProjects, generateScripts, generateTomlConfig)

main :: IO ()
main = do
    generateScripts
    generateTomlConfig
    generateProjects
