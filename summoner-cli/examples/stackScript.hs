#!/usr/bin/env stack
{- stack
  --resolver nightly-2024-10-11
  script
  --package base
-}

main :: IO ()
main = putStrLn "Hello, World!"
