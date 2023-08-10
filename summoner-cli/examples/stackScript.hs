#!/usr/bin/env stack
{- stack
  --resolver nightly-2023-08-09
  script
  --package base
-}

main :: IO ()
main = putStrLn "Hello, World!"
