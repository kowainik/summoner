#!/usr/bin/env stack
{- stack
  --resolver nightly-2022-01-10
  script
  --package base
-}

main :: IO ()
main = putStrLn "Hello, World!"
