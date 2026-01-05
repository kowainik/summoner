#!/usr/bin/env stack
{- stack
  --resolver nightly-2026-01-04
  script
  --package base
-}

main :: IO ()
main = putStrLn "Hello, World!"
