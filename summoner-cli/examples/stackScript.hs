#!/usr/bin/env stack
{- stack
  --resolver lts-18.10
  script
  --package base
-}

main :: IO ()
main = putStrLn "Hello, World!"
