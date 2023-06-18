#!/usr/bin/env stack
{- stack
  --resolver lts-22.34
  script
  --package base
-}

main :: IO ()
main = putStrLn "Hello, World!"
