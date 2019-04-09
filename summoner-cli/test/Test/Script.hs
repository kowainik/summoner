{-# LANGUAGE QuasiQuotes #-}

module Test.Script
       ( scriptSpec
       ) where

import NeatInterpolation (text)
import Test.Hspec (Spec, describe, it, shouldBe)

import Summoner.GhcVer (GhcVer (..))
import Summoner.Settings (Tool (..))
import Summoner.Template.Script (scriptFile)


scriptSpec :: Spec
scriptSpec = describe "script golden tests" $ do
    it "correctly creates cabal script" $
        scriptFile Ghc844 Cabal `shouldBe` cabalScript
    it "correctly creates stack script" $
        scriptFile Ghc864 Stack `shouldBe` stackScript

cabalScript :: Text
cabalScript = [text|
#!/usr/bin/env cabal
{- cabal:
build-depends:
  , base ^>= 4.11.1.0
-}

main :: IO ()
main = putStrLn "Hello, World!"
|]

stackScript :: Text
stackScript = [text|
#!/usr/bin/env stack
{- stack
  --resolver lts-13.16
  script
  --package base
-}

main :: IO ()
main = putStrLn "Hello, World!"
|]
