module Test.Script
    ( scriptSpec
    ) where

import Test.Hspec (Spec, describe, it, shouldReturn)

import Summoner.Default (defaultGHC)
import Summoner.Settings (Tool (..))
import Summoner.Template.Script (scriptFile)


scriptSpec :: Spec
scriptSpec = describe "script golden tests" $ do
    it "correctly creates cabal script" $
        readFileText "examples/cabalScript.hs" `shouldReturn` scriptFile defaultGHC Cabal
    it "correctly creates stack script" $
        readFileText "examples/stackScript.hs" `shouldReturn` scriptFile defaultGHC Stack
