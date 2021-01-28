module Test.Script
       ( scriptSpec
       ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Summoner.Default (defaultGHC)
import Summoner.Settings (Tool (..))
import Summoner.Template.Script (scriptFile)
import Summoner.Text (quote)


scriptSpec :: Spec
scriptSpec = describe "script golden tests" $ do
    it "correctly creates cabal script" $
        scriptFile defaultGHC Cabal `shouldBe` cabalScript
    it "correctly creates stack script" $
        scriptFile defaultGHC Stack `shouldBe` stackScript

cabalScript :: Text
cabalScript = unlines
    [ "#!/usr/bin/env cabal"
    , "{- cabal:"
    , "build-depends:"
    , "  , base ^>= 4.14.1.0"
    , "-}"
    , ""
    , "main :: IO ()"
    , "main = putStrLn " <> quote "Hello, World!"
    ]

stackScript :: Text
stackScript = unlines
    [ "#!/usr/bin/env stack"
    , "{- stack"
    , "  --resolver lts-17.0"
    , "  script"
    , "  --package base"
    , "-}"
    , ""
    , "main :: IO ()"
    , "main = putStrLn " <> quote "Hello, World!"
    ]
