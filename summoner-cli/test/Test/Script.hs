module Test.Script
    ( scriptSpec
    ) where

import Test.Hspec (Spec, describe, it, shouldReturn)

import Summoner.Default (defaultGHC)
import Summoner.Settings (Tool (..))
import Summoner.Template.Script (scriptFile)

import qualified Data.Text as T

scriptSpec :: Spec
scriptSpec = describe "script golden tests" $ do
    it "correctly creates cabal script" $
        readScript "examples/cabalScript.hs" `shouldReturn` scriptFile defaultGHC Cabal
    it "correctly creates stack script" $
        readScript "examples/stackScript.hs" `shouldReturn` scriptFile defaultGHC Stack

-- | Read a script file, normalizing line endings (CRLF -> LF) for cross-platform compatibility
readScript :: FilePath -> IO Text
readScript path = T.filter (/= '\r') . decodeUtf8 <$> readFileBS path
