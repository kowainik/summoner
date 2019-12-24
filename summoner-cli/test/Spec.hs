module Main where

import Hedgehog (Group (..), checkParallel)
import Test.Hspec (hspec)

import Test.CustomPrelude (customPreludeSpec)
import Test.DecisionSpec (decisionMonoidMempty, decisionSemigroupAssoc)
import Test.Golden (goldenSpec)
import Test.QuestionSpec (yesNoPromptSpec)
import Test.Script (scriptSpec)
import Test.Show (showCommandSpec)
import Test.TomlSpec (tomlProp)


main :: IO ()
main = do
    hspec $ do
        yesNoPromptSpec
        scriptSpec
        showCommandSpec
        goldenSpec
        customPreludeSpec
    ifM (checkParallel hedgehogTests) exitSuccess exitFailure

hedgehogTests :: Group
hedgehogTests = Group "Roundtrip properties"
    [ decisionSemigroupAssoc `named` "Decision Semigroup:Assoc"
    , decisionMonoidMempty   `named` "Decision Monoid:Mempty"
    , tomlProp               `named` "decode . encode == id"
    ]
  where
    named :: a -> b -> (b, a)
    named = flip (,)
