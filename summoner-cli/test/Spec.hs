module Main (main) where

import Test.Hspec (hspec)

import Test.CustomPrelude (customPreludeSpec)
import Test.DecisionSpec (decisionPropertySpec)
import Test.Golden (goldenSpec)
import Test.QuestionSpec (yesNoPromptSpec)
import Test.Script (scriptSpec)
import Test.Show (showCommandSpec)
import Test.TomlSpec (tomlSpec)


main :: IO ()
main = hspec $ do
    yesNoPromptSpec
    scriptSpec
    showCommandSpec
    goldenSpec
    customPreludeSpec
    tomlSpec
    decisionPropertySpec
