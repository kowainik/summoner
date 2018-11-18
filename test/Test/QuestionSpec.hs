module Test.QuestionSpec
       ( yesNoPromptSpec
       ) where

import Test.Hspec (Spec, describe, it)

import Summoner.Question (YesNoPrompt (..), mkDefaultYesNoPrompt)

yesNoPromptSpec :: Spec
yesNoPromptSpec = describe "mkDefaultYesNoPrompt works correctly" $
    it "makes cabal prompt" $
        yesNoPrompt (mkDefaultYesNoPrompt "Cabal") == "Add Cabal?"
