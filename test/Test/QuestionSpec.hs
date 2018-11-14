{-# LANGUAGE OverloadedStrings #-}

module Test.QuestionSpec where

import Test.Tasty.HUnit ((@?=))

import Summoner.Question

unit_mkDefaultYesNoPrompt :: IO ()
unit_mkDefaultYesNoPrompt =
  yesNoPrompt (mkDefaultYesNoPrompt "Cabal") @?= "Add Cabal?"
