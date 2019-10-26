module Test.CustomPrelude
       ( customPreludeSpec
       ) where

import Test.Hspec (Spec, describe, it, shouldNotSatisfy, shouldSatisfy)

import Summoner.Text (moduleNameValid, packageNameValid)


customPreludeSpec :: Spec
customPreludeSpec = do
    describe "package name validation" $ do
        it "validates 'package'" $
            "package" `shouldSatisfy` packageNameValid
        it "validates 'package-package'" $
            "package-package" `shouldSatisfy` packageNameValid
        it "should not contain symbols" $
            "package-package%" `shouldNotSatisfy` packageNameValid
        it "should be one word" $
            "package -package" `shouldNotSatisfy` packageNameValid
    describe "module name validation" $ do
        it "one fragment" $
            "Module" `shouldSatisfy` moduleNameValid
        it "several fragments" $
            "Module1.Fragment2" `shouldSatisfy` moduleNameValid
        it "no trailing dot" $
            "Module1.Fragment2." `shouldNotSatisfy` moduleNameValid
        it "no double dots" $
            "Module1..Fragment2" `shouldNotSatisfy` moduleNameValid
        it "no lower leading letter" $
            "Module1.fragment2" `shouldNotSatisfy` moduleNameValid
        it "no other Symbols" $
            "Module1.fr#agment2" `shouldNotSatisfy` moduleNameValid
        it "no several words" $
            "Module1.fra gment2" `shouldNotSatisfy` moduleNameValid
