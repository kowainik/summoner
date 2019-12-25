module Test.Show
       ( showCommandSpec
       ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Summoner.GhcVer (ghcTable)


showCommandSpec :: Spec
showCommandSpec = describe "show command golden tests" $
    it "correctly  shows 'show ghc'" $
        ghcTable `shouldBe` goldenGhcTable

goldenGhcTable :: [Text]
goldenGhcTable =
  [ "GHC-7.10.3    base-4.8.0.2    lts-6.35          "
  , "GHC-8.0.2     base-4.9.1.0    lts-9.21          "
  , "GHC-8.2.2     base-4.10.1.0   lts-11.22         "
  , "GHC-8.4.4     base-4.11.1.0   lts-12.26         "
  , "GHC-8.6.5     base-4.12.0.0   lts-14.18         "
  , "GHC-8.8.1     base-4.13.0.0   nightly-2019-12-25"
  ]
