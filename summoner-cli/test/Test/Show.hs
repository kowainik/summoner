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
    [ "GHC-8.0.2     base-4.9.1.0    lts-9.21"
    , "GHC-8.2.2     base-4.10.1.0   lts-11.22"
    , "GHC-8.4.4     base-4.11.1.0   lts-12.26"
    , "GHC-8.6.5     base-4.12.0.0   lts-14.27"
    , "GHC-8.8.4     base-4.13.0.0   lts-16.31"
    , "GHC-8.10.7    base-4.14.3.0   lts-18.28"
    , "GHC-9.0.2     base-4.15.1.0   lts-19.33"
    , "GHC-9.2.8     base-4.16.4.0   lts-20.26"
    , "GHC-9.4.8     base-4.17.2.1   lts-21.25"
    , "GHC-9.6.7     base-4.18.3.1   lts-22.44"
    , "GHC-9.8.4     base-4.19.2.0   lts-23.28"
    , "GHC-9.10.3    base-4.20.2.0   lts-24.26"
    , "GHC-9.12.3    base-4.21.1.0   nightly-2026-01-04"
    ]
