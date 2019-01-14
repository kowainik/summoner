-- | This module contains some default values to use.

module Summoner.Default
       ( defaultGHC
       , defaultCabal
       , defaultTomlFile
       , defaultConfigFile
       , defaultDescription
       , currentYear
       , defaultNixCompiler
       , defaultNixPkgSet
       ) where

import Data.Time (getCurrentTime, toGregorian, utctDay)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import Summoner.GhcVer (GhcVer, nixCompiler)
import Summoner.Settings (NixPkgSet (..))

----------------------------------------------------------------------------
-- Default Settings
----------------------------------------------------------------------------

-- | Default GHC version is the latest available.
defaultGHC :: GhcVer
defaultGHC = maxBound

-- | Default version of the Cabal.
defaultCabal :: Text
defaultCabal = "2.0"

defaultTomlFile :: FilePath
defaultTomlFile = ".summoner.toml"

defaultConfigFile :: IO FilePath
defaultConfigFile = (</> defaultTomlFile) <$> getHomeDirectory

defaultDescription :: Text
defaultDescription = "See README for more info"

currentYear :: IO Text
currentYear = do
    now <- getCurrentTime
    let (year, _, _) = toGregorian $ utctDay now
    pure $ show year

-- | Default compiler for nix to use.
defaultNixCompiler :: Text
defaultNixCompiler = nixCompiler defaultGHC

-- | The default nix package set.
defaultNixPkgSet :: NixPkgSet
defaultNixPkgSet = NixPkgSet
    { npsOwner    = "NixOS"
    , npsRepo     = "nixpkgs"
    , npsRev      = "cecec1f74468766825c2ad32d8388c2ded36225f"
    , npsSha256   = "1sq538wy0shbakah27b6n4bl5amzwkzjsds77vdd8rsq0d1nys4w"
    }
