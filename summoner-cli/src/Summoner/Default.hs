-- | This module contains some default values to use.

module Summoner.Default
       ( defaultGHC
       , defaultCabal
       , defaultTomlFile
       , defaultConfigFile
       , defaultDescription
       , currentYear
       , defaultNixCompiler
       ) where

import Data.Time (getCurrentTime, toGregorian, utctDay)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import Summoner.GhcVer (GhcVer, nixCompiler)

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
