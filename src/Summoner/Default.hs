-- | This module contains some default values to use.

module Summoner.Default
       ( defaultGHC
       , defaultTomlFile
       , defaultConfigFile
       , currentYear
       , endLine
       ) where

import Data.Text (Text)
import Data.Time (getCurrentTime, toGregorian, utctDay)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import Summoner.ProjectData (GhcVer (Ghc822))

import qualified Data.Text as T

----------------------------------------------------------------------------
-- Default Settings
----------------------------------------------------------------------------

defaultGHC :: GhcVer
defaultGHC = Ghc822

defaultTomlFile :: String
defaultTomlFile = "summoner.toml"

defaultConfigFile :: IO FilePath
defaultConfigFile = (</> defaultTomlFile) <$> getHomeDirectory

currentYear :: IO Text
currentYear = do
    now <- getCurrentTime
    let (year, _, _) = toGregorian $ utctDay now
    pure $ T.pack $ show year

endLine :: Text
endLine = "\n"
