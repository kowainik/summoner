module Summoner.Settings
       ( Settings (..)
       , CustomPrelude (..)
       ) where

import Summoner.GhcVer (GhcVer)
import Summoner.License (License, LicenseName)


data CustomPrelude = CustomPrelude
    { cpPackage :: Text
    , cpModule  :: Text
    } deriving (Show, Eq)

-- | Data needed for project creation.
data Settings = Settings
    { settingsRepo           :: !Text   -- ^ repository name
    , settingsOwner          :: !Text   -- ^ gitHub username
    , settingsDescription    :: !Text   -- ^ project description
    , settingsFullName       :: !Text   -- ^ full name
    , settingsEmail          :: !Text   -- ^ e-mail
    , settingsYear           :: !Text   -- ^ year
    , settingsCategories     :: !Text   -- ^ project category
    , settingsLicenseName    :: !LicenseName -- ^ type of license
    , settingsLicenseText    :: !License -- ^ license text
    , settingsGitHub         :: !Bool   -- ^ gitHub repository
    , settingsTravis         :: !Bool   -- ^ Travis CI integration
    , settingsAppVeyor       :: !Bool   -- ^ AppVeyor CI integration
    , settingsIsLib          :: !Bool   -- ^ is library
    , settingsIsExe          :: !Bool   -- ^ is executable
    , settingsTest           :: !Bool   -- ^ add tests
    , settingsBench          :: !Bool   -- ^ add benchmarks
    , settingsTestedVersions :: ![GhcVer]  -- ^ ghc versions
    , settingsBaseType       :: !Text -- ^ Base library to use
    , settingsPrelude        :: !(Maybe CustomPrelude)  -- ^ custom prelude to be used
    , settingsExtensions     :: ![Text] -- ^ default extensions
    , settingsWarnings       :: ![Text] -- ^ default warnings
    , settingsCabal          :: !Bool
    , settingsStack          :: !Bool
    , settingsStylish        :: !(Maybe Text) -- ^ @.stylish-haskell.yaml@ file
    , settingsContributing   :: !(Maybe Text) -- ^ @CONTRIBUTING.md@ file
    } deriving (Show)
