module Summoner.Settings
       ( Settings (..)
       , CustomPrelude (..)
       , customPreludeT
       ) where

import Toml (TomlCodec, (.=))

import Summoner.GhcVer (GhcVer)
import Summoner.License (License, LicenseName)

import qualified Toml


data CustomPrelude = CustomPrelude
    { cpPackage :: Text
    , cpModule  :: Text
    } deriving (Show, Eq)

customPreludeT :: TomlCodec CustomPrelude
customPreludeT = CustomPrelude
    <$> Toml.text "package" .= cpPackage
    <*> Toml.text "module"  .= cpModule

-- | Data needed for project creation.
data Settings = Settings
    { settingsRepo           :: !Text   -- ^ repository name
    , settingsOwner          :: !Text   -- ^ github username
    , settingsDescription    :: !Text   -- ^ project description
    , settingsFullName       :: !Text   -- ^ full name
    , settingsEmail          :: !Text   -- ^ e-mail
    , settingsYear           :: !Text   -- ^ year
    , settingsCategories     :: !Text   -- ^ project category
    , settingsLicenseName    :: !LicenseName -- ^ type of license
    , settingsLicenseText    :: !License -- ^ license text
    , settingsGitHub         :: !Bool   -- ^ GitHub repository
    , settingsPrivate        :: !Bool   -- ^ private repository
    , settingsTravis         :: !Bool   -- ^ Travis CI integration
    , settingsAppVeyor       :: !Bool   -- ^ AppVeyor CI integration
    , settingsIsLib          :: !Bool   -- ^ is library
    , settingsIsExe          :: !Bool   -- ^ is executable
    , settingsTest           :: !Bool   -- ^ add tests
    , settingsBench          :: !Bool   -- ^ add benchmarks
    , settingsTestedVersions :: ![GhcVer]  -- ^ GHC versions
    , settingsBaseType       :: !Text -- ^ Base library to use
    , settingsPrelude        :: !(Maybe CustomPrelude)  -- ^ custom prelude to be used
    , settingsExtensions     :: ![Text] -- ^ default extensions
    , settingsWarnings       :: ![Text] -- ^ default warnings
    , settingsCabal          :: !Bool
    , settingsStack          :: !Bool
    , settingsStylish        :: !(Maybe Text) -- ^ @.stylish-haskell.yaml@ file
    , settingsContributing   :: !(Maybe Text) -- ^ @CONTRIBUTING.md@ file
    , settingsNoUpload       :: !Bool  -- ^ do not upload to GitHub
    } deriving (Show)
