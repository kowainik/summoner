module Summoner.Settings
       ( Settings (..)
       , CustomPrelude (..)
       , customPreludeT
       , NixPkgSet(..)
       , nixPkgSetT
       , defaultNixPkgSet
       , showNixPkgSet
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

data NixPkgSet = NixPkgSet
    { npsOwner :: Text
    , npsRepo  :: Text
    , npsRev   :: Text
    , npsSha   :: Text
    } deriving (Show, Eq)

nixPkgSetT :: TomlCodec NixPkgSet
nixPkgSetT = NixPkgSet
    <$> Toml.text "owner"  .= npsOwner
    <*> Toml.text "repo"   .= npsRepo
    <*> Toml.text "rev"    .= npsRev
    <*> Toml.text "sha256" .= npsSha

defaultNixPkgSet :: NixPkgSet
defaultNixPkgSet = NixPkgSet
    { npsOwner = "NixOS"
    , npsRepo  = "nixpkgs"
    , npsRev   = "cecec1f74468766825c2ad32d8388c2ded36225f"
    , npsSha   = "1sq538wy0shbakah27b6n4bl5amzwkzjsds77vdd8rsq0d1nys4w"
    }

showNixPkgSet :: NixPkgSet -> Text
showNixPkgSet NixPkgSet{..} = mconcat
    [ "https://github.com/"
    , npsOwner
    , "/"
    , npsRepo
    , "/archive/"
    , npsRev
    , ".tar.gz"
    , " (SHA256 is "
    , npsSha
    , ")"
    ]

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
    , settingsNix            :: !Bool
    , settingsNixPkgSet      :: !(Maybe NixPkgSet)
    , settingsStylish        :: !(Maybe Text) -- ^ @.stylish-haskell.yaml@ file
    , settingsContributing   :: !(Maybe Text) -- ^ @CONTRIBUTING.md@ file
    , settingsNoUpload       :: !Bool  -- ^ do not upload to GitHub
    } deriving (Show)
