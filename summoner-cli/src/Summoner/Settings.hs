{- |
Module                  : Summoner.Settings
Copyright               : (c) 2017-2021 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Complete settings required for the project creation.
-}

module Summoner.Settings
       ( Settings (..)

       , Tool (..)
       , showTool
       , parseTool
       ) where

import Relude.Extra.Enum (inverseMap)

import Summoner.CustomPrelude (CustomPrelude)
import Summoner.GhcVer (GhcVer)
import Summoner.License (License, LicenseName)
import Summoner.Tree (TreeFs)


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
    , settingsGhActions      :: !Bool   -- ^ GitHub Actions CI integration
    , settingsTravis         :: !Bool   -- ^ Travis CI integration
    , settingsAppVeyor       :: !Bool   -- ^ AppVeyor CI integration
    , settingsIsLib          :: !Bool   -- ^ is library
    , settingsIsExe          :: !Bool   -- ^ is executable
    , settingsTest           :: !Bool   -- ^ add tests
    , settingsBench          :: !Bool   -- ^ add benchmarks
    , settingsTestedVersions :: ![GhcVer]  -- ^ GHC versions
    , settingsPrelude        :: !(Maybe CustomPrelude)  -- ^ custom prelude to be used
    , settingsExtensions     :: ![Text] -- ^ default extensions
    , settingsGhcOptions     :: ![Text] -- ^ default GHC options
    , settingsGitignore      :: ![Text] -- ^ .gitignore file
    , settingsCabal          :: !Bool
    , settingsStack          :: !Bool
    , settingsNoUpload       :: !Bool  -- ^ do not upload to GitHub
    , settingsFiles          :: ![TreeFs]  -- ^ Tree nodes of extra files
    } deriving stock (Show)

-- | Enum for supported build tools.
data Tool
    = Cabal
    | Stack
    deriving stock (Show, Eq, Enum, Bounded)

-- | Show 'Tool' in lowercase.
showTool :: Tool -> Text
showTool = \case
    Cabal -> "cabal"
    Stack -> "stack"

-- | Parse 'Tool' from string. Inverse of 'showTool'.
parseTool :: Text -> Maybe Tool
parseTool = inverseMap showTool
