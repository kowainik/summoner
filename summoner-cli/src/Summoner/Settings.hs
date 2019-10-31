module Summoner.Settings
       ( Settings (..)

       , Tool (..)
       , showTool
       , parseTool
       ) where

import Summoner.CustomPrelude (CustomPrelude)
import Summoner.GhcVer (GhcVer)
import Summoner.License (License, LicenseName)


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
    , settingsGhcOptions     :: ![Text] -- ^ default GHC options
    , settingsGitignore      :: ![Text] -- ^ .gitignore file
    , settingsCabal          :: !Bool
    , settingsStack          :: !Bool
    , settingsStylish        :: !(Maybe Text) -- ^ @.stylish-haskell.yaml@ file
    , settingsContributing   :: !(Maybe Text) -- ^ @CONTRIBUTING.md@ file
    , settingsNoUpload       :: !Bool  -- ^ do not upload to GitHub
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
