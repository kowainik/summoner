module Summoner.ProjectData
       ( ProjectData (..)
       , GhcVer (..)
       , supportedGhcVers
       , parseGhcVer
       , showGhcVer
       ) where

import Data.Text (Text)

-- | Data needed for project creation.
data ProjectData = ProjectData
    { repo           :: Text   -- ^ repository name
    , owner          :: Text   -- ^ github username
    , description    :: Text   -- ^ project description
    , nm             :: Text   -- ^ full name
    , email          :: Text   -- ^ e-mail
    , year           :: Text   -- ^ year
    , category       :: Text   -- ^ project category
    , license        :: Text   -- ^ type of license
    , licenseText    :: Text   -- ^ license text
    , github         :: Bool   -- ^ github repository
    , travis         :: Bool   -- ^ Travis CI integration
    , appVey         :: Bool   -- ^ AppVeyor CI integration
    , script         :: Bool   -- ^ build script
    , isLib          :: Bool   -- ^ is library
    , isExe          :: Bool   -- ^ is executable
    , test           :: Bool   -- ^ add tests
    , bench          :: Bool   -- ^ add benchmarks
    , testedVersions :: [GhcVer]  -- ^ ghc versions
    } deriving (Show)

-- | Represents some selected set of GHC versions.
data GhcVer = Ghc7103
            | Ghc801
            | Ghc802
            | Ghc822
            deriving (Eq, Ord, Show, Enum, Bounded)

-- | Supported by @summoner@ GHC versions for project templates.
supportedGhcVers :: [GhcVer]
supportedGhcVers = [minBound .. maxBound]

-- | Converts 'GhcVer' into dot-separated string.
showGhcVer :: GhcVer -> Text
showGhcVer Ghc7103 = "7.10.3"
showGhcVer Ghc801  = "8.0.1"
showGhcVer Ghc802  = "8.0.2"
showGhcVer Ghc822  = "8.2.2"

-- | Converts numeric dot-separated GHC version into 'GhcVer'.
parseGhcVer :: Text -> Maybe GhcVer
parseGhcVer "7.10.3" = Just Ghc7103
parseGhcVer "8.0.1"  = Just Ghc801
parseGhcVer "8.0.2"  = Just Ghc802
parseGhcVer "8.2.2"  = Just Ghc822
parseGhcVer _        = Nothing
