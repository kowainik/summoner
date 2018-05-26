module Summoner.ProjectData
       ( ProjectData (..)
       , GhcVer (..)
       , supportedGhcVers
       , parseGhcVer
       , showGhcVer
       , latestLts
       , baseNopreludeVer

       , Decision (..)
       , CustomPrelude (..)
       ) where

import Generics.Deriving.Monoid (GMonoid (..))
import Generics.Deriving.Semigroup (GSemigroup (..))

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
    , base           :: Text -- ^ Base library to use
    , prelude        :: Maybe CustomPrelude  -- ^ custom prelude to be used
    , extensions     :: [Text] -- ^ default extensions
    } deriving (Show)

-- | Used for detecting the user decision during CLI input.
data Decision = Yes | Nop | Idk
    deriving (Show, Eq, Enum, Bounded, Generic)

instance Semigroup Decision where
    (<>) :: Decision -> Decision -> Decision
    Idk <> x   = x
    x   <> Idk = x
    _   <> x   = x

instance Monoid Decision where
    mempty  = Idk
    mappend = (<>)

instance GSemigroup Decision where
    gsappend = (<>)

instance GMonoid Decision where
    gmempty = mempty
    gmappend = (<>)

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

-- | Returns latest known LTS resolver for all GHC versions except default one.
latestLts :: GhcVer -> Text
latestLts Ghc7103 = "6.35"
latestLts Ghc801  = "7.24"
latestLts Ghc802  = "9.21"
latestLts Ghc822  = "11.10"

baseNopreludeVer :: GhcVer -> Text
baseNopreludeVer Ghc7103 = "4.8.0.2"
baseNopreludeVer Ghc801  = "4.9.0.0"
baseNopreludeVer Ghc802  = "4.9.1.0"
baseNopreludeVer Ghc822  = "4.10.1.0"

data CustomPrelude = Prelude
    { cpPackage :: Text
    , cpModule  :: Text
    } deriving (Show)
