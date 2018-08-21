module Summoner.GhcVer
       ( GhcVer (..)
       , showGhcVer
       , parseGhcVer
       , latestLts
       , baseVer
       ) where

import Relude
import Relude.Extra.Enum (inverseMap)

-- | Represents some selected set of GHC versions.
data GhcVer
    = Ghc7103
    | Ghc801
    | Ghc802
    | Ghc822
    | Ghc843
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Converts 'GhcVer' into dot-separated string.
showGhcVer :: GhcVer -> Text
showGhcVer = \case
    Ghc7103 -> "7.10.3"
    Ghc801  -> "8.0.1"
    Ghc802  -> "8.0.2"
    Ghc822  -> "8.2.2"
    Ghc843  -> "8.4.3"

parseGhcVer :: Text -> Maybe GhcVer
parseGhcVer = inverseMap showGhcVer

-- | Returns latest known LTS resolver for all GHC versions except default one.
latestLts :: GhcVer -> Text
latestLts = \case
    Ghc7103 -> "6.35"
    Ghc801  -> "7.24"
    Ghc802  -> "9.21"
    Ghc822  -> "11.22"
    Ghc843  -> "12.7"

-- | Returns base version by 'GhcVer'.
baseVer :: GhcVer -> Text
baseVer = \case
    Ghc7103 -> "4.8.0.2"
    Ghc801  -> "4.9.0.0"
    Ghc802  -> "4.9.1.0"
    Ghc822  -> "4.10.1.0"
    Ghc843  -> "4.11.1.0"
