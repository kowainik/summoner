{- |
Module                  : Summoner.GhcVer
Copyright               : (c) 2017-2024 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Contains data type for GHC versions supported by Summoner
and some useful functions for manipulation with them.
-}

module Summoner.GhcVer
    ( GhcVer (..)
    , GhcMeta (..)
    , Pvp (..)
    , showGhcVer
    , parseGhcVer
    , latestLts
    , baseVer
    , cabalBaseVersions
    , ghcTable

    , oldGhcs
    ) where

import qualified Data.Text as T
import qualified Text.Show as Show


-- | Represents some selected set of GHC versions.
data GhcVer
    = Ghc802
    | Ghc822
    | Ghc844
    | Ghc865
    | Ghc884
    | Ghc8107
    | Ghc902
    | Ghc928
    | Ghc948
    | Ghc967
    | Ghc984
    | Ghc9103
    | Ghc9123
    deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Converts 'GhcVer' into dot-separated string.
showGhcVer :: GhcVer -> Text
showGhcVer = \case
    Ghc802  -> "8.0.2"
    Ghc822  -> "8.2.2"
    Ghc844  -> "8.4.4"
    Ghc865  -> "8.6.5"
    Ghc884  -> "8.8.4"
    Ghc8107 -> "8.10.7"
    Ghc902  -> "9.0.2"
    Ghc928  -> "9.2.8"
    Ghc948  -> "9.4.8"
    Ghc967  -> "9.6.7"
    Ghc984  -> "9.8.4"
    Ghc9103 -> "9.10.3"
    Ghc9123 -> "9.12.3"

{- | These are old GHC versions that are not working with default GHC versions
when using Stack.
-}
oldGhcs :: [GhcVer]
oldGhcs = [minBound .. Ghc844]

parseGhcVer :: Text -> Maybe GhcVer
parseGhcVer = inverseMap showGhcVer

-- | Returns latest known LTS resolver for all GHC versions except default one.
latestLts :: GhcVer -> Text
latestLts = \case
    Ghc802  -> "lts-9.21"
    Ghc822  -> "lts-11.22"
    Ghc844  -> "lts-12.26"
    Ghc865  -> "lts-14.27"
    Ghc884  -> "lts-16.31"
    Ghc8107 -> "lts-18.28"
    Ghc902  -> "lts-19.33"
    Ghc928  -> "lts-20.26"
    Ghc948  -> "lts-21.25"
    Ghc967  -> "lts-22.44"
    Ghc984  -> "lts-23.28"
    Ghc9103 -> "lts-24.26"
    Ghc9123 -> "nightly-2026-01-04"

-- | Represents PVP versioning (4 numbers).
data Pvp = Pvp
    { pvpFirst  :: !Int
    , pvpSecond :: !Int
    , pvpThird  :: !Int
    , pvpFourth :: !Int
    }

-- | Show PVP version in a standard way: @1.2.3.4@
instance Show Pvp where
    show (Pvp a b c d) = intercalate "." $ map Show.show [a, b, c, d]

-- | Returns base version by 'GhcVer' as 'Pvp'.
baseVerPvp :: GhcVer -> Pvp
baseVerPvp = \case
    Ghc802  -> Pvp 4 9 1 0
    Ghc822  -> Pvp 4 10 1 0
    Ghc844  -> Pvp 4 11 1 0
    Ghc865  -> Pvp 4 12 0 0
    Ghc884  -> Pvp 4 13 0 0
    Ghc8107 -> Pvp 4 14 3 0
    Ghc902  -> Pvp 4 15 1 0
    Ghc928  -> Pvp 4 16 4 0
    Ghc948  -> Pvp 4 17 2 1
    Ghc967  -> Pvp 4 18 3 1
    Ghc984  -> Pvp 4 19 2 0
    Ghc9103 -> Pvp 4 20 2 0
    Ghc9123 -> Pvp 4 21 1 0

-- | Returns corresponding @base@ version of the given GHC version.
baseVer :: GhcVer -> Text
baseVer = show . baseVerPvp

{- | Returns the @base@ bounds for the list of the given GHC versions.

>>> cabalBaseVersions [Ghc844]
"^>= 4.11.1.0"

>>> cabalBaseVersions [Ghc802, Ghc822, Ghc844]
">= 4.9.0.0 && < 4.12"

-}
cabalBaseVersions :: [GhcVer] -> Text
cabalBaseVersions ghcVers = case sort ghcVers of
    []          -> ""
    [v]         -> "^>= " <> baseVer v
    minGhc:x:xs -> ">= " <> baseVer minGhc <> " && < " <> upperBound (x :| xs)
  where
    upperBound :: NonEmpty GhcVer -> Text
    upperBound ghcs = let Pvp{..} = baseVerPvp $ last ghcs in
        show pvpFirst <> "." <> show (pvpSecond + 1)

-- | Data type to keep meta information for every 'GhcVer'.
data GhcMeta = GhcMeta
    { gmGhc      :: !Text
    , gmBase     :: !Text
    , gmResolver :: !Text
    }

-- | Create corresponding 'GhcMeta' from the given 'GhcVer'.
toGhcMeta :: GhcVer -> GhcMeta
toGhcMeta ghcVer = GhcMeta
    { gmGhc      = "GHC-" <> showGhcVer ghcVer
    , gmBase     = "base-" <> baseVer ghcVer
    , gmResolver = latestLts ghcVer
    }

ghcTable :: [Text]
ghcTable = map (formatGhcMeta . toGhcMeta) universe

{- Formats 'GhcMeta' in a special way.
It aligns the meta to the left, filling on the right with the spaces.

As the pad number it takes the maximum possible length of the data manually.

Example:

@
GHC-8.6.5     base-4.12.0.0   lts-14.17
@
-}
formatGhcMeta :: GhcMeta -> Text
formatGhcMeta GhcMeta{..} =
       T.justifyLeft 12 ' ' gmGhc
    <> "  "
    <> T.justifyLeft 14 ' ' gmBase
    <> "  "
    <> gmResolver
