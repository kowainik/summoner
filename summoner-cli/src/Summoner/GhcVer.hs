{- | Contains data type for GHC versions supported by Summoner
and some useful functions for manipulation with them.
-}

module Summoner.GhcVer
       ( GhcVer (..)
       , Pvp (..)
       , showGhcVer
       , parseGhcVer
       , latestLts
       , baseVer
       , cabalBaseVersions
       , nixCompiler

       , oldGhcs
       ) where

import Data.List (maximum, minimum)

import qualified Text.Show as Show

-- | Represents some selected set of GHC versions.
data GhcVer
    = Ghc7103
    | Ghc801
    | Ghc802
    | Ghc822
    | Ghc843
    | Ghc844
    | Ghc863
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Converts 'GhcVer' into dot-separated string.
showGhcVer :: GhcVer -> Text
showGhcVer = \case
    Ghc7103 -> "7.10.3"
    Ghc801  -> "8.0.1"
    Ghc802  -> "8.0.2"
    Ghc822  -> "8.2.2"
    Ghc843  -> "8.4.3"
    Ghc844  -> "8.4.4"
    Ghc863  -> "8.6.3"

-- | These are old GHC versions that are not working with default GHC versions when using Stack.
oldGhcs :: [GhcVer]
oldGhcs = [minBound .. Ghc802]

parseGhcVer :: Text -> Maybe GhcVer
parseGhcVer = inverseMap showGhcVer

-- | Returns latest known LTS resolver for all GHC versions except default one.
latestLts :: GhcVer -> Text
latestLts = \case
    Ghc7103 -> "6.35"
    Ghc801  -> "7.24"
    Ghc802  -> "9.21"
    Ghc822  -> "11.22"
    Ghc843  -> "12.14"
    Ghc844  -> "12.25"
    Ghc863  -> "13.0"

-- | Represents PVP versioning (4 numbers).
data Pvp = Pvp
    { pvpFirst  :: Int
    , pvpSecond :: Int
    , pvpThird  :: Int
    , pvpFourth :: Int
    }

-- | Show PVP version in a standard way: @1.2.3.4@
instance Show Pvp where
    show (Pvp a b c d) = intercalate "." $ map Show.show [a, b, c, d]

-- | Returns base version by 'GhcVer' as 'Pvp'.
baseVerPvp :: GhcVer -> Pvp
baseVerPvp = \case
    Ghc7103 -> Pvp 4 8 0 2
    Ghc801  -> Pvp 4 9 0 0
    Ghc802  -> Pvp 4 9 1 0
    Ghc822  -> Pvp 4 10 1 0
    Ghc843  -> Pvp 4 11 1 0
    Ghc844  -> Pvp 4 11 1 0
    Ghc863  -> Pvp 4 12 0 0

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
cabalBaseVersions []   = ""
cabalBaseVersions [v]  = "^>= " <> baseVer v
cabalBaseVersions ghcs = ">= " <> baseVer (minimum ghcs) <> " && < " <> upperBound
  where
    upperBound :: Text
    upperBound = let Pvp{..} = baseVerPvp $ maximum ghcs in
        show pvpFirst <> "." <> show (pvpSecond + 1)

-- | Textual encoding of GHC compiler versions for use in nix.
nixCompiler :: GhcVer -> Text
nixCompiler = \case
    Ghc7103 -> "ghc7103"
    Ghc801  -> "ghc801"
    Ghc802  -> "ghc802"
    Ghc822  -> "ghc822"
    Ghc843  -> "ghc843"
    Ghc844  -> "ghc844"
    Ghc863  -> "ghc863"


