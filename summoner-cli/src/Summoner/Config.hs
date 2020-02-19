{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright: (c) 2017-2019 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Summoner configurations.
-}

module Summoner.Config
       ( ConfigP (..)

       , PartialConfig
       , Config
       , configCodec
       , defaultConfig
       , finalise

       , loadFileConfig
       ) where

import Data.List (lookup)
import Generic.Data (gmappend, gmempty)
import Relude.Extra.Validation (Validation (..))
import Toml (Key, TomlBiMap, TomlCodec, (.=))

import Summoner.CustomPrelude (CustomPrelude (..), customPreludeT)
import Summoner.Decision (Decision (..))
import Summoner.GhcVer (GhcVer (..), parseGhcVer, showGhcVer)
import Summoner.License (LicenseName (..), parseLicenseName)
import Summoner.Source (Source, sourceCodec, sourceT)

import qualified Toml


-- | The phase of the configurations.
data Phase
    = Partial
    | Final

{- | Type family to map 'Phase' to the corresponding field. This is a
Higher-Kinded Data approach specialised to custom enumeration.
-}
infixl 3 :-
type family phase :- field where
    'Partial :- field = Last field
    'Final   :- field = field

-- | Potentially incomplete configuration.
data ConfigP (p :: Phase) = Config
    { cOwner        :: !(p :- Text)
    , cFullName     :: !(p :- Text)
    , cEmail        :: !(p :- Text)
    , cLicense      :: !(p :- LicenseName)
    , cGhcVer       :: !(p :- [GhcVer])
    , cCabal        :: !Decision
    , cStack        :: !Decision
    , cGitHub       :: !Decision
    , cGhActions    :: !Decision
    , cTravis       :: !Decision
    , cAppVey       :: !Decision
    , cPrivate      :: !Decision
    , cLib          :: !Decision
    , cExe          :: !Decision
    , cTest         :: !Decision
    , cBench        :: !Decision
    , cPrelude      :: !(Last CustomPrelude)
    , cExtensions   :: ![Text]
    , cGhcOptions   :: ![Text]  -- ^ GHC options to add to each stanza
    , cGitignore    :: ![Text]
    , cStylish      :: !(Last Source)  -- ^ DEPRECATED: source to .stylish-haskell.yaml
    , cContributing :: !(Last Source)  -- ^ DEPRECATED: source to CONTRIBUTING.md
    , cNoUpload     :: !Any  -- ^ Do not upload to the GitHub (even if enabled)
    , cFiles        :: !(Map FilePath Source)  -- ^ Custom files
    } deriving stock (Generic)

deriving stock instance
    ( Eq (p :- Text)
    , Eq (p :- LicenseName)
    , Eq (p :- [GhcVer])
    ) => Eq (ConfigP p)

deriving stock instance
    ( Show (p :- Text)
    , Show (p :- LicenseName)
    , Show (p :- [GhcVer])
    ) => Show (ConfigP p)

instance Semigroup PartialConfig where
    (<>) = gmappend

instance Monoid PartialConfig where
    mempty  = gmempty
    mappend = (<>)

-- | Incomplete configurations.
type PartialConfig = ConfigP 'Partial

-- | Complete configurations.
type Config = ConfigP 'Final

-- | Default 'Config' configurations.
defaultConfig :: PartialConfig
defaultConfig = Config
    { cOwner        = Last (Just "kowainik")
    , cFullName     = Last (Just "Kowainik")
    , cEmail        = Last (Just "xrom.xkov@gmail.com")
    , cLicense      = Last (Just MIT)
    , cGhcVer       = Last (Just [])
    , cCabal        = Idk
    , cStack        = Idk
    , cGitHub       = Idk
    , cGhActions    = Idk
    , cTravis       = Idk
    , cAppVey       = Idk
    , cPrivate      = Idk
    , cLib          = Idk
    , cExe          = Idk
    , cTest         = Idk
    , cBench        = Idk
    , cPrelude      = Last Nothing
    , cExtensions   = []
    , cGhcOptions   = []
    , cGitignore    = []
    , cStylish      = Last Nothing
    , cContributing = Last Nothing
    , cNoUpload     = Any False
    , cFiles        = mempty
    }

-- | Identifies how to read 'Config' data from the @.toml@ file.
configCodec :: TomlCodec PartialConfig
configCodec = Config
    <$> Toml.last Toml.text "owner"         .= cOwner
    <*> Toml.last Toml.text "fullName"      .= cFullName
    <*> Toml.last Toml.text "email"         .= cEmail
    <*> Toml.last license   "license"       .= cLicense
    <*> Toml.last ghcVerArr "ghcVersions"   .= cGhcVer
    <*> decision            "cabal"         .= cCabal
    <*> decision            "stack"         .= cStack
    <*> decision            "github"        .= cGitHub
    <*> decision            "githubActions" .= cGhActions
    <*> decision            "travis"        .= cTravis
    <*> decision            "appveyor"      .= cAppVey
    <*> decision            "private"       .= cPrivate
    <*> decision            "lib"           .= cLib
    <*> decision            "exe"           .= cExe
    <*> decision            "test"          .= cTest
    <*> decision            "bench"         .= cBench
    <*> Toml.last preludeT  "prelude"       .= cPrelude
    <*> textArr             "extensions"    .= cExtensions
    <*> textArr             "ghc-options"   .= cGhcOptions
    <*> textArr             "gitignore"     .= cGitignore
    <*> Toml.last sourceT   "stylish"       .= cStylish
    <*> Toml.last sourceT   "contributing"  .= cContributing
    <*> Toml.any            "noUpload"      .= cNoUpload
    <*> filesCodec          "files"         .= cFiles
  where
    _GhcVer :: TomlBiMap GhcVer Toml.AnyValue
    _GhcVer = Toml._TextBy showGhcVer (maybeToRight "Wrong GHC version" . parseGhcVer)

    ghcVerArr :: Key -> TomlCodec [GhcVer]
    ghcVerArr = Toml.arrayOf _GhcVer

    license :: Key -> TomlCodec LicenseName
    license = Toml.textBy show (maybeToRight "Wrong license" . parseLicenseName)

    textArr :: Key -> TomlCodec [Text]
    textArr = Toml.dimap Just maybeToMonoid . Toml.dioptional . Toml.arrayOf Toml._Text

    decision :: Key -> TomlCodec Decision
    decision = Toml.dimap fromDecision toDecision . Toml.dioptional . Toml.bool

    decisionMaybe :: [(Decision, Maybe Bool)]
    decisionMaybe = [ (Idk, Nothing)
                    , (Yes, Just True)
                    , (Nop, Just False)
                    ]

    fromDecision :: Decision -> Maybe Bool
    fromDecision d = join $ lookup d decisionMaybe

    toDecision :: Maybe Bool -> Decision
    toDecision m = fromMaybe (error "Impossible") $ lookup m $ map swap decisionMaybe

    preludeT :: Key -> TomlCodec CustomPrelude
    preludeT = Toml.table customPreludeT

    filesCodec :: Key -> TomlCodec (Map FilePath Source)
    filesCodec = Toml.map (Toml.string "path") sourceCodec

-- | Make sure that all the required configurations options were specified.
finalise :: PartialConfig -> Validation [Text] Config
finalise Config{..} = Config
    <$> fin  "owner"      cOwner
    <*> fin  "fullName"   cFullName
    <*> fin  "email"      cEmail
    <*> fin  "license"    cLicense
    <*> fin  "ghcVersions" cGhcVer
    <*> pure cCabal
    <*> pure cStack
    <*> pure cGitHub
    <*> pure cGhActions
    <*> pure cTravis
    <*> pure cAppVey
    <*> pure cPrivate
    <*> pure cLib
    <*> pure cExe
    <*> pure cTest
    <*> pure cBench
    <*> pure cPrelude
    <*> pure cExtensions
    <*> pure cGhcOptions
    <*> pure cGitignore
    <*> pure cStylish
    <*> pure cContributing
    <*> pure cNoUpload
    <*> pure cFiles
  where
    fin :: Text -> Last a -> Validation [Text] a
    fin name = maybe (Failure ["Missing field: " <> name]) Success . getLast

-- | Read configuration from the given file and return it in data type.
loadFileConfig :: MonadIO m => FilePath -> m PartialConfig
loadFileConfig = Toml.decodeFile configCodec
