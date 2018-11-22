{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Summoner configurations.

module Summoner.Config
       ( ConfigP (..)

       , PartialConfig
       , Config
       , configT
       , defaultConfig
       , finalise

       , loadFileConfig
       ) where

import Data.List (lookup)
import Generics.Deriving.Monoid (GMonoid, gmemptydefault)
import Generics.Deriving.Semigroup (GSemigroup, gsappenddefault)
import Toml (BiMap (..), Key, TomlCodec, (.=))

import Summoner.Decision (Decision (..))
import Summoner.GhcVer (GhcVer (..), parseGhcVer, showGhcVer)
import Summoner.License (LicenseName (..), parseLicenseName)
import Summoner.Settings (CustomPrelude (..), customPreludeT)
import Summoner.Source (Source, sourceT)

import qualified Toml


-- | The phase of the configurations.
data Phase = Partial | Final

-- | Potentially incomplete configuration.
data ConfigP (p :: Phase) = Config
    { cOwner        :: !(p :- Text)
    , cFullName     :: !(p :- Text)
    , cEmail        :: !(p :- Text)
    , cLicense      :: !(p :- LicenseName)
    , cGhcVersions  :: !(p :- [GhcVer])
    , cCabal        :: !Decision
    , cStack        :: !Decision
    , cGitHub       :: !Decision
    , cPrivate      :: !Decision
    , cTravis       :: !Decision
    , cAppVeyor     :: !Decision
    , cLib          :: !Decision
    , cExe          :: !Decision
    , cTest         :: !Decision
    , cBench        :: !Decision
    , cPrelude      :: !(Last CustomPrelude)
    , cExtensions   :: ![Text]
    , cWarnings     :: ![Text]
    , cStylish      :: !(Last Source)
    , cContributing :: !(Last Source)
    } deriving (Generic)

deriving instance
    ( GSemigroup (p :- Text)
    , GSemigroup (p :- LicenseName)
    , GSemigroup (p :- [GhcVer])
    ) => GSemigroup (ConfigP p)
deriving instance
    ( GMonoid (p :- Text)
    , GMonoid (p :- LicenseName)
    , GMonoid (p :- [GhcVer])
    ) => GMonoid (ConfigP p)
deriving instance
    ( Eq (p :- Text)
    , Eq (p :- LicenseName)
    , Eq (p :- [GhcVer])
    ) => Eq (ConfigP p)
deriving instance
    ( Show (p :- Text)
    , Show (p :- LicenseName)
    , Show (p :- [GhcVer])
    ) => Show (ConfigP p)

infixl 3 :-
type family phase :- field where
    'Partial :- field = Last field
    'Final   :- field = field

-- | Incomplete configurations.
type PartialConfig = ConfigP 'Partial

-- | Complete configurations.
type Config = ConfigP 'Final

instance Semigroup PartialConfig where
    (<>) = gsappenddefault

instance Monoid PartialConfig where
    mempty = gmemptydefault
    mappend = (<>)

-- | Default 'Config' configurations.
defaultConfig :: PartialConfig
defaultConfig = Config
    { cOwner        = Last (Just "kowainik")
    , cFullName     = Last (Just "Kowainik")
    , cEmail        = Last (Just "xrom.xkov@gmail.com")
    , cLicense      = Last (Just MIT)
    , cGhcVersions  = Last (Just [])
    , cCabal        = Idk
    , cStack        = Idk
    , cGitHub       = Idk
    , cTravis       = Idk
    , cAppVeyor     = Idk
    , cPrivate      = Idk
    , cLib          = Idk
    , cExe          = Idk
    , cTest         = Idk
    , cBench        = Idk
    , cPrelude      = Last Nothing
    , cExtensions   = []
    , cWarnings     = []
    , cStylish      = Last Nothing
    , cContributing = Last Nothing
    }

-- | Identifies how to read 'Config' data from the @.toml@ file.
configT :: TomlCodec PartialConfig
configT = Config
    <$> lastT Toml.text "owner"       .= cOwner
    <*> lastT Toml.text "fullName"    .= cFullName
    <*> lastT Toml.text "email"       .= cEmail
    <*> lastT license   "license"     .= cLicense
    <*> lastT ghcVerArr "ghcVersions" .= cGhcVersions
    <*> decision        "cabal"       .= cCabal
    <*> decision        "stack"       .= cStack
    <*> decision        "github"      .= cGitHub
    <*> decision        "private"     .= cPrivate
    <*> decision        "travis"      .= cTravis
    <*> decision        "appveyor"    .= cAppVeyor
    <*> decision        "lib"         .= cLib
    <*> decision        "exe"         .= cExe
    <*> decision        "test"        .= cTest
    <*> decision        "bench"       .= cBench
    <*> lastT preludeT "prelude"      .= cPrelude
    <*> textArr        "extensions"   .= cExtensions
    <*> textArr        "warnings"     .= cWarnings
    <*> lastT sourceT  "stylish"      .= cStylish
    <*> lastT sourceT  "contributing" .= cContributing
  where
    lastT :: (Key -> TomlCodec a) -> Key -> TomlCodec (Last a)
    lastT codec = Toml.dimap getLast Last . Toml.dioptional . codec

    _GhcVer :: BiMap GhcVer Toml.AnyValue
    _GhcVer = Toml._TextBy showGhcVer parseGhcVer

    ghcVerArr :: Key -> TomlCodec [GhcVer]
    ghcVerArr = Toml.arrayOf _GhcVer

    license :: Key -> TomlCodec LicenseName
    license = Toml.mdimap show parseLicenseName . Toml.text

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


-- | Make sure that all the required configurations options were specified.
finalise :: PartialConfig -> Validation [Text] Config
finalise Config{..} = Config
    <$> fin  "owner"      cOwner
    <*> fin  "fullName"   cFullName
    <*> fin  "email"      cEmail
    <*> fin  "license"    cLicense
    <*> fin  "ghcVersions" cGhcVersions
    <*> pure cCabal
    <*> pure cStack
    <*> pure cGitHub
    <*> pure cPrivate
    <*> pure cTravis
    <*> pure cAppVeyor
    <*> pure cLib
    <*> pure cExe
    <*> pure cTest
    <*> pure cBench
    <*> pure cPrelude
    <*> pure cExtensions
    <*> pure cWarnings
    <*> pure cStylish
    <*> pure cContributing
  where
    fin name = maybe (Failure ["Missing field: " <> name]) Success . getLast

-- | Read configuration from the given file and return it in data type.
loadFileConfig :: MonadIO m => FilePath -> m PartialConfig
loadFileConfig = Toml.decodeFile configT
