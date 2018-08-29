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

import Relude

import Control.Exception (throwIO)
import Data.List (lookup)
import Data.Monoid (Last (..))
import Generics.Deriving.Monoid (GMonoid, gmemptydefault)
import Generics.Deriving.Semigroup (GSemigroup, gsappenddefault)
import Toml (AnyValue (..), BiMap (..), BiToml, Bijection (..), Key, dimap, (.=))

import Summoner.Decision (Decision (..))
import Summoner.GhcVer (GhcVer (..), parseGhcVer, showGhcVer)
import Summoner.License (LicenseName (..), parseLicenseName)
import Summoner.ProjectData (CustomPrelude (..))
import Summoner.Source (Source, sourceT)
import Summoner.Validation (Validation (..))

import qualified Text.Show as Show
import qualified Toml

data Phase = Partial | Final

-- | Potentially incomplete configuration.
data ConfigP (p :: Phase) = Config
    { cOwner      :: p :- Text
    , cFullName   :: p :- Text
    , cEmail      :: p :- Text
    , cLicense    :: p :- LicenseName
    , cGhcVer     :: p :- [GhcVer]
    , cCabal      :: Decision
    , cStack      :: Decision
    , cGitHub     :: Decision
    , cTravis     :: Decision
    , cAppVey     :: Decision
    , cPrivate    :: Decision
    , cScript     :: Decision
    , cLib        :: Decision
    , cExe        :: Decision
    , cTest       :: Decision
    , cBench      :: Decision
    , cPrelude    :: Last CustomPrelude
    , cExtensions :: [Text]
    , cWarnings   :: [Text]
    , cStylish    :: Last Source
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
    { cOwner    = Last (Just "kowainik")
    , cFullName = Last (Just "Kowainik")
    , cEmail    = Last (Just "xrom.xkov@gmail.com")
    , cLicense  = Last (Just MIT)
    , cGhcVer   = Last (Just [])
    , cCabal    = Idk
    , cStack    = Idk
    , cGitHub   = Idk
    , cTravis   = Idk
    , cAppVey   = Idk
    , cPrivate  = Idk
    , cScript   = Idk
    , cLib      = Idk
    , cExe      = Idk
    , cTest     = Idk
    , cBench    = Idk
    , cPrelude  = Last Nothing
    , cExtensions = []
    , cWarnings = []
    , cStylish  = Last Nothing
    }

-- | Identifies how to read 'Config' data from the @.toml@ file.
configT :: BiToml PartialConfig
configT = Config
    <$> lastT Toml.text "owner"       .= cOwner
    <*> lastT Toml.text "fullName"    .= cFullName
    <*> lastT Toml.text "email"       .= cEmail
    <*> lastT license   "license"     .= cLicense
    <*> lastT ghcVerArr "ghcVersions" .= cGhcVer
    <*> decision        "cabal"       .= cCabal
    <*> decision        "stack"       .= cStack
    <*> decision        "github"      .= cGitHub
    <*> decision        "travis"      .= cTravis
    <*> decision        "appveyor"    .= cAppVey
    <*> decision        "private"     .= cPrivate
    <*> decision        "bscript"     .= cScript
    <*> decision        "lib"         .= cLib
    <*> decision        "exe"         .= cExe
    <*> decision        "test"        .= cTest
    <*> decision        "bench"       .= cBench
    <*> lastT (Toml.table preludeT) "prelude" .= cPrelude
    <*> textArr         "extensions"  .= cExtensions
    <*> textArr         "warnings"    .= cWarnings
    <*> wrapLastT (maybeSourceT "stylish") .= cStylish
  where
    wrapLastT :: BiToml (Maybe a) -> BiToml (Last a)
    wrapLastT = Toml.dimap getLast Last

    maybeSourceT :: Key -> BiToml (Maybe Source)
    maybeSourceT key = dimaybeT (sourceT key)

    dimaybeT :: BiToml a -> BiToml (Maybe a)
    dimaybeT bi = Bijection
        { biRead  = optional (biRead bi)
        , biWrite = \case
            Nothing -> pure Nothing
            Just v  -> Just v <$ biWrite bi v
        }

    lastT :: (Key -> BiToml a) -> Key -> BiToml (Last a)
    lastT = Toml.wrapper . Toml.maybeT

    _GhcVer :: BiMap AnyValue GhcVer
    _GhcVer = BiMap
        { forward = \(AnyValue t) -> Toml.matchText t >>= parseGhcVer
        , backward = Just . AnyValue . Toml.Text . showGhcVer
        }

    ghcVerArr :: Key -> BiToml [GhcVer]
    ghcVerArr = Toml.arrayOf _GhcVer

    license :: Key -> BiToml LicenseName
    license = Toml.mdimap show parseLicenseName . Toml.text

    textArr :: Key -> BiToml [Text]
    textArr = dimap Just maybeToMonoid . Toml.maybeT (Toml.arrayOf Toml._Text)

    decision :: Key -> BiToml Decision
    decision = dimap fromDecision toDecision . Toml.maybeT Toml.bool

    decisionMaybe :: [(Decision, Maybe Bool)]
    decisionMaybe = [ (Idk, Nothing)
                    , (Yes, Just True)
                    , (Nop, Just False)
                    ]

    fromDecision :: Decision -> Maybe Bool
    fromDecision d = join $ lookup d decisionMaybe

    toDecision :: Maybe Bool -> Decision
    toDecision m = fromMaybe (error "Impossible") $ lookup m $ map swap decisionMaybe

    preludeT :: BiToml CustomPrelude
    preludeT = Prelude
        <$> Toml.text "package" .= cpPackage
        <*> Toml.text "module"  .= cpModule

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
    <*> pure cTravis
    <*> pure cAppVey
    <*> pure cPrivate
    <*> pure cScript
    <*> pure cLib
    <*> pure cExe
    <*> pure cTest
    <*> pure cBench
    <*> pure cPrelude
    <*> pure cExtensions
    <*> pure cWarnings
    <*> pure cStylish
  where
    fin name = maybe (Failure ["Missing field: " <> name]) Success . getLast

-- | Read configuration from the given file and return it in data type.
loadFileConfig :: MonadIO m => FilePath -> m PartialConfig
loadFileConfig filePath = (Toml.decode configT <$> readFile filePath) >>= liftIO . errorWhenLeft
  where
    errorWhenLeft :: Either Toml.DecodeException PartialConfig -> IO PartialConfig
    errorWhenLeft (Left e)   = throwIO $ LoadTomlException filePath $ Toml.prettyException e
    errorWhenLeft (Right pc) = pure pc

data LoadTomlException = LoadTomlException FilePath Text

instance Show.Show LoadTomlException where
    show (LoadTomlException filePath msg) = "Couldnt parse file " ++ filePath ++ ": " ++ show msg

instance Exception LoadTomlException
